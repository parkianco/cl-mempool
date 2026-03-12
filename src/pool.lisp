;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; src/pool.lisp - Main Mempool Implementation
;;;;
;;;; Thread-safe transaction mempool with priority ordering and eviction

(in-package #:cl-mempool)

;;; Error Conditions

(define-condition mempool-error (error)
  ((message :initarg :message :reader mempool-error-message))
  (:report (lambda (c s)
             (format s "Mempool error: ~A" (mempool-error-message c)))))

(define-condition mempool-full-error (mempool-error)
  ()
  (:default-initargs :message "Transaction pool is full"))

(define-condition mempool-underpriced-error (mempool-error)
  ((required-price :initarg :required :reader required-price))
  (:report (lambda (c s)
             (format s "Transaction underpriced. Required: ~A" (required-price c)))))

(define-condition mempool-nonce-error (mempool-error)
  ((expected :initarg :expected :reader expected-nonce)
   (got :initarg :got :reader actual-nonce))
  (:report (lambda (c s)
             (format s "Invalid nonce. Expected: ~A, Got: ~A"
                     (expected-nonce c) (actual-nonce c)))))

(define-condition mempool-duplicate-error (mempool-error)
  ()
  (:default-initargs :message "Duplicate transaction"))

(define-condition mempool-invalid-error (mempool-error)
  ()
  (:default-initargs :message "Invalid transaction"))

;;; Pool Configuration

(defstruct (pool-config (:constructor make-pool-config)
                        (:copier nil))
  "Configuration for the mempool."
  ;; Size limits
  (max-pool-size 5000 :type (unsigned-byte 32))
  (max-per-account 64 :type (unsigned-byte 16))
  (max-pool-bytes (* 32 1024 1024) :type unsigned-byte)

  ;; Fee settings
  (min-gas-price 1000000000 :type unsigned-byte)  ; 1 Gwei
  (price-bump-percent 10 :type (unsigned-byte 8))

  ;; Time settings
  (tx-lifetime 10800 :type (unsigned-byte 32))  ; 3 hours
  (eviction-interval 60 :type (unsigned-byte 16))

  ;; Slot configuration
  (local-slots 1000 :type (unsigned-byte 32))
  (global-slots 4000 :type (unsigned-byte 32))
  (global-queue 1024 :type (unsigned-byte 32)))

;;; Account State

(defstruct (account-state (:constructor %make-account-state)
                          (:copier nil))
  "State for a single account in the mempool."
  (address (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)
           :type (simple-array (unsigned-byte 8) (20)))
  (pending-nonce 0 :type (unsigned-byte 64))    ; Next expected nonce
  (pending-txs nil :type list)                  ; Executable txs (sorted by nonce)
  (queued-txs nil :type list))                  ; Non-executable txs (nonce gaps)

(defun make-account-state (address)
  "Create a new account state."
  (%make-account-state :address address))

;;; Mempool Structure

(defstruct (mempool (:constructor %make-mempool)
                    (:copier nil))
  "Thread-safe transaction mempool."
  ;; Configuration
  (config (make-pool-config) :type pool-config)
  (eviction-config (make-eviction-config) :type eviction-config)

  ;; State
  (base-fee 0 :type unsigned-byte)
  (priority-heap nil :type (or null priority-heap))

  ;; Transaction storage
  (tx-by-hash (make-hash-table :test 'equalp) :type hash-table)
  (accounts (make-hash-table :test 'equalp) :type hash-table)

  ;; Counts
  (pending-count 0 :type (unsigned-byte 32))
  (queued-count 0 :type (unsigned-byte 32))
  (total-bytes 0 :type unsigned-byte)

  ;; Synchronization
  (lock (make-lock "mempool-lock")))

(defun make-mempool (&key (config (make-pool-config))
                          (eviction-config (make-eviction-config))
                          (base-fee 0))
  "Create a new mempool instance."
  (%make-mempool
   :config config
   :eviction-config eviction-config
   :base-fee base-fee
   :priority-heap (make-priority-heap :base-fee base-fee)))

;;; Internal Helpers

(defun %get-account (pool sender)
  "Get or create account state for SENDER."
  (or (gethash sender (mempool-accounts pool))
      (setf (gethash sender (mempool-accounts pool))
            (make-account-state sender))))

(defun %add-to-priority-heap (pool tx)
  "Add TX to the priority heap."
  (heap-push (mempool-priority-heap pool) tx))

(defun %remove-from-priority-heap (pool tx)
  "Remove TX from the priority heap."
  (heap-remove (mempool-priority-heap pool) tx))

(defun %promote-queued (pool account)
  "Move queued transactions to pending when they become executable."
  (let ((pending-nonce (account-state-pending-nonce account)))
    (loop for tx in (sort (copy-list (account-state-queued-txs account)) #'< :key #'tx-nonce)
          while (= (tx-nonce tx) pending-nonce)
          do (setf (account-state-queued-txs account)
                   (remove tx (account-state-queued-txs account) :test #'eq))
             (push tx (account-state-pending-txs account))
             (%add-to-priority-heap pool tx)
             (incf (mempool-pending-count pool))
             (decf (mempool-queued-count pool))
             (incf pending-nonce))
    (setf (account-state-pending-nonce account) pending-nonce)))

;;; Public API

(defun mempool-add (pool tx)
  "Add TX to the mempool. Returns T on success.
   Signals an error if the transaction is invalid or cannot be added."
  (with-lock ((mempool-lock pool))
    (let* ((config (mempool-config pool))
           (evict-config (mempool-eviction-config pool))
           (base-fee (mempool-base-fee pool))
           (hash (tx-hash tx))
           (sender (tx-sender tx)))

      ;; Check for duplicate
      (when (gethash hash (mempool-tx-by-hash pool))
        (error 'mempool-duplicate-error))

      ;; Check if underpriced
      (when (tx-underpriced-p tx base-fee (pool-config-min-gas-price config))
        (error 'mempool-underpriced-error
               :required (pool-config-min-gas-price config)))

      ;; Check pool size limit
      (when (>= (+ (mempool-pending-count pool) (mempool-queued-count pool))
                (pool-config-max-pool-size config))
        ;; Try to evict before failing
        (let ((evictable (heap-peek (mempool-priority-heap pool))))
          (when (or (null evictable)
                    (tx-better-p evictable tx base-fee))
            (error 'mempool-full-error))))

      ;; Get or create account state
      (let* ((account (%get-account pool sender))
             (pending-nonce (account-state-pending-nonce account)))

        ;; Check for past nonce, but allow RBF replacement of the
        ;; highest pending nonce (pending-nonce - 1)
        (when (< (tx-nonce tx) pending-nonce)
          ;; Check if this is an RBF candidate (same sender, same nonce,
          ;; and it's the highest pending nonce)
          (let ((existing (when (= (tx-nonce tx) (1- pending-nonce))
                           (find-if (lambda (ptx)
                                      (and (bytes= (tx-sender ptx) sender)
                                           (= (tx-nonce ptx) (tx-nonce tx))))
                                    (account-state-pending-txs account)))))
            (if existing
                (if (rbf-allowed-p tx existing evict-config)
                    (progn
                      ;; Replace existing transaction: reset pending-nonce so
                      ;; the replacement can be re-added at the same nonce slot
                      (%mempool-remove-unlocked pool existing)
                      (setf (account-state-pending-nonce account) (tx-nonce tx)))
                    (error 'mempool-underpriced-error
                           :required (rbf-minimum-price existing evict-config)))
                (error 'mempool-nonce-error
                       :expected pending-nonce
                       :got (tx-nonce tx)))))

        ;; Add to appropriate queue (use live pending-nonce, may have changed via RBF)
        (let ((current-pending-nonce (account-state-pending-nonce account)))
          (cond
            ;; Executable: nonce matches expected
            ((= (tx-nonce tx) current-pending-nonce)
             (push tx (account-state-pending-txs account))
             (%add-to-priority-heap pool tx)
             (incf (mempool-pending-count pool))
             (incf (account-state-pending-nonce account))
             ;; Try to promote queued txs
             (%promote-queued pool account))

            ;; Future nonce: queue it
            ((> (tx-nonce tx) current-pending-nonce)
             (push tx (account-state-queued-txs account))
             (incf (mempool-queued-count pool)))))

        ;; Update storage
        (setf (gethash hash (mempool-tx-by-hash pool)) tx)
        (incf (mempool-total-bytes pool) (tx-size tx)))

      t)))

(defun %mempool-remove-unlocked (pool tx)
  "Internal: Remove TX from the mempool without acquiring lock.
   Must be called while holding the mempool lock."
  (let* ((hash (tx-hash tx))
         (existing (gethash hash (mempool-tx-by-hash pool))))
    (when existing
      (let* ((sender (tx-sender tx))
             (account (gethash sender (mempool-accounts pool))))
        (when account
          ;; Remove from pending or queued
          (cond
            ((member tx (account-state-pending-txs account) :test #'eq)
             (setf (account-state-pending-txs account)
                   (remove tx (account-state-pending-txs account) :test #'eq))
             (%remove-from-priority-heap pool tx)
             (decf (mempool-pending-count pool)))
            ((member tx (account-state-queued-txs account) :test #'eq)
             (setf (account-state-queued-txs account)
                   (remove tx (account-state-queued-txs account) :test #'eq))
             (decf (mempool-queued-count pool))))))
      ;; Remove from storage
      (remhash hash (mempool-tx-by-hash pool))
      (decf (mempool-total-bytes pool) (tx-size tx))
      t)))

(defun mempool-remove (pool tx)
  "Remove TX from the mempool. Returns T if found and removed."
  (with-lock ((mempool-lock pool))
    (%mempool-remove-unlocked pool tx)))

(defun mempool-get (pool hash)
  "Get a transaction by its hash."
  (with-lock ((mempool-lock pool))
    (gethash hash (mempool-tx-by-hash pool))))

(defun mempool-contains-p (pool hash)
  "Check if a transaction with HASH exists in the mempool."
  (with-lock ((mempool-lock pool))
    (not (null (gethash hash (mempool-tx-by-hash pool))))))

(defun mempool-size (pool)
  "Return total number of transactions in the mempool."
  (+ (mempool-pending-count pool) (mempool-queued-count pool)))

(defun mempool-clear (pool)
  "Remove all transactions from the mempool."
  (with-lock ((mempool-lock pool))
    (clrhash (mempool-tx-by-hash pool))
    (clrhash (mempool-accounts pool))
    (heap-clear (mempool-priority-heap pool))
    (setf (mempool-pending-count pool) 0
          (mempool-queued-count pool) 0
          (mempool-total-bytes pool) 0)))

;;; Priority Operations

(defun mempool-top (pool)
  "Return the highest priority pending transaction."
  (with-lock ((mempool-lock pool))
    (heap-peek (mempool-priority-heap pool))))

(defun mempool-top-n (pool n)
  "Return the top N highest priority pending transactions."
  (with-lock ((mempool-lock pool))
    (heap-top-n (mempool-priority-heap pool) n)))

(defun mempool-pop (pool)
  "Remove and return the highest priority pending transaction."
  (with-lock ((mempool-lock pool))
    (let ((tx (heap-pop (mempool-priority-heap pool))))
      (when tx
        ;; Clean up account state and storage without touching the heap
        ;; (heap-pop already removed it from the heap)
        (let* ((sender (tx-sender tx))
               (account (gethash sender (mempool-accounts pool))))
          (when account
            (when (member tx (account-state-pending-txs account) :test #'eq)
              (setf (account-state-pending-txs account)
                    (remove tx (account-state-pending-txs account) :test #'eq))
              (decf (mempool-pending-count pool)))))
        (remhash (tx-hash tx) (mempool-tx-by-hash pool))
        (decf (mempool-total-bytes pool) (tx-size tx))
        tx))))

(defun mempool-get-executable (pool &key (limit 1000))
  "Get executable transactions for block building, up to LIMIT."
  (with-lock ((mempool-lock pool))
    (heap-top-n (mempool-priority-heap pool) limit)))

;;; Account Operations

(defun mempool-sender-txs (pool sender)
  "Get all transactions from SENDER."
  (with-lock ((mempool-lock pool))
    (let ((account (gethash sender (mempool-accounts pool))))
      (when account
        (append (copy-list (account-state-pending-txs account))
                (copy-list (account-state-queued-txs account)))))))

(defun mempool-sender-nonce (pool sender)
  "Get the pending nonce for SENDER."
  (with-lock ((mempool-lock pool))
    (let ((account (gethash sender (mempool-accounts pool))))
      (if account
          (account-state-pending-nonce account)
          0))))

(defun mempool-sender-count (pool sender)
  "Get the number of transactions from SENDER."
  (with-lock ((mempool-lock pool))
    (let ((account (gethash sender (mempool-accounts pool))))
      (if account
          (+ (length (account-state-pending-txs account))
             (length (account-state-queued-txs account)))
          0))))

(defun mempool-peek-by-sender (pool sender)
  "Get the next executable transaction from SENDER."
  (with-lock ((mempool-lock pool))
    (let ((account (gethash sender (mempool-accounts pool))))
      (when (and account (account-state-pending-txs account))
        (first (sort (copy-list (account-state-pending-txs account))
                     #'< :key #'tx-nonce))))))

;;; Eviction Operations

(defun mempool-evict-expired (pool)
  "Evict all expired transactions. Returns list of evicted txs."
  (with-lock ((mempool-lock pool))
    (let ((all-txs (loop for tx being the hash-values of (mempool-tx-by-hash pool)
                         collect tx))
          (evicted nil))
      (multiple-value-bind (remaining expired)
          (evict-expired all-txs (mempool-eviction-config pool))
        (declare (ignore remaining))
        (dolist (tx expired)
          (mempool-remove pool tx)
          (push tx evicted)))
      evicted)))

(defun mempool-evict-lowest (pool count)
  "Evict COUNT lowest priority transactions. Returns list of evicted txs."
  (with-lock ((mempool-lock pool))
    (let ((evicted nil))
      (dotimes (i count)
        (let ((tx (heap-pop (mempool-priority-heap pool))))
          (when tx
            (mempool-remove pool tx)
            (push tx evicted))))
      evicted)))

(defun mempool-evict-by-size (pool target-size)
  "Evict transactions until pool size is at most TARGET-SIZE."
  (with-lock ((mempool-lock pool))
    (let ((evicted nil))
      (loop while (> (mempool-size pool) target-size)
            for tx = (heap-pop (mempool-priority-heap pool))
            while tx
            do (mempool-remove pool tx)
               (push tx evicted))
      evicted)))

(defun mempool-enforce-limits (pool)
  "Enforce all configured limits. Returns list of evicted txs."
  (with-lock ((mempool-lock pool))
    (let ((config (mempool-config pool))
          (evicted nil))
      ;; Evict expired first
      (setf evicted (nconc (mempool-evict-expired pool) evicted))
      ;; Evict by count
      (when (> (mempool-size pool) (pool-config-max-pool-size config))
        (let ((excess (- (mempool-size pool) (pool-config-max-pool-size config))))
          (setf evicted (nconc (mempool-evict-lowest pool excess) evicted))))
      evicted)))

;;; Base Fee Updates

(defun mempool-set-base-fee (pool new-base-fee)
  "Update the base fee. This affects priority ordering."
  (with-lock ((mempool-lock pool))
    (setf (mempool-base-fee pool) new-base-fee)
    (heap-update-base-fee (mempool-priority-heap pool) new-base-fee)))
