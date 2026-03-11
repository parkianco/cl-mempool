;;;; src/transaction.lisp - Transaction Structure
;;;;
;;;; Core transaction type for mempool management

(in-package #:cl-mempool)

;;; Transaction Type Constants

(defconstant +tx-type-legacy+ 0
  "Legacy transaction (pre-EIP-2718).")

(defconstant +tx-type-access-list+ 1
  "EIP-2930 access list transaction.")

(defconstant +tx-type-dynamic-fee+ 2
  "EIP-1559 dynamic fee transaction.")

(defconstant +tx-type-blob+ 3
  "EIP-4844 blob transaction.")

;;; Transaction Structure

(defstruct (tx (:constructor %make-tx)
               (:copier nil)
               (:predicate tx-p))
  "A pooled transaction in the mempool."
  ;; Identity
  (hash (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)
        :type (simple-array (unsigned-byte 8) (32))
        :read-only t)
  (sender (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (20))
          :read-only t)
  (nonce 0 :type (unsigned-byte 64) :read-only t)

  ;; Gas parameters
  (gas-limit 21000 :type (unsigned-byte 64) :read-only t)
  (gas-price 0 :type (unsigned-byte 64) :read-only t)           ; Legacy only
  (max-fee-per-gas 0 :type (unsigned-byte 64) :read-only t)     ; EIP-1559
  (max-priority-fee 0 :type (unsigned-byte 64) :read-only t)    ; EIP-1559

  ;; Value and data
  (value 0 :type unsigned-byte :read-only t)
  (data #() :type (simple-array (unsigned-byte 8) (*)) :read-only t)

  ;; Transaction type
  (type +tx-type-legacy+ :type (unsigned-byte 8) :read-only t)

  ;; Blob parameters (EIP-4844)
  (blob-gas 0 :type (unsigned-byte 64) :read-only t)
  (max-fee-per-blob-gas 0 :type (unsigned-byte 64) :read-only t)

  ;; Mempool metadata (mutable)
  (timestamp (current-time-seconds) :type (unsigned-byte 64))
  (size 0 :type (unsigned-byte 32))                             ; Serialized size
  (local-p nil :type boolean)                                   ; Local vs remote

  ;; Cached computations
  (effective-gas-price-cache nil :type (or null unsigned-byte))
  (mining-score-cache nil :type (or null rational)))

;;; Transaction Constructor

(defun make-tx (&key hash sender nonce
                     (gas-limit 21000)
                     (gas-price 0)
                     (max-fee-per-gas 0)
                     (max-priority-fee 0)
                     (value 0)
                     (data nil)
                     (type +tx-type-legacy+)
                     (blob-gas 0)
                     (max-fee-per-blob-gas 0)
                     (timestamp (current-time-seconds))
                     (size 0)
                     (local-p nil))
  "Create a new transaction for the mempool."
  (let ((tx-hash (or hash (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
        (tx-sender (or sender (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
        (tx-data (cond
                   ((null data) (make-array 0 :element-type '(unsigned-byte 8)))
                   ((typep data '(simple-array (unsigned-byte 8) (*))) data)
                   (t (coerce data '(simple-array (unsigned-byte 8) (*)))))))
    (%make-tx :hash tx-hash
              :sender tx-sender
              :nonce nonce
              :gas-limit gas-limit
              :gas-price gas-price
              :max-fee-per-gas max-fee-per-gas
              :max-priority-fee max-priority-fee
              :value value
              :data tx-data
              :type type
              :blob-gas blob-gas
              :max-fee-per-blob-gas max-fee-per-blob-gas
              :timestamp timestamp
              :size (if (zerop size) (+ 100 (length tx-data)) size)
              :local-p local-p)))

;;; Computed Properties

(defun tx-effective-gas-price (tx base-fee)
  "Compute effective gas price for TX given current BASE-FEE.
   For legacy transactions: gas-price
   For EIP-1559: min(max-fee-per-gas, base-fee + max-priority-fee)"
  (if (= (tx-type tx) +tx-type-legacy+)
      (tx-gas-price tx)
      (min (tx-max-fee-per-gas tx)
           (+ base-fee (tx-max-priority-fee tx)))))

(defun tx-effective-tip (tx base-fee)
  "Compute effective tip (priority fee) for TX given current BASE-FEE.
   For legacy transactions: gas-price - base-fee
   For EIP-1559: min(max-priority-fee, max-fee-per-gas - base-fee)"
  (if (= (tx-type tx) +tx-type-legacy+)
      (max 0 (- (tx-gas-price tx) base-fee))
      (min (tx-max-priority-fee tx)
           (max 0 (- (tx-max-fee-per-gas tx) base-fee)))))

(defun tx-max-cost (tx)
  "Compute maximum cost of TX (value + max gas cost + max blob cost)."
  (let ((max-gas-cost (* (tx-gas-limit tx)
                         (if (= (tx-type tx) +tx-type-legacy+)
                             (tx-gas-price tx)
                             (tx-max-fee-per-gas tx))))
        (max-blob-cost (* (tx-blob-gas tx) (tx-max-fee-per-blob-gas tx))))
    (+ (tx-value tx) max-gas-cost max-blob-cost)))

(defun tx-fee-rate (tx base-fee)
  "Compute fee rate (fee per byte) for TX."
  (let ((effective-gas-price (tx-effective-gas-price tx base-fee))
        (gas-limit (tx-gas-limit tx))
        (size (max 1 (tx-size tx))))
    (safe-div (* effective-gas-price gas-limit) size)))

(defun tx-mining-score (tx base-fee)
  "Compute mining score for TX.
   Higher score = more profitable to include.
   Score = effective-tip * gas-limit / size"
  (let ((effective-tip (tx-effective-tip tx base-fee))
        (gas-limit (tx-gas-limit tx))
        (size (max 1 (tx-size tx))))
    (safe-div (* effective-tip gas-limit) size)))

;;; Transaction Comparison

(defun tx-hash= (tx1 tx2)
  "Check if two transactions have the same hash."
  (bytes= (tx-hash tx1) (tx-hash tx2)))

(defun tx-sender= (tx1 tx2)
  "Check if two transactions have the same sender."
  (bytes= (tx-sender tx1) (tx-sender tx2)))

(defun tx-better-p (tx1 tx2 base-fee)
  "Return T if TX1 has higher priority than TX2."
  (> (tx-mining-score tx1 base-fee) (tx-mining-score tx2 base-fee)))

(defun tx-replaces-p (new-tx old-tx price-bump-percent)
  "Check if NEW-TX can replace OLD-TX under RBF rules.
   NEW-TX must have same sender/nonce and higher effective fee."
  (and (bytes= (tx-sender new-tx) (tx-sender old-tx))
       (= (tx-nonce new-tx) (tx-nonce old-tx))
       (let* ((old-fee (if (= (tx-type old-tx) +tx-type-legacy+)
                           (tx-gas-price old-tx)
                           (tx-max-fee-per-gas old-tx)))
              (new-fee (if (= (tx-type new-tx) +tx-type-legacy+)
                           (tx-gas-price new-tx)
                           (tx-max-fee-per-gas new-tx)))
              (min-required (ceiling (* old-fee (+ 100 price-bump-percent)) 100)))
         (>= new-fee min-required))))
