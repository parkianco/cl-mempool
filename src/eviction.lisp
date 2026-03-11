;;;; src/eviction.lisp - Eviction Policies
;;;;
;;;; Transaction eviction and replacement policies

(in-package #:cl-mempool)

;;; Eviction Configuration

(defstruct (eviction-config (:constructor make-eviction-config)
                            (:copier nil))
  "Configuration for mempool eviction policies."
  ;; Size limits
  (max-pool-size 5000 :type (unsigned-byte 32))
  (max-per-account 64 :type (unsigned-byte 16))
  (max-pool-bytes (* 32 1024 1024) :type unsigned-byte)  ; 32 MB

  ;; Time limits
  (tx-lifetime 10800 :type (unsigned-byte 32))  ; 3 hours in seconds
  (eviction-interval 60 :type (unsigned-byte 16))  ; 1 minute

  ;; RBF settings
  (price-bump-percent 10 :type (unsigned-byte 8))  ; 10% minimum bump
  (rbf-enabled t :type boolean)

  ;; Priority settings
  (local-priority-bonus 1000 :type unsigned-byte)  ; Bonus for local txs
  (evict-remote-first t :type boolean))

;;; RBF (Replace-By-Fee) Operations

(defun rbf-allowed-p (new-tx old-tx config)
  "Check if NEW-TX can replace OLD-TX under RBF rules.
   Returns T if replacement is allowed."
  (and (eviction-config-rbf-enabled config)
       (bytes= (tx-sender new-tx) (tx-sender old-tx))
       (= (tx-nonce new-tx) (tx-nonce old-tx))
       ;; New tx must pay higher fee
       (let* ((old-max-fee (if (= (tx-type old-tx) +tx-type-legacy+)
                               (tx-gas-price old-tx)
                               (tx-max-fee-per-gas old-tx)))
              (new-max-fee (if (= (tx-type new-tx) +tx-type-legacy+)
                               (tx-gas-price new-tx)
                               (tx-max-fee-per-gas new-tx)))
              (bump-percent (eviction-config-price-bump-percent config))
              (min-required (ceiling (* old-max-fee (+ 100 bump-percent)) 100)))
         (>= new-max-fee min-required))))

(defun rbf-minimum-price (old-tx config)
  "Calculate minimum gas price required to replace OLD-TX."
  (let* ((old-max-fee (if (= (tx-type old-tx) +tx-type-legacy+)
                          (tx-gas-price old-tx)
                          (tx-max-fee-per-gas old-tx)))
         (bump-percent (eviction-config-price-bump-percent config)))
    (ceiling (* old-max-fee (+ 100 bump-percent)) 100)))

;;; CPFP (Child-Pays-For-Parent) Operations

(defun cpfp-eligible-p (parent-tx child-tx base-fee)
  "Check if CHILD-TX can boost PARENT-TX via CPFP.
   Child must have higher effective fee rate."
  (and (bytes= (tx-sender parent-tx) (tx-sender child-tx))
       (= (tx-nonce child-tx) (1+ (tx-nonce parent-tx)))
       (> (tx-fee-rate child-tx base-fee) (tx-fee-rate parent-tx base-fee))))

(defun cpfp-package-fee-rate (txs base-fee)
  "Calculate combined fee rate for a package of related transactions.
   Returns total-fee / total-size."
  (let ((total-fee 0)
        (total-size 0))
    (dolist (tx txs)
      (incf total-fee (* (tx-effective-gas-price tx base-fee) (tx-gas-limit tx)))
      (incf total-size (tx-size tx)))
    (safe-div total-fee total-size)))

;;; Eviction Selectors

(defun select-expired-txs (txs config)
  "Select transactions that have exceeded their lifetime."
  (let ((now (current-time-seconds))
        (lifetime (eviction-config-tx-lifetime config)))
    (remove-if-not (lambda (tx)
                     (> (- now (tx-timestamp tx)) lifetime))
                   txs)))

(defun select-lowest-priority-txs (txs base-fee count)
  "Select the COUNT lowest priority transactions."
  (let ((sorted (sort (copy-seq txs)
                      (lambda (a b) (tx-better-p b a base-fee)))))
    (subseq sorted 0 (min count (length sorted)))))

(defun select-remote-txs (txs)
  "Select all remote (non-local) transactions."
  (remove-if #'tx-local-p txs))

(defun select-excess-per-account (txs config)
  "Select transactions exceeding per-account limit.
   Returns list of txs to evict (keeps highest priority ones)."
  (let ((by-sender (make-hash-table :test 'equalp))
        (max-per-account (eviction-config-max-per-account config))
        (to-evict nil))
    ;; Group by sender
    (dolist (tx txs)
      (push tx (gethash (tx-sender tx) by-sender nil)))
    ;; Find accounts over limit
    (maphash (lambda (sender sender-txs)
               (declare (ignore sender))
               (when (> (length sender-txs) max-per-account)
                 ;; Sort by nonce (ascending) and keep only max-per-account
                 (let* ((sorted (sort sender-txs #'< :key #'tx-nonce))
                        (excess (nthcdr max-per-account sorted)))
                   (setf to-evict (nconc excess to-evict)))))
             by-sender)
    to-evict))

;;; Eviction Operations

(defun evict-expired (txs config)
  "Remove and return expired transactions."
  (let ((expired (select-expired-txs txs config)))
    (values (set-difference txs expired :test (lambda (a b) (bytes= (tx-hash a) (tx-hash b))))
            expired)))

(defun evict-to-size (txs base-fee max-count)
  "Evict lowest priority transactions until pool is at max-count.
   Returns (remaining-txs evicted-txs)."
  (if (<= (length txs) max-count)
      (values txs nil)
      (let* ((excess-count (- (length txs) max-count))
             (evicted (select-lowest-priority-txs txs base-fee excess-count)))
        (values (set-difference txs evicted
                                :test (lambda (a b) (bytes= (tx-hash a) (tx-hash b))))
                evicted))))

(defun evict-to-bytes (txs base-fee max-bytes)
  "Evict lowest priority transactions until pool is under max-bytes.
   Returns (remaining-txs evicted-txs)."
  (let ((total-bytes (reduce #'+ txs :key #'tx-size)))
    (if (<= total-bytes max-bytes)
        (values txs nil)
        (let* ((sorted (sort (copy-seq txs)
                             (lambda (a b) (tx-better-p b a base-fee))))
               (evicted nil)
               (remaining-bytes total-bytes))
          ;; Evict from lowest priority until under limit
          (loop for tx in sorted
                while (> remaining-bytes max-bytes)
                do (push tx evicted)
                   (decf remaining-bytes (tx-size tx)))
          (values (set-difference txs evicted
                                  :test (lambda (a b) (bytes= (tx-hash a) (tx-hash b))))
                  evicted)))))

(defun enforce-per-account-limits (txs config)
  "Enforce per-account transaction limits.
   Returns (remaining-txs evicted-txs)."
  (let ((evicted (select-excess-per-account txs config)))
    (values (set-difference txs evicted
                            :test (lambda (a b) (bytes= (tx-hash a) (tx-hash b))))
            evicted)))

;;; Underpriced Detection

(defun tx-underpriced-p (tx base-fee min-gas-price)
  "Check if TX is underpriced for current network conditions."
  (< (tx-effective-gas-price tx base-fee) min-gas-price))

(defun filter-underpriced (txs base-fee min-gas-price)
  "Remove all underpriced transactions.
   Returns (acceptable-txs underpriced-txs)."
  (let ((underpriced nil)
        (acceptable nil))
    (dolist (tx txs)
      (if (tx-underpriced-p tx base-fee min-gas-price)
          (push tx underpriced)
          (push tx acceptable)))
    (values acceptable underpriced)))
