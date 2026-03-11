;;;; test/test-mempool.lisp - Tests for cl-mempool
;;;;
;;;; Basic test suite for mempool functionality

(in-package #:cl-mempool.test)

;;; Test Infrastructure

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  "Define a test case."
  `(defun ,name ()
     (incf *test-count*)
     (handler-case
         (progn ,@body
                (incf *pass-count*)
                (format t "  PASS: ~A~%" ',name))
       (error (e)
         (incf *fail-count*)
         (format t "  FAIL: ~A - ~A~%" ',name e)))))

(defmacro assert-true (form)
  "Assert that FORM evaluates to true."
  `(unless ,form
     (error "Assertion failed: ~S" ',form)))

(defmacro assert-false (form)
  "Assert that FORM evaluates to false."
  `(when ,form
     (error "Assertion failed: expected false for ~S" ',form)))

(defmacro assert-equal (expected actual)
  "Assert that EXPECTED equals ACTUAL."
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equal exp act)
       (error "Assertion failed: expected ~S, got ~S" exp act))))

(defmacro assert-error (error-type &body body)
  "Assert that BODY signals an error of ERROR-TYPE."
  `(handler-case
       (progn ,@body
              (error "Expected ~A but no error was signaled" ',error-type))
     (,error-type () t)
     (error (e)
       (error "Expected ~A but got ~A" ',error-type (type-of e)))))

;;; Test Helpers

(defun make-test-sender (n)
  "Create a test sender address."
  (let ((addr (make-array 20 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref addr 0) n)
    addr))

(defun make-test-hash (n)
  "Create a test transaction hash."
  (let ((hash (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (setf (aref hash 0) n)
    hash))

(defun make-test-tx (&key (sender 1) (nonce 0) (gas-price 1000000000) (id 0))
  "Create a test transaction."
  (cl-mempool:make-tx :hash (make-test-hash id)
                      :sender (make-test-sender sender)
                      :nonce nonce
                      :gas-price gas-price
                      :gas-limit 21000
                      :type cl-mempool:+tx-type-legacy+))

;;; Transaction Tests

(deftest test-tx-creation
  (let ((tx (make-test-tx :sender 1 :nonce 5 :gas-price 2000000000 :id 1)))
    (assert-equal 5 (cl-mempool:tx-nonce tx))
    (assert-equal 2000000000 (cl-mempool:tx-gas-price tx))
    (assert-equal 21000 (cl-mempool:tx-gas-limit tx))))

(deftest test-tx-effective-gas-price-legacy
  (let ((tx (make-test-tx :gas-price 2000000000)))
    (assert-equal 2000000000 (cl-mempool:tx-effective-gas-price tx 1000000000))))

(deftest test-tx-effective-gas-price-eip1559
  (let ((tx (cl-mempool:make-tx :hash (make-test-hash 1)
                                :sender (make-test-sender 1)
                                :nonce 0
                                :max-fee-per-gas 3000000000
                                :max-priority-fee 1000000000
                                :gas-limit 21000
                                :type cl-mempool:+tx-type-dynamic-fee+)))
    ;; base-fee=1500000000, max-priority=1000000000
    ;; effective = min(3000000000, 1500000000 + 1000000000) = 2500000000
    (assert-equal 2500000000 (cl-mempool:tx-effective-gas-price tx 1500000000))))

(deftest test-tx-mining-score
  (let ((tx1 (make-test-tx :gas-price 1000000000 :id 1))
        (tx2 (make-test-tx :gas-price 2000000000 :id 2)))
    (assert-true (> (cl-mempool:tx-mining-score tx2 0)
                    (cl-mempool:tx-mining-score tx1 0)))))

;;; Pool Tests

(deftest test-pool-creation
  (let ((pool (cl-mempool:make-mempool)))
    (assert-equal 0 (cl-mempool:mempool-size pool))
    (assert-equal 0 (cl-mempool:mempool-pending-count pool))
    (assert-equal 0 (cl-mempool:mempool-queued-count pool))))

(deftest test-pool-add-single
  (let ((pool (cl-mempool:make-mempool))
        (tx (make-test-tx :sender 1 :nonce 0 :id 1)))
    (assert-true (cl-mempool:mempool-add pool tx))
    (assert-equal 1 (cl-mempool:mempool-size pool))
    (assert-equal 1 (cl-mempool:mempool-pending-count pool))))

(deftest test-pool-add-sequential
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 1 :id 2))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 2 :id 3))
    (assert-equal 3 (cl-mempool:mempool-pending-count pool))
    (assert-equal 3 (cl-mempool:mempool-sender-nonce pool (make-test-sender 1)))))

(deftest test-pool-add-with-gap
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 2 :id 2)) ; gap at 1
    (assert-equal 1 (cl-mempool:mempool-pending-count pool))
    (assert-equal 1 (cl-mempool:mempool-queued-count pool))))

(deftest test-pool-gap-filling
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 2 :id 2))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 1 :id 3)) ; fills gap
    (assert-equal 3 (cl-mempool:mempool-pending-count pool))
    (assert-equal 0 (cl-mempool:mempool-queued-count pool))))

(deftest test-pool-duplicate-rejection
  (let ((pool (cl-mempool:make-mempool))
        (tx (make-test-tx :sender 1 :nonce 0 :id 1)))
    (cl-mempool:mempool-add pool tx)
    (assert-error cl-mempool:mempool-duplicate-error
      (cl-mempool:mempool-add pool tx))))

(deftest test-pool-underpriced-rejection
  (let* ((config (cl-mempool:make-pool-config :min-gas-price 2000000000))
         (pool (cl-mempool:make-mempool :config config))
         (tx (make-test-tx :gas-price 1000000000 :id 1)))
    (assert-error cl-mempool:mempool-underpriced-error
      (cl-mempool:mempool-add pool tx))))

(deftest test-pool-nonce-too-low
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 1 :id 2))
    (assert-error cl-mempool:mempool-nonce-error
      (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 3)))))

(deftest test-pool-get-by-hash
  (let ((pool (cl-mempool:make-mempool))
        (tx (make-test-tx :sender 1 :nonce 0 :id 42)))
    (cl-mempool:mempool-add pool tx)
    (let ((found (cl-mempool:mempool-get pool (make-test-hash 42))))
      (assert-true found)
      (assert-equal 0 (cl-mempool:tx-nonce found)))))

(deftest test-pool-contains
  (let ((pool (cl-mempool:make-mempool))
        (tx (make-test-tx :sender 1 :nonce 0 :id 99)))
    (assert-false (cl-mempool:mempool-contains-p pool (make-test-hash 99)))
    (cl-mempool:mempool-add pool tx)
    (assert-true (cl-mempool:mempool-contains-p pool (make-test-hash 99)))))

(deftest test-pool-remove
  (let ((pool (cl-mempool:make-mempool))
        (tx (make-test-tx :sender 1 :nonce 0 :id 1)))
    (cl-mempool:mempool-add pool tx)
    (assert-equal 1 (cl-mempool:mempool-size pool))
    (cl-mempool:mempool-remove pool tx)
    (assert-equal 0 (cl-mempool:mempool-size pool))))

(deftest test-pool-clear
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 2 :nonce 0 :id 2))
    (cl-mempool:mempool-clear pool)
    (assert-equal 0 (cl-mempool:mempool-size pool))))

;;; Priority Tests

(deftest test-pool-top
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :gas-price 1000000000 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 2 :nonce 0 :gas-price 3000000000 :id 2))
    (cl-mempool:mempool-add pool (make-test-tx :sender 3 :nonce 0 :gas-price 2000000000 :id 3))
    (let ((top (cl-mempool:mempool-top pool)))
      (assert-equal 3000000000 (cl-mempool:tx-gas-price top)))))

(deftest test-pool-top-n
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :gas-price 1000000000 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 2 :nonce 0 :gas-price 3000000000 :id 2))
    (cl-mempool:mempool-add pool (make-test-tx :sender 3 :nonce 0 :gas-price 2000000000 :id 3))
    (let ((top-2 (cl-mempool:mempool-top-n pool 2)))
      (assert-equal 2 (length top-2))
      (assert-equal 3000000000 (cl-mempool:tx-gas-price (first top-2)))
      (assert-equal 2000000000 (cl-mempool:tx-gas-price (second top-2))))))

(deftest test-pool-pop
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :gas-price 1000000000 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 2 :nonce 0 :gas-price 2000000000 :id 2))
    (let ((popped (cl-mempool:mempool-pop pool)))
      (assert-equal 2000000000 (cl-mempool:tx-gas-price popped))
      (assert-equal 1 (cl-mempool:mempool-size pool)))))

;;; RBF Tests

(deftest test-rbf-replacement
  (let* ((evict-config (cl-mempool:make-eviction-config :price-bump-percent 10))
         (pool (cl-mempool:make-mempool :eviction-config evict-config)))
    ;; Add original tx
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :gas-price 1000000000 :id 1))
    ;; Replace with higher fee (12% bump > 10% required)
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :gas-price 1120000000 :id 2))
    (assert-equal 1 (cl-mempool:mempool-size pool))
    (let ((tx (cl-mempool:mempool-top pool)))
      (assert-equal 1120000000 (cl-mempool:tx-gas-price tx)))))

(deftest test-rbf-insufficient-bump
  (let* ((evict-config (cl-mempool:make-eviction-config :price-bump-percent 10))
         (pool (cl-mempool:make-mempool :eviction-config evict-config)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :gas-price 1000000000 :id 1))
    ;; Try to replace with insufficient bump (5% < 10% required)
    (assert-error cl-mempool:mempool-underpriced-error
      (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :gas-price 1050000000 :id 2)))))

;;; Account Tests

(deftest test-sender-txs
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 1 :id 2))
    (cl-mempool:mempool-add pool (make-test-tx :sender 2 :nonce 0 :id 3))
    (assert-equal 2 (length (cl-mempool:mempool-sender-txs pool (make-test-sender 1))))
    (assert-equal 1 (length (cl-mempool:mempool-sender-txs pool (make-test-sender 2))))))

(deftest test-sender-count
  (let ((pool (cl-mempool:make-mempool)))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 0 :id 1))
    (cl-mempool:mempool-add pool (make-test-tx :sender 1 :nonce 1 :id 2))
    (assert-equal 2 (cl-mempool:mempool-sender-count pool (make-test-sender 1)))
    (assert-equal 0 (cl-mempool:mempool-sender-count pool (make-test-sender 99)))))

;;; Utility Tests

(deftest test-bytes-to-hex
  (let ((bytes #(#xDE #xAD #xBE #xEF)))
    (assert-equal "DEADBEEF" (cl-mempool:bytes-to-hex bytes))))

(deftest test-hex-to-bytes
  (let ((bytes (cl-mempool:hex-to-bytes "DEADBEEF")))
    (assert-equal 4 (length bytes))
    (assert-equal #xDE (aref bytes 0))
    (assert-equal #xEF (aref bytes 3))))

;;; Test Runner

(defun run-tests ()
  "Run all tests and report results."
  (setf *test-count* 0
        *pass-count* 0
        *fail-count* 0)
  (format t "~%Running cl-mempool tests...~%~%")

  ;; Transaction tests
  (format t "Transaction tests:~%")
  (test-tx-creation)
  (test-tx-effective-gas-price-legacy)
  (test-tx-effective-gas-price-eip1559)
  (test-tx-mining-score)

  ;; Pool tests
  (format t "~%Pool tests:~%")
  (test-pool-creation)
  (test-pool-add-single)
  (test-pool-add-sequential)
  (test-pool-add-with-gap)
  (test-pool-gap-filling)
  (test-pool-duplicate-rejection)
  (test-pool-underpriced-rejection)
  (test-pool-nonce-too-low)
  (test-pool-get-by-hash)
  (test-pool-contains)
  (test-pool-remove)
  (test-pool-clear)

  ;; Priority tests
  (format t "~%Priority tests:~%")
  (test-pool-top)
  (test-pool-top-n)
  (test-pool-pop)

  ;; RBF tests
  (format t "~%RBF tests:~%")
  (test-rbf-replacement)
  (test-rbf-insufficient-bump)

  ;; Account tests
  (format t "~%Account tests:~%")
  (test-sender-txs)
  (test-sender-count)

  ;; Utility tests
  (format t "~%Utility tests:~%")
  (test-bytes-to-hex)
  (test-hex-to-bytes)

  ;; Summary
  (format t "~%================================~%")
  (format t "Tests: ~A  Passed: ~A  Failed: ~A~%"
          *test-count* *pass-count* *fail-count*)
  (if (zerop *fail-count*)
      (format t "All tests passed!~%")
      (format t "~A test(s) failed!~%" *fail-count*))
  (zerop *fail-count*))
