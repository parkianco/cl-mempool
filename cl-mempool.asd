;;;; cl-mempool.asd - ASDF System Definition
;;;;
;;;; Standalone transaction mempool management library
;;;; - Fee-based priority ordering
;;;; - Configurable eviction policies
;;;; - Replace-by-fee (RBF) support
;;;; - Child-Pays-For-Parent (CPFP) support
;;;; - Thread-safe operations

(asdf:defsystem #:cl-mempool
  :description "Standalone transaction mempool with fee-based priority and eviction policies"
  :author "Parkian Company LLC"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components
  ((:file "package")
   (:module "src"
    :serial t
    :components
    ((:file "util")
     (:file "transaction")
     (:file "priority")
     (:file "eviction")
     (:file "pool"))))
  :in-order-to ((test-op (test-op #:cl-mempool/test))))

(asdf:defsystem #:cl-mempool/test
  :description "Tests for cl-mempool"
  :depends-on (#:cl-mempool)
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "test-mempool"))))
  :perform (test-op (op c)
             (let ((result (uiop:symbol-call :cl-mempool.test :run-tests)))
               (unless result
                 (error "Tests failed")))))
