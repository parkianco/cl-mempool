;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; package.lisp - cl-mempool Package Definition
;;;;
;;;; Standalone transaction mempool management

(in-package #:cl-user)

(defpackage #:cl-mempool
  (:use #:cl)
  (:documentation "Standalone transaction mempool with fee-based priority and eviction policies.")
  (:export
   ;; Transaction Types
   #:+tx-type-legacy+
   #:+tx-type-access-list+
   #:+tx-type-dynamic-fee+
   #:+tx-type-blob+

   ;; Transaction Structure
   #:tx
   #:make-tx
   #:tx-hash
   #:tx-sender
   #:tx-nonce
   #:tx-gas-limit
   #:tx-gas-price
   #:tx-max-fee-per-gas
   #:tx-max-priority-fee
   #:tx-value
   #:tx-data
   #:tx-type
   #:tx-timestamp
   #:tx-size
   #:tx-local-p
   #:tx-blob-gas
   #:tx-max-fee-per-blob-gas

   ;; Computed Transaction Properties
   #:tx-effective-gas-price
   #:tx-effective-tip
   #:tx-max-cost
   #:tx-fee-rate
   #:tx-mining-score

   ;; Pool Configuration
   #:pool-config
   #:make-pool-config
   #:config-max-pool-size
   #:config-max-per-account
   #:config-min-gas-price
   #:config-price-bump-percent
   #:config-eviction-interval
   #:config-tx-lifetime
   #:config-local-slots
   #:config-global-slots
   #:config-global-queue

   ;; Eviction Configuration
   #:eviction-config
   #:make-eviction-config
   #:eviction-config-max-pool-size
   #:eviction-config-max-per-account
   #:eviction-config-max-pool-bytes
   #:eviction-config-tx-lifetime
   #:eviction-config-eviction-interval
   #:eviction-config-price-bump-percent
   #:eviction-config-rbf-enabled
   #:eviction-config-local-priority-bonus
   #:eviction-config-evict-remote-first

   ;; Pool Operations
   #:mempool
   #:make-mempool
   #:mempool-add
   #:mempool-remove
   #:mempool-get
   #:mempool-contains-p
   #:mempool-size
   #:mempool-pending-count
   #:mempool-queued-count
   #:mempool-clear

   ;; Priority Operations
   #:mempool-top
   #:mempool-top-n
   #:mempool-pop
   #:mempool-peek-by-sender
   #:mempool-get-executable

   ;; Account Operations
   #:mempool-sender-txs
   #:mempool-sender-nonce
   #:mempool-sender-count

   ;; Eviction
   #:mempool-evict-expired
   #:mempool-evict-lowest
   #:mempool-evict-by-size
   #:mempool-enforce-limits

   ;; RBF/CPFP
   #:rbf-allowed-p
   #:rbf-minimum-price
   #:cpfp-eligible-p
   #:cpfp-package-fee-rate

   ;; Conditions
   #:mempool-error
   #:mempool-full-error
   #:mempool-underpriced-error
   #:mempool-nonce-error
   #:mempool-duplicate-error
   #:mempool-invalid-error

   ;; Utility
   #:hash-bytes
   #:bytes-to-hex
   #:hex-to-bytes))

(defpackage #:cl-mempool.test
  (:use #:cl #:cl-mempool)
  (:export #:run-tests))
