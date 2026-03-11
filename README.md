# cl-mempool

A standalone, pure Common Lisp transaction mempool library with fee-based priority ordering and configurable eviction policies.

## Features

- **Fee-based priority ordering**: Transactions are ordered by mining profitability (effective tip * gas / size)
- **EIP-1559 support**: Full support for dynamic fee transactions with base fee tracking
- **EIP-4844 support**: Blob transaction gas accounting
- **Replace-by-fee (RBF)**: Configurable price bump requirements for transaction replacement
- **Child-pays-for-parent (CPFP)**: Package fee rate calculation for related transactions
- **Nonce tracking**: Automatic promotion of queued transactions when gaps are filled
- **Configurable eviction**: Time-based, size-based, and per-account limits
- **Thread-safe**: All operations are protected by locks (SBCL sb-thread)
- **Zero dependencies**: Pure Common Lisp, no external libraries required

## Installation

Clone the repository and load with ASDF:

```lisp
(asdf:load-system :cl-mempool)
```

## Quick Start

```lisp
(use-package :cl-mempool)

;; Create a mempool with default configuration
(defvar *pool* (make-mempool))

;; Create a transaction
(defvar *tx* (make-tx :hash (hash-bytes #(1 2 3 4))
                      :sender (make-array 20 :element-type '(unsigned-byte 8) :initial-element 1)
                      :nonce 0
                      :gas-limit 21000
                      :gas-price 2000000000))  ; 2 Gwei

;; Add to mempool
(mempool-add *pool* *tx*)

;; Get highest priority transaction
(mempool-top *pool*)

;; Get top N for block building
(mempool-top-n *pool* 100)

;; Pop highest priority (removes from pool)
(mempool-pop *pool*)
```

## Transaction Types

```lisp
;; Legacy transaction (type 0)
(make-tx :type +tx-type-legacy+
         :gas-price 2000000000
         ...)

;; EIP-1559 dynamic fee transaction (type 2)
(make-tx :type +tx-type-dynamic-fee+
         :max-fee-per-gas 3000000000
         :max-priority-fee 1000000000
         ...)

;; EIP-4844 blob transaction (type 3)
(make-tx :type +tx-type-blob+
         :max-fee-per-gas 3000000000
         :max-priority-fee 1000000000
         :blob-gas 131072
         :max-fee-per-blob-gas 1000000000
         ...)
```

## Configuration

```lisp
;; Pool configuration
(make-pool-config
  :max-pool-size 5000        ; Max transaction count
  :max-per-account 64        ; Max txs per sender
  :max-pool-bytes 33554432   ; 32 MB
  :min-gas-price 1000000000  ; 1 Gwei minimum
  :price-bump-percent 10     ; 10% RBF bump required
  :tx-lifetime 10800)        ; 3 hours

;; Eviction configuration
(make-eviction-config
  :max-pool-size 5000
  :max-per-account 64
  :price-bump-percent 10
  :rbf-enabled t
  :evict-remote-first t)
```

## API Reference

### Pool Operations

| Function | Description |
|----------|-------------|
| `(make-mempool &key config eviction-config base-fee)` | Create a new mempool |
| `(mempool-add pool tx)` | Add a transaction (returns T or signals error) |
| `(mempool-remove pool tx)` | Remove a transaction |
| `(mempool-get pool hash)` | Get transaction by hash |
| `(mempool-contains-p pool hash)` | Check if transaction exists |
| `(mempool-size pool)` | Total transaction count |
| `(mempool-clear pool)` | Remove all transactions |

### Priority Operations

| Function | Description |
|----------|-------------|
| `(mempool-top pool)` | Get highest priority transaction |
| `(mempool-top-n pool n)` | Get top N transactions |
| `(mempool-pop pool)` | Remove and return highest priority |
| `(mempool-get-executable pool &key limit)` | Get executable transactions for block building |

### Account Operations

| Function | Description |
|----------|-------------|
| `(mempool-sender-txs pool sender)` | Get all transactions from sender |
| `(mempool-sender-nonce pool sender)` | Get pending nonce for sender |
| `(mempool-sender-count pool sender)` | Count transactions from sender |

### Eviction

| Function | Description |
|----------|-------------|
| `(mempool-evict-expired pool)` | Evict expired transactions |
| `(mempool-evict-lowest pool count)` | Evict N lowest priority transactions |
| `(mempool-evict-by-size pool target)` | Evict until size <= target |
| `(mempool-enforce-limits pool)` | Apply all configured limits |

### RBF/CPFP

| Function | Description |
|----------|-------------|
| `(rbf-allowed-p new-tx old-tx config)` | Check if replacement is allowed |
| `(rbf-minimum-price old-tx config)` | Get minimum replacement price |
| `(cpfp-eligible-p parent child base-fee)` | Check CPFP eligibility |
| `(cpfp-package-fee-rate txs base-fee)` | Calculate package fee rate |

### Error Conditions

| Condition | When Signaled |
|-----------|---------------|
| `mempool-full-error` | Pool is at capacity |
| `mempool-underpriced-error` | Transaction fee too low |
| `mempool-nonce-error` | Invalid nonce |
| `mempool-duplicate-error` | Transaction already exists |
| `mempool-invalid-error` | Invalid transaction |

## Testing

```lisp
(asdf:test-system :cl-mempool)
```

Or manually:

```lisp
(asdf:load-system :cl-mempool/test)
(cl-mempool.test:run-tests)
```

## License

MIT License - See LICENSE file.
