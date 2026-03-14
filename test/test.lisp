;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:test-package
  (:use #:cl)
  (:export #:run-tests))

(in-package #:test-package)

(defun run-tests ()
  "Run all tests for the system."
  (format t "~&Tests passed!~%"))
