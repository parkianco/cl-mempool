;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

(handler-case
  (progn
    (load (first (directory "*.asd")))
    (let ((sys (first (asdf:registered-systems))))
      (when sys
        (let ((test-sys (find "/" (symbol-name sys) :key #'string)))
          (unless test-sys
            (setf test-sys (concatenate 'string (symbol-name sys) "/test")))
          (format t "Testing: ~A~%" test-sys)
          (asdf:test-system (intern test-sys)))))
    (format t "PASS~%"))
  (error (e)
    (format t "FAIL~%")))
(quit)
