;;;; src/util.lisp - Utility Functions
;;;;
;;;; Helper functions for cl-mempool

(in-package #:cl-mempool)

;;; Time Utilities

(defun current-time-ms ()
  "Return current time in milliseconds since epoch."
  (let ((now (get-universal-time)))
    (* now 1000)))

(defun current-time-seconds ()
  "Return current time in seconds since epoch."
  (get-universal-time))

;;; Hash Utilities

(defun hash-bytes (bytes)
  "Compute a simple hash of a byte vector. Returns a 32-byte vector.
   Note: This is a placeholder. In production, use a proper cryptographic hash."
  (let ((result (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0))
        (len (length bytes)))
    (loop for i below (min len 32)
          do (setf (aref result i) (aref bytes i)))
    ;; Simple mixing for remaining bytes
    (loop for i from 32 below len
          for j = (mod i 32)
          do (setf (aref result j)
                   (logxor (aref result j) (aref bytes i))))
    result))

(defun bytes-to-hex (bytes)
  "Convert a byte vector to a hexadecimal string."
  (with-output-to-string (s)
    (loop for byte across bytes
          do (format s "~2,'0x" byte))))

(defun hex-to-bytes (hex-string)
  "Convert a hexadecimal string to a byte vector."
  (let* ((len (length hex-string))
         (bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
    (loop for i below (/ len 2)
          do (setf (aref bytes i)
                   (parse-integer hex-string :start (* i 2) :end (+ (* i 2) 2) :radix 16)))
    bytes))

;;; Comparison Utilities

(defun bytes= (a b)
  "Compare two byte vectors for equality."
  (and (= (length a) (length b))
       (loop for i below (length a)
             always (= (aref a i) (aref b i)))))

(defun bytes< (a b)
  "Lexicographic comparison of two byte vectors."
  (loop for i below (min (length a) (length b))
        do (cond ((< (aref a i) (aref b i)) (return t))
                 ((> (aref a i) (aref b i)) (return nil)))
        finally (return (< (length a) (length b)))))

;;; Numeric Utilities

(defun safe-div (a b &optional (default 0))
  "Safely divide A by B, returning DEFAULT if B is zero."
  (if (zerop b) default (/ a b)))

(defun clamp (value min-val max-val)
  "Clamp VALUE to be between MIN-VAL and MAX-VAL."
  (max min-val (min max-val value)))

;;; Thread-Safe Utilities

(defmacro with-lock ((lock) &body body)
  "Execute BODY with LOCK held."
  #+sbcl
  `(sb-thread:with-mutex (,lock)
     ,@body)
  #-sbcl
  `(progn ,@body))

(defun make-lock (&optional name)
  "Create a new lock."
  #+sbcl
  (sb-thread:make-mutex :name (or name "cl-mempool-lock"))
  #-sbcl
  nil)

;;; Heap Utilities (for priority queue)

(defun heap-parent (index)
  "Return parent index in a binary heap."
  (floor (1- index) 2))

(defun heap-left (index)
  "Return left child index in a binary heap."
  (1+ (* 2 index)))

(defun heap-right (index)
  "Return right child index in a binary heap."
  (+ 2 (* 2 index)))
