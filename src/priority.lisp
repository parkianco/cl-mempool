;;;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; src/priority.lisp - Priority Queue Implementation
;;;;
;;;; Binary heap-based priority queue for transaction ordering

(in-package #:cl-mempool)

;;; Priority Heap Structure

(defstruct (priority-heap (:constructor %make-priority-heap)
                          (:copier nil))
  "A binary max-heap for transaction priority ordering."
  (elements (make-array 1024 :adjustable t :fill-pointer 0)
            :type vector)
  (base-fee 0 :type unsigned-byte)
  (lock (make-lock "priority-heap-lock")))

(defun make-priority-heap (&key (initial-capacity 1024) (base-fee 0))
  "Create a new priority heap."
  (%make-priority-heap
   :elements (make-array initial-capacity :adjustable t :fill-pointer 0)
   :base-fee base-fee))

;;; Heap Operations (Internal)

(defun %heap-swap (heap i j)
  "Swap elements at indices I and J."
  (let ((elements (priority-heap-elements heap)))
    (rotatef (aref elements i) (aref elements j))))

(defun %heap-compare (heap i j)
  "Return T if element at I has higher priority than element at J."
  (let ((elements (priority-heap-elements heap))
        (base-fee (priority-heap-base-fee heap)))
    (tx-better-p (aref elements i) (aref elements j) base-fee)))

(defun %heap-sift-up (heap index)
  "Sift element at INDEX up to its correct position."
  (loop while (and (> index 0)
                   (%heap-compare heap index (heap-parent index)))
        do (let ((parent (heap-parent index)))
             (%heap-swap heap index parent)
             (setf index parent))))

(defun %heap-sift-down (heap index)
  "Sift element at INDEX down to its correct position."
  (let ((size (length (priority-heap-elements heap))))
    (loop
      (let ((left (heap-left index))
            (right (heap-right index))
            (largest index))
        (when (and (< left size) (%heap-compare heap left largest))
          (setf largest left))
        (when (and (< right size) (%heap-compare heap right largest))
          (setf largest right))
        (when (= largest index)
          (return))
        (%heap-swap heap index largest)
        (setf index largest)))))

;;; Public Heap Operations

(defun heap-push (heap tx)
  "Add TX to the priority heap."
  (with-lock ((priority-heap-lock heap))
    (vector-push-extend tx (priority-heap-elements heap))
    (%heap-sift-up heap (1- (length (priority-heap-elements heap))))))

(defun heap-pop (heap)
  "Remove and return the highest priority transaction."
  (with-lock ((priority-heap-lock heap))
    (let ((elements (priority-heap-elements heap)))
      (when (zerop (length elements))
        (return-from heap-pop nil))
      (let ((top (aref elements 0)))
        (setf (aref elements 0) (vector-pop elements))
        (when (> (length elements) 0)
          (%heap-sift-down heap 0))
        top))))

(defun heap-peek (heap)
  "Return the highest priority transaction without removing it."
  (with-lock ((priority-heap-lock heap))
    (let ((elements (priority-heap-elements heap)))
      (when (> (length elements) 0)
        (aref elements 0)))))

(defun heap-size (heap)
  "Return the number of elements in the heap."
  (length (priority-heap-elements heap)))

(defun heap-empty-p (heap)
  "Return T if the heap is empty."
  (zerop (heap-size heap)))

(defun heap-clear (heap)
  "Remove all elements from the heap."
  (with-lock ((priority-heap-lock heap))
    (setf (fill-pointer (priority-heap-elements heap)) 0)))

(defun heap-update-base-fee (heap new-base-fee)
  "Update the base fee and reheapify.
   This is needed when the base fee changes."
  (with-lock ((priority-heap-lock heap))
    (setf (priority-heap-base-fee heap) new-base-fee)
    ;; Rebuild the heap with new priorities
    (let ((elements (priority-heap-elements heap)))
      (loop for i from (floor (length elements) 2) downto 0
            do (%heap-sift-down heap i)))))

(defun heap-remove (heap tx)
  "Remove TX from the heap. Returns T if found and removed."
  (with-lock ((priority-heap-lock heap))
    (let* ((elements (priority-heap-elements heap))
           (hash (tx-hash tx))
           (index (position-if (lambda (e) (bytes= (tx-hash e) hash)) elements)))
      (when index
        ;; Replace with last element and reheapify
        (setf (aref elements index) (vector-pop elements))
        (when (< index (length elements))
          (%heap-sift-down heap index)
          (%heap-sift-up heap index))
        t))))

(defun heap-top-n (heap n)
  "Return the top N highest priority transactions.
   Does not remove them from the heap."
  (with-lock ((priority-heap-lock heap))
    (let* ((elements (priority-heap-elements heap))
           (base-fee (priority-heap-base-fee heap))
           (size (length elements))
           (count (min n size)))
      (if (<= count 0)
          nil
          ;; Copy and sort to get top N
          (let ((sorted (sort (copy-seq elements)
                              (lambda (a b) (tx-better-p a b base-fee)))))
            (coerce (subseq sorted 0 count) 'list))))))

(defun heap-find-by-hash (heap hash)
  "Find a transaction by its hash."
  (with-lock ((priority-heap-lock heap))
    (find-if (lambda (tx) (bytes= (tx-hash tx) hash))
             (priority-heap-elements heap))))

(defun heap-find-by-sender (heap sender)
  "Find all transactions from SENDER."
  (with-lock ((priority-heap-lock heap))
    (coerce (remove-if-not (lambda (tx) (bytes= (tx-sender tx) sender))
                           (priority-heap-elements heap))
            'list)))

(defun heap-to-list (heap)
  "Convert heap to a list of transactions (unsorted)."
  (with-lock ((priority-heap-lock heap))
    (coerce (copy-seq (priority-heap-elements heap)) 'list)))

(defun heap-to-sorted-list (heap)
  "Convert heap to a priority-sorted list of transactions."
  (with-lock ((priority-heap-lock heap))
    (let ((base-fee (priority-heap-base-fee heap)))
      (sort (coerce (copy-seq (priority-heap-elements heap)) 'list)
            (lambda (a b) (tx-better-p a b base-fee))))))
