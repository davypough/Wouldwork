;;;; Filename: hstack.lisp

;;; An hstack (hash stack) is a stack representation containing an adjustable
;;; one-dimensional array to organize elements, plus a hash table for quickly
;;; determining if an element is in the stack. New elements are pushed
;;; at the fill-pointer, and popped at the fill-pointer minus 1. 

(in-package :hs)


(defstruct (hstack (:print-function print-hstack))
  "An hstack (hash stack) is a functional stack containing an adjustable
   one-dimensional array of elements, plus a hash table for quickly
   determining if an element is in the stack. Keyfn is applied to elements to
   access the hash table. New elements are pushed at the fill-pointer, and
   popped at the fill-pointer minus 1."
  (vector (make-array 0 :adjustable t :fill-pointer t) :type (array * (*)))
  (table (make-hash-table) :type hash-table)  ;change to take a custom hash table
  (keyfn #'identity :type function))  ;fn to get hash table keys


(defun print-hstack (hstack stream depth)
  (declare (hstack hstack) (ignore stream depth))
  (ut::show (hstack-vector hstack)))
  ;(format stream "{HSTACK :COUNT ~D}" (hash-table-count (hstack-table hstack))))


(defun create-hstack (&key (element-type t) (ht-keyfn #'identity) (ht-test 'eql) (synchronized nil))
  (make-hstack :vector (make-array 0 :element-type element-type :adjustable t :fill-pointer t)
               :table (if synchronized
                        (make-hash-table :test ht-test :synchronized synchronized)
                        (make-hash-table :test ht-test))
               :keyfn ht-keyfn))


(defun push-hstack (elt hstk)
  (incf (gethash (funcall (hstack-keyfn hstk) elt) (hstack-table hstk) 0))
  (vector-push-extend elt (hstack-vector hstk)))


(defun pop-hstack (hstk)
  "Pops an element from hstack's vector. Also decrements the element's count from
   the element's hash table entry (and the entry itself if it's the last one)."
  (let* ((vec (hstack-vector hstk))
         (fptr-1 (1- (fill-pointer vec)))
         (tbl (hstack-table hstk))
         (key (funcall (hstack-keyfn hstk) (aref vec fptr-1))))
    (when (zerop (decf (gethash key tbl)))
      (remhash key tbl))
    (vector-pop vec)))


(defun empty-hstack (hstk)
  (zerop (fill-pointer (hstack-vector hstk))))


(defun length-hstack (hstk)
  (length (hstack-vector hstk)))


(defun nth-hstack (n hstk)
  (aref (hstack-vector hstk) n))


(defun key-present-hstack (key hstk)
  (gethash key (hstack-table hstk)))


(defun in-hstack (key hstk)
  (loop for element across (hstack-vector hstk)
          thereis (funcall (hash-table-test (hstack-table hstk))
                           key (funcall (hstack-keyfn hstk) element))))


(defun deletef-nth-hstack (n hstk)
  (let* ((vec (hstack-vector hstk))
         (tbl (hstack-table hstk))
         (key (funcall (hstack-keyfn hstk) (aref vec n))))
    (when (zerop (decf (gethash key tbl)))
      (remhash key tbl))
    (setf vec (delete-if (constantly t) vec :start n :count 1))))


(defun size-hstack (hstk)
  (array-total-size (hstack-vector hstk)))


(defun peek-hstack (hstk)
  (let ((fptr (fill-pointer (hstack-vector hstk))))
    (when (> fptr 0)
      (aref (hstack-vector hstk) (1- fptr)))))


(defun clear-hstack (hstk)
  (setf (fill-pointer (hstack-vector hstk)) 0)
  (clrhash (hstack-table hstk)))
