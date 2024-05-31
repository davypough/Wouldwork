;;; Filename: ww-hstack.lisp

;;; An hstack (hash stack) is a stack representation containing an adjustable
;;; one-dimensional array to organize elements, plus a hash table for quickly
;;; determining if an element is in the stack. New elements are pushed
;;; at the fill-pointer, and popped at the fill-pointer minus 1. 

(in-package :hs)


(defstruct (hstack (:conc-name hstack.))
  "An hstack (hash stack) is a functional stack containing an adjustable
   one-dimensional array of elements, plus a hash table for quickly
   determining if an element is in the stack. Keyfn is applied to elements to
   access the hash table. New elements are pushed at the fill-pointer, and
   popped at the fill-pointer minus 1."
  (vector (make-array 0 :adjustable t :fill-pointer t) :type (array * (*)))
  (table (make-hash-table) :type hash-table)  ;change to take a custom hash table
  (keyfn #'identity :type function))  ;fn to get hash table keys


(defun push-hstack (elt hstk)
  "Pushes an element onto hstack's vector and table."
  (setf (gethash (funcall (hstack.keyfn hstk) elt) (hstack.table hstk))
        elt)
  (vector-push-extend elt (hstack.vector hstk))
  hstk)


(defun pop-hstack (hstk)
  "Pops an element from hstack's vector and removes it from the table. Error if empty."
  (let* ((vec (hstack.vector hstk))
         (fptr-1 (1- (fill-pointer vec)))
         (tbl (hstack.table hstk))
         (key (funcall (hstack.keyfn hstk) (aref vec fptr-1))))
    (remhash key tbl)
    (vector-pop vec)))


(defun empty-hstack (hstk)
  "Determine if a hash stack is empty."
  (zerop (fill-pointer (hstack.vector hstk))))


(defun length-hstack (hstk)
  (length (hstack.vector hstk)))


(defun nth-hstack (n hstk)
  (aref (hstack.vector hstk) n))


(defun key-present-hstack (key hstk)
  "Test whether a key is present in a hash stack,
   returns the value associated with that key."
  (gethash key (hstack.table hstk)))


(defun deletef-nth-hstack (n hstk)
  "Deletes the nth entry in a hash stack and returns it."
  (declare (type fixnum n) (hstack hstk))
  (let* ((vec (hstack.vector hstk))
         (tbl (hstack.table hstk))
         (nth-entry (aref vec n))
         (key (funcall (hstack.keyfn hstk) nth-entry)))
    (remhash key tbl)
    (setf vec (delete-if (constantly t) vec :start n :count 1))
    ;(decf (fill-pointer vec))
    nth-entry))


(defun size-hstack (hstk)
  (array-total-size (hstack.vector hstk)))


(defun peek-hstack (hstk)
  (let ((fptr (fill-pointer (hstack.vector hstk))))
    (when (> fptr 0)
      (aref (hstack.vector hstk) (1- fptr)))))


(defun clear-hstack (hstk)
  (setf (fill-pointer (hstack.vector hstk)) 0)
  (clrhash (hstack.table hstk)))
