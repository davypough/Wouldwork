;;; Filename: HASH-STACK.LSP

;;; A simple stack of items with hash table indexing to any item.

(defpackage :hash-stack-pkg
  (:use :cl)
  (:nicknames :hs))


(in-package :hs)


(defstruct hash-stack
  (stack nil :type list)
  (table nil :type hash-table)
  (keyfn nil :type function))   ;applied to items in the stack to get a hash key



(declaim (inline hash-stack-is-empty push-onto-hash-stack pop-from-hash-stack
                 peek-at-hash-stack hash-stack-count find-in-hash-stack))



(defun create-hash-stack (size test keyfn)
  (make-hash-stack :table (make-hash-table :size size :test test)
                   :keyfn keyfn))



(defun hash-stack-is-empty (hstk)
  (declare (hash-stack hstk))
  (null (hash-stack-stack hstk)))



(defun hash-stack-count (hstk)
  (declare (hash-stack hstk))
  (hash-table-count (hash-stack-table hstk)))



(defun peek-at-hash-stack (index hstk)
  (declare (fixnum index) (hash-stack hstk))
  (nth index (hash-stack-stack hstk)))



(defun push-onto-hash-stack (item hstk)
  (declare (hash-stack hstk))
  (push item (hash-stack-stack hstk))
  (setf (gethash (funcall (hash-stack-keyfn hstk) item)
                 (hash-stack-table hstk)) item))
    
    
    
(defun find-in-hash-stack (key hstk)
  (declare (hash-stack hstk))
  (gethash key (hash-stack-table hstk)))



(defun pop-from-hash-stack (hstk)
  (declare (hash-stack hstk))
  (when (hash-stack-is-empty hstk)
    (error "Cannot remove item from empty hash stack: in remove-from-hash-stack."))
  (let ((item (pop (hash-stack-stack hstk))))
    (remhash (funcall (hash-stack-keyfn hstk) item) (hash-stack-table hstk))
    item))