;;; Filename: HASH_STACK.LSP

;;; A simple stack of items with hash table indexing to any item.

(defpackage :hash_stack_pkg
  (:use :cl)
  (:nicknames :hs))


(in-package :hs)


(defstruct hash_stack
  (stack nil :type list)
  (table nil :type hash-table)
  (keyfn nil :type function))   ;applied to items in the stack to get a hash key



(declaim (inline hash_stack_is_empty push_onto_hash_stack pop_from_hash_stack
                 peek_at_hash_stack hash_stack_count find_in_hash_stack))



(defun create_hash_stack (size test keyfn)
  (make-hash_stack :table (make-hash-table :size size :test test)
                   :keyfn keyfn))



(defun hash_stack_is_empty (hstk)
  (declare (hash_stack hstk))
  (null (hash_stack-stack hstk)))



(defun hash_stack_count (hstk)
  (declare (hash_stack hstk))
  (hash-table-count (hash_stack-table hstk)))



(defun peek_at_hash_stack (index hstk)
  (declare (fixnum index) (hash_stack hstk))
  (nth index (hash_stack-stack hstk)))



(defun push_onto_hash_stack (item hstk)
  (declare (hash_stack hstk))
  (push item (hash_stack-stack hstk))
  (setf (gethash (funcall (hash_stack-keyfn hstk) item)
                 (hash_stack-table hstk)) item))
    
    
    
(defun find_in_hash_stack (key hstk)
  (declare (hash_stack hstk))
  (gethash key (hash_stack-table hstk)))



(defun pop_from_hash_stack (hstk)
  (declare (hash_stack hstk))
  (when (hash_stack_is_empty hstk)
    (error "Cannot remove item from empty hash stack: in remove_from_hash_stack."))
  (let ((item (pop (hash_stack-stack hstk))))
    (remhash (funcall (hash_stack-keyfn hstk) item) (hash_stack-table hstk))
    item))