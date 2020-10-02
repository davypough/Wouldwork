;;; Filename: frequencies.lisp

;;; Computes the frequencies of action macro sequences in *solutions*.
;;; Eg, (freq 1 2 3) will compute the frequencies of all macro sequences
;;; of length 1, 2, and 3 appearing in the solutions to a problem.
;;; Use on a small problem to develop macros to apply to a larger problem.

(in-package :ww)

(defparameter *freq-ht* (make-hash-table))  ;the ht of action sequences - count

(defun freq (&rest run-lengths)
  "Return the alist of sequences with their respective counts, max first."
  (clrhash *freq-ht*)
  (iter (for solution in *solutions*)
        (iter (for len in run-lengths)
              (process-solution solution len)))
  (let ((alist (alexandria:hash-table-alist *freq-ht*)))
    (sort alist #'> :key #'cdr)))


(defun process-solution (solution run-length)
  "Installs sequences of run-length in a solution into a hash table."
  (iter (with path = (solution-path solution))
        (for steps on path)
        (for i from 0 to (- (length path) run-length))
        (for run = (subseq steps 0 run-length))
        (for sequence-list = (mapcar #'(lambda (x) (car (second x))) run))
        (for csv-list = (format nil "~{~A~^=>~}" sequence-list))
        (for symbol-list = (ut::intern-symbol csv-list))
        (incf (gethash symbol-list *freq-ht* 0))))