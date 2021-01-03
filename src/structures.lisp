;;; Filename: structures.lisp

;;; Structure definitions.


(in-package :ww)


(defstruct (problem-state (:print-function print-problem-state) (:copier nil))
  ;A planning state including the current propositional database
  (name nil :type symbol)  ;last action executed
  (instantiations nil :type list)  ;from last action effect
  (happenings nil :type list)  ;a list of (object (next-index next-time next-direction)) pairs
  (time 0.0 :type real)
  (value 0.0 :type real)
  (heuristic 0.0 :type real)
  (idb (make-hash-table) :type hash-table)  ;integer hash table of propositions
  (hidb (make-hash-table) :type hash-table))  ;integer table for happening events
;Note: hidb is separate from idb because otherwise each exogenous event will change
;the state, leading to endless revisiting of the same similar state
;Note: happenings contains an entry for each object's next event, updated as events occur


(defun convert-to-proposition (integer)
  ;Converts an integer code back to a proposition.
  (iter (with x = integer)
        (for (int triple) = (multiple-value-list (truncate x 1000)))
        (collecting triple into int-list)
        (until (zerop int))
        (setf x int)
        (finally (return (mapcar (lambda (i)
                                   (gethash i *integer-constants*))
                                 int-list)))))


(defun get-prop-fluent-indices (proposition)
  (gethash (car proposition) *fluent-relation-indices*))


(defun convert-to-fluent-proposition (key values)
  ;Converts an idb partial prop -> index values into literal prop.
  (loop with partial-prop = (convert-to-proposition key)
        for index in (get-prop-fluent-indices partial-prop)
        for value in values
          do (ut::ninsert-list value index partial-prop)
        finally (return partial-prop)))


(defun list-database (db)
  (let ((propositions (iter (for (key values) in-hashtable db)
                            (if (eq values t)
                                (collecting (convert-to-proposition key))  ;non-fluent prop
                                (collecting (convert-to-fluent-proposition key values))))))
    (sort propositions #'string< :key (lambda (prop) (format nil "~A" prop)))))
;    (loop for prop in propositions
;          sum (expt 2 (1- (read-from-string (subseq (format nil "~A" (second prop)) 5)))))))


(defun print-problem-state (state stream depth)
  (declare (problem-state state) (ignore depth))
  (format stream "<~A ~A ~A ~A ~A ~A~%  ~A~%  ~A>"
      (problem-state-name state)
      (problem-state-instantiations state)
      (problem-state-happenings state)
      (problem-state-time state)
      (problem-state-value state)
      (problem-state-heuristic state)
      (list-database (problem-state-idb state))
      (list-database (problem-state-hidb state))))


(defun copy-problem-state (state)
  (make-problem-state
   :name (problem-state-name state)
   :instantiations (copy-list (problem-state-instantiations state))
   :happenings (copy-tree (problem-state-happenings state))
   :time (problem-state-time state)
   :value (problem-state-value state)
   :heuristic (problem-state-heuristic state)
   :idb (copy-db (problem-state-idb state))
   :hidb (copy-db (problem-state-hidb state))))


(defun copy-db (db)
  (declare (hash-table db))
  (alexandria:copy-hash-table db
    :key (lambda (value)
           (if (listp value)
             (copy-list value)
             value))))


(defparameter *start-state* (make-problem-state)
  "Start search from this state.")
(declaim (problem-state *start-state*))


(defstruct action
  (name nil :type symbol)
  (duration 0.0 :type real)
  (precondition-params nil :type list)
  (precondition-variables nil :type list)
  (precondition-types nil :type list)
  (dynamic nil :type (or nil t))  ;a dynamic rule requires recomputation of params on each execution
  (precondition-instantiations nil :type (or list symbol))
  (precondition-lambda nil :type list)
  (iprecondition-lambda nil :type list)
  ;(precondition-lits nil :type list)  ;used for backward search (not implemented)
  (iprecondition #'identity :type function)
  (effect-variables nil :type list)
  (effect-types nil :type list)
  (effect-adds nil :type list)  ;nonnegative literals only for backward search
  (effect-lambda nil :type list)
  (ieffect-lambda nil :type list)
  (ieffect #'identity :type function))


(defstruct update  ;db updates resulting from a successful action instantiation
  (changes nil :type list)
  (value 0.0 :type real)
  (instantiations nil :type list)
  (followups nil :type list))  ;next & finally followup function calls


(defstruct solution  ;the record of a solution
  (depth 0 :type fixnum)
  (time 0.0 :type real)
  (value 0.0 :type real)
  (path nil :type list)
  (goal (make-problem-state) :type problem-state))

