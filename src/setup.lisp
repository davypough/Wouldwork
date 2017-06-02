;;; Filename: setup.lisp

;;; Setup for planning program.

(in-package :ww)


(defstruct (problem-state (:print-function print-problem-state) (:copier nil))
  ;A planning state including the current propositional database
  (name nil :type symbol)  ;last action executed
  (instantiations nil :type list)  ;from last action effect
  (happenings nil :type list)  ;plist of (next-index next-time next-direction) object pairs
  (time 0 :type real)
  (db (make-hash-table) :type hash-table))


(defun print-problem-state (state stream depth)
  (declare (ignore depth) (special *monitored-relations*))
  (format stream "<~A ~A ~A ~A~%~A>~%"
    (problem-state-name state)
    (problem-state-instantiations state)
    (problem-state-happenings state)
    (problem-state-time state)
    (sort (loop for key being the hash-keys in (problem-state-db state)
              using (hash-value value)
              when (gethash (car key) *monitored-relations*)
              if (eq value t)
              collect key
              else collect (append key value))
          #'string< :key (lambda (x) (symbol-name (car x))))))


(defun copy-problem-state (state)
  (make-problem-state
   :name (problem-state-name state)
   :instantiations (copy-list (problem-state-instantiations state))
   :happenings (copy-list (problem-state-happenings state))
   :time (problem-state-time state)
   :db (alexandria:copy-hash-table (problem-state-db state))))


(defstruct action
  (name nil :type symbol)
  (duration 0 :type real)
  (precondition-variables nil :type list)
  (precondition-types nil :type list)
  (precondition-lambda nil :type list)
  (precondition #'identity :type function)
  (effect-variables nil :type list)
  (effect-types nil :type list)
  (effect-lambda nil :type list)
  (effect #'identity :type function)) ;temp default


(defvar *debug* 0)
  ;Set this parameter to a number (n) to display debugging info.
  ;0 - no debugging
  ;1 - display full search tree
  ;2 - search tree + basic node info
  ;3 - search tree + full node info
  ;4 - search tree + full node info + break after each expansion cycle

(setq *print-right-margin* 140)
  ;Allows non-wrap printing of *search-tree* for deep trees.

(defparameter *max-states* 10000000)
  ;Estimated maximum number of unique states to be explored during search.
  ;If this number is exceeded, hash table resizing will slow search significantly.

(defparameter *first-solution-sufficient* nil)
  ;Specify whether only one solution, the first found, is required, ending search.

(defparameter *progress-reporting-interval* 100000)
  ;Print progress during search after each multiple n of states examined.

(defvar *depth-cutoff* 20)
  ;The max possible number of steps to consider toward any goal.
  ;Negative or 0 means no cutoff, value set in problem.lisp.

(defparameter *start-state* (make-problem-state :db (make-hash-table :test #'equal)))

(defparameter *tree-or-graph* 'graph)
  ;Whether there are repeated states (graph) or not (tree)

(defparameter *search-tree* nil)
  ;The dfs summary representation of the search

(defparameter *solutions* nil)
  ;The resulting list of solutions found.


(defparameter *connectives* '(and or not))

(defparameter *relations* (make-hash-table :test #'eq))

(defparameter *monitored-relations* (make-hash-table :test #'eq))

(defparameter *types* (make-hash-table :test #'eq))

(defparameter *derived* (make-hash-table :test #'eq))

(defparameter *function-names* nil)  ;list of all user-defined functions

(defparameter *symmetrics* (make-hash-table :test #'eq))

(defparameter *actions* nil)  ;list of all potential actions

(defparameter *happenings* nil)  ;the list of objects having exogenous events.

(defparameter *db* (make-hash-table :test #'equal))
  ;Holds initial database only

(defparameter *constraint* nil)  ;will hold any constraint test function

(defparameter *goal* nil)  ;will hold the goal test function
