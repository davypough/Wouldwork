;;; Filename: setup.lisp

;;; Setup for planning program.

(in-package :pl)


(defstruct (problem_state (:print-function print_problem_state))
  ;A planning state including the current propositional database
  (name nil :type symbol)  ;last action executed
  (instantiations nil :type list)  ;from last action effect
  (happenings nil :type list)  ;plist of (next_index next_time next_direction) object pairs
  (time 0 :type real)
  (db (make-hash-table) :type hash-table))


(defun print_problem_state (state stream depth)
  (declare (ignore depth) (special *monitored_relations*))
  (format stream "<~A ~A ~A ~A~%~A>~%"
    (problem_state-name state)
    (problem_state-instantiations state)
    (problem_state-happenings state)
    (problem_state-time state)
    (sort (loop for key being the hash-keys in (problem_state-db state)
              using (hash-value value)
              when (gethash (car key) *monitored_relations*)
              if (eq value t)
              collect key
              else collect (append key value))
          #'string< :key (lambda (x) (symbol-name (car x))))))


(defun copy_problem_state (state)
  (make-problem_state
   :name (problem_state-name state)
   :instantiations (copy-list (problem_state-instantiations state))
   :happenings (copy-list (problem_state-happenings state))
   :time (problem_state-time state)
   :db (alexandria:copy-hash-table (problem_state-db state))))


(defstruct action
  (name nil :type symbol)
  (duration 0 :type real)
  (precondition_variables nil :type list)
  (precondition_types nil :type list)
  (precondition_lambda nil :type list)
  (precondition #'identity :type function)
  (effect_variables nil :type list)
  (effect_types nil :type list)
  (effect_lambda nil :type list)
  (effect #'identity :type function)) ;temp default


(defvar *debug* 0)
  ;Set this parameter to a number (n) to display debugging info.
  ;0 - no debugging
  ;1 - display full search tree
  ;2 - search tree + basic node info
  ;3 - search tree + full node info
  ;4 - search tree + full node info + break after each expansion cycle

(setq *print-right-margin* 140)
  ;Allows non-wrap printing of *search_tree* for deep trees.

(defparameter *max_states* 10000000)
  ;Estimated maximum number of unique states to be explored during search.
  ;If this number is exceeded, hash table resizing will slow search significantly.

(defparameter *first_solution_sufficient* nil)
  ;Specify whether only one solution, the first found, is required, ending search.

(defparameter *progress_reporting_interval* 100000)
  ;Print progress during search after each multiple n of states examined.

(defvar *depth_cutoff* 20)
  ;The max possible number of steps to consider toward any goal.
  ;Negative or 0 means no cutoff, value set in problem.lisp.

(defparameter *start_state* (make-problem_state :db (make-hash-table :test #'equal)))

(defparameter *tree_or_graph* 'graph)
  ;Whether there are repeated states (graph) or not (tree)

(defparameter *search_tree* nil)
  ;The dfs summary representation of the search


(defparameter *connectives* '(and or not))

(defparameter *relations* (make-hash-table :test #'eq))

(defparameter *monitored_relations* (make-hash-table :test #'eq))

(defparameter *types* (make-hash-table :test #'eq))

(defparameter *derived* (make-hash-table :test #'eq))

(defparameter *function_names* nil)  ;list of all user-defined functions

(defparameter *symmetrics* (make-hash-table :test #'eq))

(defparameter *actions* nil)  ;list of all potential actions

(defparameter *happenings* nil)  ;the list of objects having exogenous events.

(defparameter *db* (make-hash-table :test #'equal))
  ;Holds initial database only

(defparameter *constraint* nil)  ;will hold any constraint test function

(defparameter *goal* nil)  ;will hold the goal test function
