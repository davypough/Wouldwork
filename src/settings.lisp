;;; Filename: settings.lisp

;;; Default settings for planning program.

(in-package :ww)


(defparameter *num-parallel-threads* 0
  "Set the number of parallel threads to use.
    0 means no parallelism (ie, serial processing)
    1 means use one parallel thread
      (in addition to parallel management, effectively serial, useful for debugging)
    2 means two or more parallel processing threads
    N up to the number of available CPU threads -1")
(declaim (fixnum *num-parallel-threads*))


(defparameter *debug* 0
  "Set the debug level for subsequent runs.
    0 - no debugging
    1 - display full search tree
    2 - display full search tree with states
    3 - display basic nodes
    4 - display full nodes
    5 - display full nodes + break after each expansion cycle")
(declaim (fixnum *debug*))


(setq *print-right-margin* 140)
;Allows non-wrap printing of *search-tree* for deep trees.


;Global settings for wouldwork planner
(let ((settings (make-hash-table :test #'eq)))
  ;Estimated maximum number of unique states to be explored during search.
  ;If this number is exceeded, hash table resizing may slow graph search.
    (setf (gethash 'max-states settings) 10000)
  ;Name of the current problem, assigned in problem.lisp by user.
    (setf (gethash 'problem settings) 'unspecified)
  ;Specify whether to search for 'first, 'min-length, 'min-time, or 'every solution.
    (setf (gethash 'solution-type settings) 'first)
  ;Print progress during search after each multiple n of states examined.
    (setf (gethash 'progress-reporting-interval settings) 100000)
  ;The max possible number of steps to consider toward any goal.
  ;Negative or 0 means no cutoff, value set in problem.lisp.
    (setf (gethash 'depth-cutoff settings) 10)
  ;Whether there are repeated states (graph) or not (tree); try both
    (setf (gethash 'tree-or-graph settings) 'graph)
  (defun ww-get (var)  ;accessors for ww settings
    (gethash var settings))
  (defun ww-set (var value)
    (if (ut::hash-table-present var settings)
      (setf (gethash var settings) value)
      (error "~A is not a setable Wouldwork parameter." var))))


;To display debugging info, (ww-set 'debug <n>) where <n> is 0-5.
  ;0 - no debugging
  ;1 - display full search tree
  ;2 - display full search tree with states
  ;3 - display basic nodes
  ;4 - display full nodes
  ;5 - display full nodes + break after each expansion cycle


(defparameter *types* (make-hash-table :test #'eq)
  "Table of all types.")
(declaim (hash-table *types*))

(defparameter *relations* (make-hash-table :test #'eq)
  "Dynamic relations.")
(declaim (hash-table *relations*))

(defparameter *static-relations* (make-hash-table :test #'eq)
  "Static relations.")
(declaim (hash-table *static-relations*))

(defparameter *connectives* '(and or not)
  "Logical connectives.")
(declaim (list *connectives*))

(defparameter *symmetrics* (make-hash-table :test #'eq)
  "Symmetric relations.")
(declaim (hash-table *symmetrics*))

(defparameter *complements* (make-hash-table :test #'eq)
  "Table of complement relations.")
(declaim (hash-table *complements*))

(defparameter *fluent-relation-indices* (make-hash-table :test #'eq)
  "List of fluent argument indices for a relation.")
(declaim (hash-table *fluent-relation-indices*))

(defparameter *db* (make-hash-table :test #'equal)
  "Initial database of dynamic relations.")
(declaim (hash-table *db*))

(defparameter *idb* (make-hash-table)
  "Initial integer database of dynamic propositions.")
(declaim (hash-table *idb*))

(defparameter *constant-integers* (make-hash-table)
  "Integer codes for the problem's object constants.")
(declaim (hash-table *constant-integers*))

(defparameter *integer-constants* (make-hash-table)
  "Translating codes back to constants for printout.")
(declaim (hash-table *integer-constants*))

(defparameter *min-action-duration* 0.0
  "The least action duration among all actions.")
(declaim (real *min-action-duration*))

(defparameter *query-names* nil
  "List of all user-defined query functions.")
(declaim (list *query-names*))

(defparameter *update-names* nil
  "List of all user-defined update functions.")
(declaim (list *update-names*))

(defparameter *actions* nil
  "List of all potential actions.")
(declaim (list *actions*))

(defparameter *init-actions* nil
  "List of all initialization actions.")
(declaim (list *init-actions*))

(defparameter *happenings* nil
  "The list of objects having exogenous events.")
(declaim (list *happenings*))

(defparameter *static-db* (make-hash-table :test #'equal)
  "Initial database of static propositions.")
(declaim (hash-table *static-db*))

(defparameter *static-idb* (make-hash-table)
  "Initial integer database of static propositions.")
(declaim (hash-table *static-idb*))

(defparameter *goal* nil
  "Holds the goal test function--value is a lambda expression, symbol-function is the function.")
(declaim (list *goal*))

(defparameter *constraint* nil
  "Any constraint test function.")
(declaim (list *constraint*))

(defparameter *last-object-index* 0
  "Last index of object constants seen so far in propositions.")
(declaim (fixnum *last-object-index*))
