;;; Filename: settings.lisp

;;; Default settings for planning program.

(in-package :ww)


;Note: It is necessary to close & reopen the lisp environment after
;      changing from nonparallel to parallel, or parallel to nonparallel here.
(defparameter *threads* 0
  "The number of parallel threads to use.
    0 means no parallelism (ie, serial processing)
    1 means use one parallel thread
      (in addition to parallel management, effectively serial, useful for debugging)
    2 means two or more parallel processing threads
    N up to the number of available CPU threads")
(declaim (fixnum *threads*))


(declaim (ftype (function (&rest list) boolean) eql*))
(defun eql* (&rest arguments)
  (every #'eql arguments (rest arguments)))


(setq *print-right-margin* 140)
;Allows non-wrap printing of *search-tree* for deep trees.


(unintern 'heuristic?)
(unintern 'prune?)
(unintern 'bounding-function?)
;Reset user defined functions, when defined on previous load.


(defparameter *problem* 'unspecified
  "Name of the current problem, assigned in problem.lisp by user.")
(declaim (symbol *problem*))

(defparameter *solution-type* 'first
  "Specify whether to search for 'first, 'min-length, 'min-time, or 'every solution.")
(declaim (symbol *solution-type*))

(defparameter *tree-or-graph* 'graph
  "Whether there are repeated states (graph) or not (tree); try both.")
(declaim (symbol *tree-or-graph*))

(defparameter *depth-cutoff* 0
  "Negative or 0 means no cutoff.")
(declaim (fixnum *depth-cutoff*))

;(defparameter *max-states* 100000
;  "If this number is exceeded, hash table resizing may slow graph search.")
;(declaim (fixnum *max-states*))

(defparameter *progress-reporting-interval* 200000
  "Print progress during search after each multiple n of states examined.")
(declaim (fixnum *progress-reporting-interval*))

(defparameter *randomize-search* nil
  "Set to t or nil.")
(declaim ((or nil t) *randomize-search*))

(defvar *probe* nil
  "Inserts a probe to stop processing at a specific state.")
;Example probes:
;   Stops at specified node, for debugging given 
;   (<action name> <instantiations> <depth> &optional <count>)
;   (ww-set *probe* (wait (1 area4) 11))
;   (ww-set *probe* (pour (jug4 9 jug2 0 4) 5))
;   (ww-set *probe* (move (AREA1 AREA8) 3 5))  ;problem-crater
;   (ww-set *probe* (pickup-connector (CONNECTOR3 AREA8) 4))
;   (ww-set *probe* (JUMP (1 3 LD) 4))
(declaim (list *probe*))

(defparameter *debug* (if (member :wouldwork-debug *features*) *debug* 0)
  "Set the debug level for subsequent runs.
    0 - no debugging
    1 - display full search tree
    2 - display full search tree with states
    3 - display basic nodes
    4 - display full nodes
    5 - display full nodes + break after each expansion cycle")
(declaim (fixnum *debug*))



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
  "Initial database of dynamic db relations.")
(declaim (hash-table *db*))

(defparameter *hdb* (make-hash-table :test #'equal)
  "Initial database of dynamic hdb relations.")
(declaim (hash-table *hdb*))

(defparameter *idb* (make-hash-table)
  "Initial integer database of dynamic idb propositions.")
(declaim (hash-table *idb*))

(defparameter *hidb* (make-hash-table)
  "Initial integer database of dynamic hidb propositions.")
(declaim (hash-table *hidb*))

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

(defparameter *hap-db* (make-hash-table :test #'equal)
  "Initial database of happenings propositions.")
(declaim (hash-table *hap-db*))

(defparameter *hap-idb* (make-hash-table)
  "Initial integer database of happenings propositions.")
(declaim (hash-table *hap-idb*))

(defparameter *goal* nil
  "Holds the goal test function--value is a lambda expression,
   symbol-function is the function.")
(declaim (list *goal*))

(defparameter *constraint* nil
  "Any constraint test function.")
(declaim (list *constraint*))

(defparameter *last-object-index* 0
  "Last index of object constants seen so far in propositions.")
(declaim (fixnum *last-object-index*))

(defparameter *objective-value-p* nil
  "Does the variable $objective-value appear in an action rule.")
(declaim ((or nil t) *objective-value-p*))

(defparameter *eff-param-vars* nil
  "Make eff-param-vars available in translate-assert.")
(declaim (list *eff-param-vars*))

(defparameter *solutions* nil)
  ;The resulting list of solutions found.
(declaim (list *solutions*))

(defparameter *unique-solutions* nil)
  ;The culled list of unique solutions.
(declaim (list *unique-solutions*))

(defparameter *best-states* nil)
  ;The states having the best results found so far for min-value,max-value.
(declaim (list *best-states*))

(defparameter *unique-best-states* nil)
  ;The states having the best results found so far for min-value,max-value.
(declaim (list *unique-best-states*))

(defparameter *upper-bound* 1000000.0)
  ;The current upper bound if bounds are being calculated.
(declaim (real *upper-bound*))

(defparameter *cost* 0.0)
  ;The memoized cost bound for left search tree expansions. 
(declaim (real *cost*))

(defparameter *upper* 0.0)
  ;The memoized upper bound for left search tree expansions.
(declaim (real *upper*))

(defvar *state-codes* (make-hash-table)
  "Holding place for integer state codes in bi-directional search.")
(declaim (hash-table *state-codes*))

