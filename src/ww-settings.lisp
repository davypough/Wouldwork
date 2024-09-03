;;; Filename: ww-settings.lisp

;;; Default settings for planning program.

(in-package :ww)


;Note: It is necessary to close & reopen the lisp environment after
;      changing here from nonparallel to parallel, or parallel to nonparallel.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *threads* 0
    "The number of parallel threads to use.
      0 means no parallelism (ie, serial processing)
      1 means use one parallel thread
        (in addition to parallel management, effectively serial, useful for debugging)
      2 means two or more parallel processing threads
      N up to the number of available CPU threads"))
(declaim (type fixnum *threads*))


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
(declaim (type list *probe*))


(defvar *debug* 0
  "Set the debug level for subsequent runs.
    0 - no debugging
    1 - display full search tree
    2 - display full search tree with states
    3 - display basic nodes
    4 - display full nodes
    5 - display full nodes + break after each expansion cycle")
(declaim (type fixnum *debug*))


(defparameter *lock* (bt:make-lock))  ;for thread protection


(if (> *threads* 0)
  (setf *debugger-hook* #'(lambda (condition original-hook)
                            (declare (ignore original-hook))
                            (bt:with-lock-held (*lock*)
                              (sb-debug:print-backtrace)
                              (format *error-output* "~%~A~2%" condition)
                              (finish-output *error-output*))
                            (abort)))
  (setf *debugger-hook* nil))


(defun eql* (&rest arguments)
  (every #'eql arguments (rest arguments)))
;(declaim (type ftype (function (&rest list) boolean) eql*))


(setq *print-right-margin* 140)
;Allows non-wrap printing of *search-tree* for deep trees.


(defmacro define-global (var-name val-form &optional doc-string)
  `,(if (> *threads* 0)
      (if (boundp var-name)
        `(setf ,var-name ,val-form)
        `(sb-ext:defglobal ,var-name ,val-form ,doc-string))
      `(defparameter ,var-name ,val-form ,doc-string)))


(defmacro increment-global (var-name &optional (delta-form 1))
  `(progn (declaim (type fixnum ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-incf ,var-name ,delta-form)
        `(incf ,var-name ,delta-form))))


(defmacro push-global (item var-name)
  `(progn (declaim (type list ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-push ,item ,var-name)
        `(push ,item ,var-name))))


(defmacro pop-global (var-name)
  `(progn (declaim (type list ,var-name))
     ,(if (> *threads* 0)
        `(sb-ext:atomic-pop ,var-name)
        `(pop ,var-name))))


(defun reset-globals (symbols)
  (dolist (symbol symbols)
    (unintern symbol)))


(reset-globals '(goal-fn constraint-fn heuristic? prune? bounding-function?))
;Reset certain user defined functions, when defined on previous load.

#|
(when (boundp '*query-names*)
  (reset-globals *query-names*))
(when (boundp '*update-names*)
  (reset-globals *update-names*))
(when (boundp '*actions*)
  (reset-globals (mapcar #'action.pre-defun-name *actions*))
  (reset-globals (mapcar #'action.eff-defun-name *actions*)))
(when (boundp '*init-actions*)
  (reset-globals (mapcar #'action.pre-defun-name *init-actions*))
  (reset-globals (mapcar #'action.eff-defun-name *init-actions*)))
|#

;;;;;;;;;;;;;;;;;;;; Global Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-global *troubleshoot-current-node* nil
  "A flag telling wouldwork to redo the current node for debugging.")
(declaim (type boolean *troubleshoot-current-node*))

(define-global *branch* -1
  "If n>0, explore only the nth branch from the *start-state*.")
(declaim (type fixnum *branch*))

(define-global *counter* 1
  "For misc debugging with probe function")
(declaim (type fixnum *counter*))

(define-global *-* '---------------------------------------------------------
  "Division marker for debugging printout convenience.")
(declaim (type symbol *-*))

(define-global *solution-count* 0
  "Holds the total number of solutions found following search.")
(declaim (type fixnum *solution-count*))

(define-global *num-idle-threads* 0
  "Holds the number of currently idle threads (shared).")
(declaim (type fixnum *num-idle-threads*))

(define-global *total-states-processed* 0
  "Count of states either newly generated, updated, or regenerated while searching (shared).")
(declaim (type fixnum *total-states-processed*))

(define-global *prior-total-states-processed* 0
  "Count of states produced since last progress printing (shared).")
(declaim (type fixnum *prior-total-states-processed*))

(define-global *prior-time* 0
  "Time since last progress printing (shared).")
(declaim (type fixnum *prior-time*))

(define-global *best-states* nil
  "Holds the best states encountered during a graph search.")
(declaim (type list *best-states*))

(define-global *repeated-states* 0
  "Count of the repeated states during a graph search.")
(declaim (type fixnum *repeated-states*))

(define-global *program-cycles* 0
 "Count of complete cycles of searching (shared).")
(declaim (type fixnum *program-cycles*))

(define-global *max-depth-explored* 0
  "Keeps track of the maximum depth reached so far during the search (shared).")
(declaim (type fixnum *max-depth-explored*))

(define-global *accumulated-depths* 0
  "Sums the final depths of all terminated paths so far.")
(declaim (type fixnum *accumulated-depths*))

(define-global *num-paths* 0
  "Tracks the total number of paths explored so far.")
(declaim (type fixnum *num-paths*))

(define-global *num-init-successors* 0
  "The number of branches completed so far from the start state.")
(declaim (type fixnum *num-init-successors*))

(define-global *rem-init-successors* nil
  "Holds the remaining initial branch nodes from the start state.")
(declaim (type list *rem-init-successors*))

(define-global *solutions* nil
  "Holds the solutions found during search.")
(declaim (type list *solutions*))

(define-global *average-branching-factor* 0.0
  "Average branching factor so far during search (shared).")
(declaim (type single-float *average-branching-factor*))

(define-global *search-tree* nil
  "DFS search tree for debugging (serial processing only).")
(declaim (type list *search-tree*))

(define-global *start-time* 0
  "Stores time at beginning of the search.")
(declaim (type (integer 0 4611686018427387903) *start-time*))

(define-global *problem* 'unspecified
  "Name of the current problem, assigned in problem.lisp by user.")
(declaim (type symbol *problem*))

(define-global *problem-type* 'planning
  "Spedify whether it's a planning problem or constraint satisfaction problem.")
(declaim (type symbol *problem-type*))

(define-global *solution-type* 'first
  "Specify whether to search for 'first, 'min-length, 'min-time, or 'every solution.")
(declaim (type symbol *solution-type*))

(define-global *tree-or-graph* 'graph
  "Whether there are repeated states (graph) or not (tree); try both.")
(declaim (type symbol *tree-or-graph*))

(define-global *depth-cutoff* 0
  "Negative or 0 means no cutoff.")
(declaim (type fixnum *depth-cutoff*))

(define-global *progress-reporting-interval* 200000
  "Print progress during search after each multiple n of states examined.")
(declaim (type fixnum *progress-reporting-interval*))

(define-global *randomize-search* nil
  "Set to t or nil.")
(declaim (type (member nil t) *randomize-search*))

(define-global *types* (make-hash-table :test #'eq)
  "Table of all types.")
(declaim (type hash-table *types*))

(define-global *relations* (make-hash-table :test #'eq)
  "Dynamic relations.")
(declaim (type hash-table *relations*))

(define-global *static-relations* (make-hash-table :test #'eq)
  "Static relations.")
(declaim (type hash-table *static-relations*))

(define-global *connectives* '(and or not)
  "Logical connectives.")
(declaim (type list *connectives*))

(define-global *symmetrics* (make-hash-table :test #'eq)
  "Symmetric relations.")
(declaim (type hash-table *symmetrics*))

(define-global *complements* (make-hash-table :test #'eq)
  "Table of complement relations.")
(declaim (type hash-table *complements*))

(define-global *fluent-relation-indices* (make-hash-table :test #'eq)
  "List of fluent argument indices for a relation.")
(declaim (type hash-table *fluent-relation-indices*))

(define-global *db* (make-hash-table :test #'equal)
  "Initial database of dynamic db relations.")
(declaim (type hash-table *db*))

(define-global *hdb* (make-hash-table :test #'equal)
  "Initial database of dynamic hdb relations.")
(declaim (type hash-table *hdb*))

(define-global *idb* (make-hash-table)
  "Initial integer database of dynamic idb propositions.")
(declaim (type hash-table *idb*))

(define-global *hidb* (make-hash-table)
  "Initial integer database of dynamic hidb propositions.")
(declaim (type hash-table *hidb*))

(define-global *constant-integers* (make-hash-table)
  "Integer codes for the problem's object constants.")
(declaim (type hash-table *constant-integers*))

(define-global *integer-constants* (make-hash-table)
  "Translating codes back to constants for printout.")
(declaim (type hash-table *integer-constants*))

(define-global *min-action-duration* 0.0
  "The least action duration among all actions.")
(declaim (type real *min-action-duration*))

(define-global *query-names* nil
  "List of all user-defined query functions.")
(declaim (type list *query-names*))

(define-global *update-names* nil
  "List of all user-defined update functions.")
(declaim (type list *update-names*))

(define-global *actions* nil
  "List of all potential actions.")
(declaim (type list *actions*))

(define-global *init-actions* nil
  "List of all initialization actions.")
(declaim (type list *init-actions*))

(define-global *happening-names* nil
  "The list of objects having exogenous events.")
(declaim (type list *happening-names*))

(define-global *static-db* (make-hash-table :test #'equal)
  "Initial database of static propositions.")
(declaim (type hash-table *static-db*))

(define-global *static-idb* (make-hash-table)
  "Initial integer database of static propositions.")
(declaim (type hash-table *static-idb*))

(define-global *hap-db* (make-hash-table :test #'equal)
  "Initial database of happenings propositions.")
(declaim (type hash-table *hap-db*))

(define-global *hap-idb* (make-hash-table)
  "Initial integer database of happenings propositions.")
(declaim (type hash-table *hap-idb*))

(define-global *last-object-index* 0
  "Last index of object constants seen so far in propositions.")
(declaim (type fixnum *last-object-index*))

(define-global *objective-value-p* nil
  "Does the variable $objective-value appear in an action rule.")
(declaim (type (member nil t) *objective-value-p*))

(define-global *eff-param-vars* nil
  "Make eff-param-vars available in translate-assert.")
(declaim (type list *eff-param-vars*))

(define-global *unique-solutions* nil)
  ;The culled list of unique solutions.
(declaim (type list *unique-solutions*))

(define-global *upper-bound* 1000000.0)
  ;The current upper bound if bounds are being calculated.
(declaim (type real *upper-bound*))

(define-global *cost* 0.0)
  ;The memoized cost bound for left search tree expansions. 
(declaim (type real *cost*))

(define-global *upper* 0.0)
  ;The memoized upper bound for left search tree expansions.
(declaim (type real *upper*))

(defvar *state-codes* (make-hash-table)
  "Holding place for integer state codes in bi-directional search.")
(declaim (type hash-table *state-codes*))

(define-global *parameter-headers* '(standard product combination dot-product)
  "The different ways values can be combined in a pre-parameter list.")
(declaim (type list *parameter-headers*))
