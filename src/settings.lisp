;;; Filename: settings.lisp

;;; Default settings for planning program.

(in-package :ww)

;Note: It is necessary to close & reopen the lisp environment after
;      changing from nonparallel to parallel, or parallel to nonparallel.
(defparameter *num-parallel-threads* 0
  "Set the number of parallel threads to use.
    0 means no parallelism (ie, serial processing)
    1 means use one parallel thread
      (in addition to parallel management, effectively serial, useful for debugging)
    2 means two or more parallel processing threads
    N up to the number of available CPU threads")
(declaim (fixnum *num-parallel-threads*))


(setq *print-right-margin* 140)
;Allows non-wrap printing of *search-tree* for deep trees.


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

(defparameter *max-states* 100000
  "If this number is exceeded, hash table resizing may slow graph search.")
(declaim (fixnum *max-states*))

(defparameter *progress-reporting-interval* 100000
  "Print progress during search after each multiple n of states examined.")
(declaim (fixnum *progress-reporting-interval*))

(defparameter *randomize-search* nil
  "Set to t or nil.")
(declaim ((or nil t) *randomize-search*))

(defparameter *probe* nil
  "Inserts a probe to stop processing at a specific state.")
;Example probes:
;   Stops at specified node, for debugging given <action name> <instantiations> <depth>
;   (wait (1 area4) 11)
;   (pour (jug4 9 jug2 0 4) 5)
;   (move (AREA1 AREA8) 3)  ;problem-crater
;   (pickup-connector (CONNECTOR3 AREA8) 4)
;   (JUMP (1 3 LD) 4)
(declaim (list *probe*))

(defparameter *debug* 0
  "Set the debug level for subsequent runs.
    0 - no debugging
    1 - display full search tree
    2 - display full search tree with states
    3 - display basic nodes
    4 - display full nodes
    5 - display full nodes + break after each expansion cycle")
(declaim (fixnum *debug*))


(defmacro ww-set (parameter value)
  ;Allows resetting of user parameters after loading.
  (case parameter
    ((*problem* *depth-cutoff* *max-states* *progress-reporting-interval* *randomize-search*)
       `(setq ,parameter ',value))
    (*solution-type* 
       `(progn (setq *solution-type* ',value)
               (setf (symbol-function 'process-successor-graph)
                   (compile nil (function-lambda-expression #'process-successor-graph)))
               (setf (symbol-function 'process-successor-tree)
                   (compile nil (function-lambda-expression #'process-successor-tree)))
               (setf (symbol-function 'detect-goals)
                 (compile nil (function-lambda-expression #'detect-goals)))
               ',value))
    (*debug*
       `(progn (setq *debug* ',value)
               (setf (symbol-function 'df-bnb1)
                 (compile nil (function-lambda-expression #'df-bnb1)))
               (setf (symbol-function 'process-successor-graph)
                 (compile nil (function-lambda-expression #'process-successor-graph)))
               (setf (symbol-function 'process-successor-tree)
                 (compile nil (function-lambda-expression #'process-successor-tree)))
               (setf (symbol-function 'generate-new-node)
                 (compile nil (function-lambda-expression #'generate-new-node)))
               (setf (symbol-function 'close-barren-nodes)
                 (compile nil (function-lambda-expression #'close-barren-nodes)))
               (setf (symbol-function 'pop-discontinued-node)
                 (compile nil (function-lambda-expression #'pop-discontinued-node)))
               (setf (symbol-function 'register-solution)
                 (compile nil (function-lambda-expression #'register-solution)))
               (setf (symbol-function 'amend-happenings)
                 (compile nil (function-lambda-expression #'amend-happenings)))
               (setf (symbol-function 'generate-children)
                 (compile nil (function-lambda-expression #'generate-children)))
               (setf (symbol-function 'get-new-states)
                 (compile nil (function-lambda-expression #'get-new-states)))
               (setf (symbol-function 'update-search-tree)
                 (compile nil (function-lambda-expression #'update-search-tree)))
               (setf (symbol-function 'process-followup-updates)
                 (compile nil (function-lambda-expression #'process-followup-updates)))
               (setf (symbol-function 'commit1)
                 (compile nil (function-lambda-expression #'commit1)))
               ',value))
    (*tree-or-graph*
       `(progn (setq *tree-or-graph* ',value)
               (setf (symbol-function 'process-nongoal-succ-states)
                 (compile nil (function-lambda-expression #'process-nongoal-succ-states)))
               ',value))
    (*probe*
       `(progn (setq *probe* ',value)
               (ww-set *debug* 0)
               (setq *counter* 1)
               ',value))
    (*num-parallel-threads*
       `(progn (format t "~%*num-parallel-threads* cannot be changed with ww-set.")
               (format t "~%Instead, set its value in the file settings.lisp, and then recompile.~2%")))
    (otherwise
       (format t "~%~A is not a valid parameter name in ww-set.~%" parameter))))


(fmakunbound 'heuristic?)
(fmakunbound 'prune?)
(fmakunbound 'get-best-relaxed-value?)


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
  "Holds the goal test function--value is a lambda expression, symbol-function is the function.")
(declaim (list *goal*))

(defparameter *constraint* nil
  "Any constraint test function.")
(declaim (list *constraint*))

(defparameter *last-object-index* 0
  "Last index of object constants seen so far in propositions.")
(declaim (fixnum *last-object-index*))

(defparameter *solutions* nil)
  ;The resulting list of solutions found.
(declaim (list *solutions*))

(defvar *state-codes* (make-hash-table)
  "Holding place for integer state codes in bi-directional search.")
(declaim (hash-table *state-codes*))
