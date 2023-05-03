;;; Filename:  ww-searcher.lisp

;;; Nonstandard Depth First Branch & Bound. Optional duplicate checking for nodes
;;; previously visited (graph search) as specified in problem spec. Open nodes are
;;; kept in an indexed stack and (optionally for graph search) visited nodes in a
;;; hash table. Search
;;; follows the scheme described in "Ch 3&4 Artificial Intelligence: A Modern
;;; Approach" by Russell & Norvig, p87 (with closing of barren nodes),
;;; but keeps nodes on OPEN until the search skeleton is pruned (either dropped for
;;; tree search or to VISITED for graph search.
;;; A complete path from a goal to the start state is then available in OPEN.
;;; Searches up to a user-specified depth cutoff.  In graph search a visited node is
;;; either in OPEN or VISITED at any particular time. A user-defined heuristic can be
;;; used to expand the best states first at each level (beam search), but
;;; the entire search graph may ultimately be searched (completeness).


(in-package :ww)


(defstruct (node (:conc-name node.)
             (:print-function
               (lambda (node stream depth)
                 ;Prints out a node. Used for debugging.
                 (declare (ignore depth) (node node) (stream stream))
                 (format stream "~&NODE: STATE=~A DEPTH=~:D"   ;PARENT=~S~%"
                   (node.state node) (node.depth node)))))
  (state (make-problem-state) :type problem-state)    ;problem state
  (depth 0 :type fixnum)           ;depth in the search tree
  (parent nil :type (or null node)))  ;this node's parent


#+:sbcl (sb-ext:define-hash-table-test state-equal-p state-equal-p-hash)
;Hash stack test for *open*.
(declaim (function state-equal-p state-equal-p-hash))


(defparameter *lock* (bt:make-lock))  ;for debugging


(defun protect (fn &rest args)
  "If multi-threading, protect a function with a lock."
  (if (> *threads* 0)
    (bt:with-lock-held (*lock*)
      (apply fn args)
      (finish-output))
    (apply fn args)))


(defmacro lprt (&rest vars)
  "Print some variable values in a thread for debugging."
  `(bt:with-lock-held (*lock*)  ;grab lock for uninterrupted printing
     (let ((*package* (find-package :ww))  ;suppress printing package prefixes
           (thread-index  (lparallel:kernel-worker-index)))  ;get current thread
       ;(with-open-file (s "D:\\output.lisp" :direction :output
       ;            :if-exists :supersede)
       ;  (format s "The value of *standard-output* is ~S~%" *standard-output*)
       ;  (format s "The value of *terminal-io* is ~S~%" *terminal-io*))
       (terpri)
       (ut::prt thread-index)
       (ut::prt ,@vars)
       (finish-output))))  ;make sure printout is complete before continuing


(defparameter *counter* 1)  ;for debugging


;;;;;;;;;;;;;;;;;;; Shared Global Update Macros ;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro increment-global-fixnum (var-name &optional (delta-form 1))
  (if (> *threads* 0)
    #+sbcl `(sb-ext:atomic-incf ,var-name ,delta-form)
    #+allegro `(excl:incf-atomic ,var-name ,delta-form)
    `(incf ,var-name ,delta-form)))


(defmacro push-global (item var-name)
  (if (> *threads* 0)
    #+sbcl `(sb-ext:atomic-push ,item ,var-name)
    #+allegro `(excl:push-atomic ,item ,var-name)
    `(push ,item ,var-name)))


(defmacro pop-global (var-name)
  (if (> *threads* 0)
    #+sbcl `(sb-ext:atomic-pop ,var-name)
    #+allegro `(excl:push-atomic ,var-name)
    `(pop ,var-name)))


(defmacro update-branching-factor (n)
  (if (> *threads* 0)
    #+sbcl `(sb-ext:atomic-update *average-branching-factor* #'compute-average-branching-factor ,n)
    #+allegro `(let ((abf (compute-average-branching-factor ,n 0.0)))
                 (excl:update-atomic (*average-branching-factor* *average-branching-factor*) abf))  
    `(setf *average-branching-factor* (compute-average-branching-factor ,n 0))))
       

(defparameter *-* '---------------------------------------------------------)
  ;Division marker for debugging printout convenience.


;;;;;;;;;;;;;;;;;;;; Search Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-global-fixnum (var-name val-form &optional doc-string)
  `(progn (declaim (fixnum ,var-name))
     ,(if (> *threads* 0)
       #+sbcl (when (not (boundp var-name))
                `(sb-ext:defglobal ,var-name ,val-form ,doc-string))
       #+allegro `(defparameter ,var-name ,val-form ,doc-string)
        `(defparameter ,var-name ,val-form ,doc-string))))


(define-global-fixnum *solution-count* 0
  "Holds the total number of solutions found so far (shared).")

(define-global-fixnum *num-idle-threads* 0
  "Holds the number of currently idle threads (shared).")

(define-global-fixnum *total-states-processed* 0
  "Count of states either newly generated, updated, or regenerated while searching (shared).")

(define-global-fixnum *prior-total-states-processed* 0
  "Count of states produced since last progress printing (shared).")

(define-global-fixnum *prior-time* 0
  "Time since last progress printing (shared).")

(define-global-fixnum *repeated-states* 0
  "Count of the repeated states during a graph search.")

;(define-global-fixnum *unique-states-encountered-graph* 0
;  "Count of unique states encountered, only used during graph search (shared)")

(define-global-fixnum *program-cycles* 0
 "Count of complete cycles of searching (shared).")

;(declaim (fixnum *program-cycles*))
;(sb-ext:defglobal *program-cycles* 0
;  "Count of complete cycles of searching (shared).")

(define-global-fixnum *max-depth-explored* 0
  "Keeps track of the maximum depth reached so far during the search (shared).")

(define-global-fixnum *accumulated-depths* 0
  "Sums the final depths of all terminated paths so far.")

(define-global-fixnum *num-paths* 0
  "Tracks the total number of paths explored so far.")

(define-global-fixnum *num-init-successors* 0
  "The number of branches completed so far from the start state.")

(defparameter *rem-init-successors* nil
  "Holds the remaining initial branch nodes from the start state.")
(declaim (list *rem-init-successors*))

(defparameter *average-branching-factor* 0.0
  "Average branching factor so far during search (shared).")
(declaim (single-float *average-branching-factor*))

(defparameter *search-tree* nil
  "DFS search tree for debugging (serial processing only).")
(declaim (list *search-tree*))

(defparameter *start-time* 0
  "Stores time at beginning of the search.")
(declaim ((integer 0 4611686018427387903) *start-time*))


(defparameter *open*
  (if (and (> *threads* 0) (alexandria:featurep :sbcl))
    (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node.state
                       :ht-test 'state-equal-p :synchronized t)
    (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node.state
                       :ht-test 'state-equal-p))
  "The hash-stack structure containing the set of open nodes, local to search-parallel.")
(declaim (hs::hstack *open*))


#+:sbcl (defparameter *visited*
          (if (eql *tree-or-graph* 'graph)
            (if (> *threads* 0)
              (make-hash-table :test 'state-equal-p :size 100000 :synchronized t)
              (make-hash-table :test 'state-equal-p :size 100000))
            (make-hash-table :size 0)))  ;unused null placeholder


#+:allegro (defparameter *visited*
             (when (eql *tree-or-graph* 'graph)
               (make-hash-table :test 'state-equal-p))
             "The hash-table containing the set of visited and closed nodes, global.")
(declaim (hash-table *visited*))


;;; Search Functions


(defun dfs ()
  "Main search program."
  (when (eql *tree-or-graph* 'graph)
    ;(clrhash *visited*)
    (if (> *threads* 0)
      (setf *visited*
            (make-hash-table :test 'state-equal-p :size 100000 :rehash-size 2.0 :synchronized t))
      (setf *visited*
            (make-hash-table :test 'state-equal-p :size 100000 :rehash-size 2.0))))
  (setf (gethash *start-state* *visited*) (list nil 0 0.0 0.0))  ;(dead depth time value)
;  (when (fboundp 'bounding-function?)
;    (setf *upper-bound*
;          (funcall 'bounding-function? *start-state*)))
  (setf *open*
    (if (and (> *threads* 0) (member :sbcl *features*))
      (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node.state
                         :ht-test 'state-equal-p :synchronized t)
      (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node.state
                         :ht-test 'state-equal-p)))
  (hs::push-hstack (make-node :state (copy-problem-state *start-state*)) *open*)
  (setf *program-cycles* 0)
  (setf *average-branching-factor* 0.0)
  (setf *total-states-processed* 1)
  (setf *prior-total-states-processed* 0)
  (setf *rem-init-successors* nil)  ;branch nodes from start state
  (setf *num-init-successors* 0)
  (setf *max-depth-explored* 0)
  (setf *num-idle-threads* 0)
  (setf *accumulated-depths* 0)
  (setf *repeated-states* 0)
  (setf *num-paths* 0)
  (setf *solutions* nil)
  (setf *unique-solutions* nil)
  (setf *best-states* (list *start-state*))
  ;(setf *unique-best-states* nil)
  (setf *solution-count* 0)
  (setf *upper-bound* 1000000)
  (setf *search-tree* nil)
  (setf *start-time* (get-internal-real-time))
  (setf *prior-time* 0)
  (if (> *threads* 0)
    ;(with-open-stream (*standard-output* (make-broadcast-stream)) ;ignore *standard-output*
    (process-threads)
    (search-serial))
  (let ((*package* (find-package :ww)))  ;avoid printing package prefixes
    (summarize-search-results (if (eql *solution-type* 'first)
                                'first
                                'exhausted))))


(defun search-serial ()
  "Branch & Bound DFS serial search."
  (progn (iter
    (for succ-nodes = (df-bnb1 *open*))  ;may modify *open* via close-barren-nodes
    (when (equal succ-nodes '(first))
      (return-from search-serial 'first))
    (when (hs::empty-hstack *open*)
      (leave))  ;terminate *open*
    (when succ-nodes  ;nongoal succ-nodes
      (if (fboundp 'heuristic?)
        (setf succ-nodes (sort succ-nodes #'>
                               :key (lambda (node)
                                      (problem-state.heuristic (node.state node)))))
        (when *randomize-search*
          (setf succ-nodes (alexandria:shuffle succ-nodes)))))
    (when (= *program-cycles* 1)  ;ie, expanding the start state
      (setf *num-init-successors* (length succ-nodes))
      (setf *rem-init-successors* (reverse succ-nodes)))
    (loop for succ-node in succ-nodes
          do (hs::push-hstack succ-node *open*))  ;push lowest heuristic value last
;    (terpri) (ut::prt *program-cycles* *num-init-successors* *rem-init-successors*)
;    (terpri) (print "*open*") (hs::print-hstack *open* t 1)
    (after-each #+:wouldwork-debug (when (>= *debug* 5) (break))))))


(defun df-bnb1 (open)
  "Performs expansion of one node from open. Returns
   new successor nodes, (first), or nil if no new nodes generated."
  (declare (hs::hstack open))
  #+:wouldwork-debug 
    (when (>= *debug* 3)
      (format t "~&-----------------------------------------------------------~%"))
  (let ((current-node (get-next-node-for-expansion open)))
    (when *probe*
      (apply #'probe current-node *probe*))
    #+:wouldwork-debug (when (>= *debug* 3)
                         (format t "~%Current node selected:~%~S~2%" current-node))
    (when (null current-node)  ;open is empty
      (return-from df-bnb1 nil))
    (when (fboundp 'bounding-function?)
        (multiple-value-bind (current-cost current-upper)
                             (funcall (symbol-function 'bounding-function?) (node.state current-node))
         #+:wouldwork-debug (when (>= *debug* 3)
                              (format t "~&Cost bound = ~A, Upper bound = ~A~%" current-cost current-upper))
         (cond ((> current-cost *upper-bound*)
                  #+:wouldwork-debug (when (>= *debug* 1)
                                       (update-search-tree (node.state current-node) (node.depth current-node)
                                                           "State killed by bounding"))
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "~&State killed by bounding")
                                       (format t "~&current-cost = ~F > *upper-bound* = ~F~%" current-cost *upper-bound*))
                  (protect #'format t "~%bounding a state~%")
                  (close-barren-nodes current-node open)
                  (finalize-path-depth (node.depth current-node))
                  (return-from df-bnb1 nil))
               ((< current-upper *upper-bound*)
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "~&Updating *upper-bound* from ~F to ~F~%" *upper-bound* current-upper))
                  (setf *upper-bound* current-upper)))))
    (let ((succ-states (get-successors current-node)))  ;from generate-children
      ;(when (>= *debug* 4)
      ;  (format t "~%ALL SUCCESSORS~%  ~S" succ-states))
      (update-max-depth-explored (1+ (node.depth current-node)))
      (when (null succ-states)  ;no successors
        #+:wouldwork-debug (when (>= *debug* 3)
                             (format t "~%No successor states"))
        #+:wouldwork-debug (when (>= *debug* 1)
                             (update-search-tree (node.state current-node) (node.depth current-node)
                                                 "No successor states"))
        (close-barren-nodes current-node open)
        (finalize-path-depth (node.depth current-node))
        (return-from df-bnb1 nil))
      (ut::mvb (goal-succ-states nongoal-succ-states) (detect-goals current-node succ-states)
         ;(when (>= *debug* 3)
         ;   (format t "~%GOAL SUCCESSOR STATES~%  ~S" goal-succ-states)
         ;   (format t "~&NONGOAL SUCCESSOR STATES~%  ~S" nongoal-succ-states))
         (when goal-succ-states
           #+:wouldwork-debug (when (>= *debug* 1)
                                (update-search-tree (node.state current-node) (node.depth current-node) "")
                                (iter (for state in goal-succ-states)
                                      (update-search-tree state (1+ (node.depth current-node)) "***goal***")))
           (finalize-path-depth (1+ (node.depth current-node)))
           (when (and goal-succ-states (eql *solution-type* 'first))
             (return-from df-bnb1 '(first))))
         #+:wouldwork-debug (when (>= *debug* 1)
                              (when nongoal-succ-states
                                (update-search-tree (node.state current-node) (node.depth current-node) "")))
         (when (and (null goal-succ-states) (null nongoal-succ-states))
           #+:wouldwork-debug (when (>= *debug* 1)
                                (update-search-tree (node.state current-node) (node.depth current-node)
                                                    "No viable successor states"))
           (close-barren-nodes current-node open)
           (return-from df-bnb1 nil))
         ;nongoal successors will be updated on subsequent get-current-node
         (let ((succ-nodes (process-nongoal-succ-states current-node nongoal-succ-states)))  ;analyze in successors.lisp
            (when (null succ-nodes)
              (if (and goal-succ-states (eql *tree-or-graph* 'graph))
                (pop-discontinued-node open)
                (close-barren-nodes current-node open)))    
            (increment-global-fixnum *program-cycles* 1)
            (update-branching-factor (length succ-nodes))
            (protect #'print-search-progress)       ;#nodes expanded so far
            (return-from df-bnb1 (nreverse succ-nodes)))))))


(defun probe (current-node name instantiations depth &optional (count 1))
  "Breaks when the current node matches action name, instantiations, depth, and count."
  (declare (node current-node))  ;(ut::prt name instantiations depth current-node) (break)
  (let ((state (node.state current-node)))
    (when (and (eql (problem-state.name state) name)
               (equal (problem-state.instantiations state) instantiations)
               (= (node.depth current-node) depth))
      (if (= count *counter*)
        (setq *debug* 5)
        (incf *counter*)))))



(defun update-max-depth-explored (succ-depth)
  (when (> succ-depth *max-depth-explored*)
    (increment-global-fixnum *max-depth-explored* (- succ-depth *max-depth-explored*))))


(defun get-next-node-for-expansion (open)
  "Returns the node at the top of open."
  (declare (hs::hstack open))
  (hs::peek-hstack open))  ;top of stack


(defun get-successors (current-node)
  "Returns children states of current node to df-bnb1."
  (declare (node current-node))
  (expand (node.state current-node)))


(defun compute-average-branching-factor (n prior-value)
  "Cumulative average of expanded states."
  (declare (fixnum n) (ignore prior-value))
  (/ (+ n (* *program-cycles* *average-branching-factor*))
     *program-cycles*))


(defun on-current-path (succ-state current-node)
  "Determines if a successor is already on the current path from the start state."
  (iter (for node initially current-node then (node.parent node))
        (while node)  
        (thereis (state-equal-p succ-state (node.state node)))))
          

(defun generate-new-node (current-node succ-state)
  "Produces a new node for a given successor."
  (declare (node current-node) (problem-state succ-state))
  (let ((succ-node (make-node :state succ-state
                              :depth (1+ (node.depth current-node))
                              :parent current-node)))
    #+:wouldwork-debug (when (>= *debug* 3)
                         (format t "~%Installing new or updated successor:~%~S~%" succ-node))
    (when (eql *tree-or-graph* 'graph)
      (setf (gethash succ-state *visited*) (list nil  ;whether dead or not, initially live
                                                 (1+ (node.depth current-node))
                                                 (problem-state.time succ-state)
                                                 (problem-state.value succ-state))))
    succ-node))


(defun close-barren-nodes (current-node open)
  "Remove nodes from open & close, unless current-node has goal succ."
  (declare (node current-node) (hs::hstack open))
  (do ((node current-node) parent)
      ((or (null node) (not (eq node (hs::peek-hstack open)))))
    (hs::pop-hstack open)
    (when (eql *tree-or-graph* 'graph)
      (setf (first (gethash (node.state node) *visited*)) t))  ;close dead node
    (setq parent (node.parent node))
    (setf (node.parent node) nil)
    #+:wouldwork-debug (when (>= *debug* 4)
                         (format t "~2%Closing barren node:~%~S~%" node))
    (setq node parent)))


(defun pop-discontinued-node (open)
  "Discontinue node if all successors are goals."
  (let ((node (hs::pop-hstack open)))
     (declare (ignorable node))
     #+:wouldwork-debug  (when (>= *debug* 4)
                           (format t "~2%Popping discontinued node:~%~S~%" node))
     (when (= (hs::length-hstack open) 1)  ;last node on open = start node
        (close-barren-nodes (hs::pop-hstack open) open))))


(defun update-search-tree (state depth message)
  (declare (problem-state state) (fixnum depth) (string message))
  (when (and (not (> *threads* 0)) (>= *debug* 1))
    (push `((,(problem-state.name state) 
             ,@(problem-state.instantiations state))
           ,depth
           ,message
           ,@(case *debug*
               (1 nil)
               (2 (list (list-database (problem-state.idb state))
                        (list-database (problem-state.hidb state))))))
          *search-tree*)))


(defun at-max-depth (succ-depth)
  "Determines if installing a nongoal successor to the current node will be
   pointless, based on it being at the max allowable depth."
  (declare (fixnum succ-depth))
  (let ((depth-cutoff *depth-cutoff*))
    (and (> depth-cutoff 0)
         (= succ-depth depth-cutoff))))


(defun best-states-last (state1 state2)
  "Used to sort a list of expanded states according to the user-defined heuristic."
  (declare (problem-state state1 state2))
  (> (estimate-to-goal state1) (estimate-to-goal state2)))


(defun finalize-path-depth (depth)
  "Records the path depth of a path that terminates."
  (increment-global-fixnum *accumulated-depths* depth)
  (increment-global-fixnum *num-paths* 1))


;;; Solution Processing Functions


(defun record-solution-path (goal-node)
  "Recovers a path from a goal node back to the start node following parent links."
  (declare (node goal-node))
  (let ((path nil))
    (do ((n goal-node (node.parent n)))
        ((null (node.parent n)))
      (push (record-move (node.state (node.parent n)) (node.state n))
            path))
    (return-from record-solution-path path)))

  
;(defun unique-states-encountered-graph (open)
;  ;Count of how many unique states have been encountered during searching.
;  (+ (hs::length-hstack open)
;     (1- (hash-table-count *visited*))
;     (length *solutions*)))

   
(defun summarize-search-results (condition)
  (declare (symbol condition))
  (format t "~2%In problem ~A, performed ~A search for ~A solution."
            *problem* *tree-or-graph* *solution-type*)
  (ecase condition
    (first
      (when *solutions*
        (format t "~2%Search ended with first solution found." )))
    (exhausted
      (format t "~2%Search process completed normally.")
      (ecase *solution-type*
        ((every count)
          (format t "~2%Examined every state up to the depth cutoff."))
        ((min-length min-time max-value min-value)
          (format t "~2%Examined only worthwhile states up to the depth cutoff.")))))
  (format t "~2%Depth cutoff = ~:D" *depth-cutoff*)
  (format t "~2%Maximum depth explored = ~:D" *max-depth-explored*)
  (format t "~2%Total states processed = ~:D" *total-states-processed*)
  (when (eql *tree-or-graph* 'graph)
    (format t "~2%Repeated states = ~:D, ie, ~,1F percent"
              *repeated-states* (* (/ *repeated-states* *total-states-processed*) 100)))
  ;(format t "~2%Unique states encountered = ~:D" (unique-states-encountered-graph))
  (format t "~2%Program cycles (state expansions) = ~:D" *program-cycles*)
  (format t "~2%Average branching factor = ~,1F" *average-branching-factor*)
  (format t "~2%Start state:~%~A" (list-database (problem-state.idb *start-state*)))
  (format t "~2%Goal:~%~A~2%" (get '*goal* 'formula))
  (when (and (eql *solution-type* 'count)) (> *solution-count* 0)
    (format t "~%Total solution paths found = ~:D ~2%" *solution-count*))
  (when *solutions*  ;ie, recorded solutions
    (let* ((shallowest-depth (reduce #'min *solutions* :key #'solution.depth))
           (shallowest-depth-solution (find shallowest-depth *solutions* :key #'solution.depth))
           (minimum-time (reduce #'min *solutions* :key #'solution.time))
           (minimum-time-solution (find minimum-time *solutions* :key #'solution.time))
           (min-max-value-solution (first *solutions*))
           (min-max-value (solution.value min-max-value-solution)))
      (format t "~2%Total solution paths recorded = ~:D, of which ~:D is/are unique solution paths" 
                (length *solutions*) (length *unique-solutions*))
      (format t "~%Check *solutions* and *unique-solutions* for solution records.")
      (case *solution-type*
        (first
          (format t "~2%Number of steps in first solution found: = ~:D" shallowest-depth)
          (format t "~2%Duration of first solution found = ~:D" minimum-time)
          (format t "~2%Solution path of first solution found from start state to goal state:~%")
          (printout-solution shallowest-depth-solution))
        (min-length
          (format t "~2%Number of steps in a minimum path length solution = ~:D" shallowest-depth)
          (format t "~2%A minimum length solution path from start state to goal state:~%")
          (printout-solution shallowest-depth-solution))
        (min-time
          (format t "~2%Duration of a minimum time solution = ~:D" minimum-time)
          (format t "~2%A minimum time solution path from start state to goal state:~%")
          (printout-solution minimum-time-solution))
        (min-value
          (format t "~2%Value of a minimum value solution = ~:D" min-max-value)
          (format t "~2%A minimum value solution path from start state to goal state:~%")
          (printout-solution min-max-value-solution))
        (max-value
          (format t "~2%Value of a maximum value solution = ~:D" min-max-value)
          (format t "~2%A maximum value solution path from start state to goal state:~%")
          (printout-solution min-max-value-solution))
        (every
          (format t "~2%Number of steps in a minimum path length solution = ~:D" shallowest-depth)
          (format t "~2%A minimum length solution path from start state to goal state:~%")
          (printout-solution shallowest-depth-solution)
          (cond ((equalp shallowest-depth-solution minimum-time-solution)
                   (format t "~%A shortest path solution is also a minimum duration solution.~2%"))
                (t (format t "~2%Duration of a minimum time solution = ~:D" minimum-time)
                   (format t "~2%A minimum time solution path from start state to goal state:~%")
                   (printout-solution minimum-time-solution)))))))
  (if (get '*goal* 'formula)
    (when (or (and (eql *solution-type* 'count) (= *solution-count* 0))
              (and (not (eql *solution-type* 'count)) (null *solutions*)))
      (format t "~&No solutions found."))
    (format t "~&No solutions requested."))
  (when (null (get '*goal* 'formula))
    ;(setf *unique-best-states*
    ;  (remove-duplicates *best-states* :test #'state-equal-p))
    (format t "~2%Total number of results recorded = ~:D." (length *best-states*))
    (format t "~%Check *best-states* for result records.")
    (case *solution-type*
        (min-value
          (format t "~2%The minimum objective value found = ~:D" (problem-state.value (car *best-states*)))
          (format t "~2%A minimum value state:~%")
          (print-problem-state (car *best-states*))
          (format t "~%"))
        (max-value
          (format t "~2%The maximum objective value found = ~:D" (problem-state.value (car *best-states*)))
          (format t "~2%A maximum value state:~%")
          (print-problem-state (car *best-states*))
          (format t "~%")
          (finish-output nil))))
  (print-search-tree))


(defun print-search-tree ()
  (when (and (not (> *threads* 0)) (or (= *debug* 1) (= *debug* 2)))
    (format t "~2%Search tree:~%")
    (loop for act in (reverse *search-tree*)
          do (if (alexandria:length= 2 act)
               (format t "~vT~d:~a~%" (* 4 (second act)) (second act) (first act))
               (case *debug*
                 (1 (format t "~vT~d:~a ~a~%"
                              (* 4 (second act)) (second act) (first act) (third act)))
                 (2 (format t "~vT~d:~a ~a~%" 
                              (* 4 (second act)) (second act) (first act) (third act))
                    (format t "~vT  ~a~%"
                              (* 4 (second act)) (fourth act))
                    (when (fifth act)
                      (format t "~vT  ~a~%"
                                (* 4 (second act)) (fifth act))))))
          finally (terpri))))

 
(defun register-solution (current-node goal-state)
  "Inserts a new solution on the list of *solutions*."
  (declare (node current-node) (problem-state goal-state))
  (let* ((state-depth (1+ (node.depth current-node)))
         (solution
           (make-solution
             :depth state-depth
             :time (problem-state.time goal-state)
             :value (problem-state.value goal-state)
             :path (let ((nominal-path (append (record-solution-path current-node)
                                               (list (record-move (node.state current-node)
                                                     goal-state)))))
                     (if (= (hash-table-count *state-codes*) 0)  ;if in backward search
                       nominal-path
                       (append nominal-path 
                               (reverse (gethash (funcall 'encode-state 
                                                          (list-database (problem-state.idb goal-state)))
                                                 *state-codes*)))))
             :goal goal-state)))
    (cond ((> *threads* 0)
             #+:wouldwork-debug (when (>= *debug* 1)
                                  (lprt))
             (let ((ctrl-str "~&New path to goal found at depth = ~:D~%"))
               (bt:with-lock-held (*lock*)
                 (if (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
                   (format t (concatenate 'string ctrl-str "Objective value = ~:A~2%")
                             state-depth (solution.value solution))
                   (format t ctrl-str state-depth)))))
           (t (format t "~%New path to goal found at depth = ~:D~%" state-depth)
              (when (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
                (format t "Objective value = ~:A~%" (solution.value solution)))))
    #+:wouldwork-debug (when (>= *debug* 3)
                         (format t "~%New solution found:~%  ~A~%" solution))
    (push-global solution *solutions*)
    (when (not (member (problem-state.idb (solution.goal solution)) *unique-solutions* 
                       :key (lambda (soln) (problem-state.idb (solution.goal soln)))
                       :test #'equalp))
      (push-global solution *unique-solutions*))))


(defun printout-solution (soln)
  (declare (solution soln))
  (dolist (item (solution.path soln))
    (write item :pretty t)
    (terpri))
  (format t "~%Final state:~%~A~2%"
    (list-database (problem-state.idb (solution.goal soln)))))


(defun print-search-progress ()
  "Printout of nodes expanded so far during search modulo reporting interval."
    (when (and (> *program-cycles* 1)
               (>= (/ (the fixnum (- *total-states-processed* *prior-total-states-processed*))
                      *progress-reporting-interval*)
                   1))
        (format t "~%total states processed so far = ~:D" *total-states-processed*)
        (when (eql *tree-or-graph* 'graph)
          (format t "~%ht count: ~:D    ht size: ~:D"
                  (hash-table-count *visited*) (hash-table-size *visited*)))
        (format t "~%net average branching factor = ~:D" (round *average-branching-factor*))
        ;(when *rem-init-successors*
        (iter (while (and *rem-init-successors*
                          (not (hs::in-hstack (node.state (first *rem-init-successors*)) *open*))))
              (pop-global *rem-init-successors*))
        (format t "~%current progress: in #~:D of ~:D branches"
                  (the fixnum (- (1+ *num-init-successors*)
                                 (length *rem-init-successors*)))
                  *num-init-successors*)
        (format t "~%current average search depth = ~A"
                  (if (> *num-paths* 0)
                    (round (/ *accumulated-depths* *num-paths*))
                    'pending))
        (format t "~%current average processing speed = ~:D states/sec" 
                  (round (/ (the fixnum (- *total-states-processed* *prior-total-states-processed*))
                            (/ (- (get-internal-real-time) *prior-time*)
                               internal-time-units-per-second))))
        (format t "~%elapsed time = ~:D sec~2%" (round (/ (- (get-internal-real-time) *start-time*)
                                                       internal-time-units-per-second)))
        (finish-output)
        (setf *accumulated-depths* 0)
        (setf *num-paths* 0)
        (setf *prior-time* (get-internal-real-time))
        (setf *prior-total-states-processed* *total-states-processed*)))


(defun solve ()
  "Runs a branch & bound search on the problem specification."
  (initialize)  ;currently does nothing
  (when (and (> *threads* 0) (> *debug* 1))
    (format t "~%ADVISORY - Running parallel threads. Resetting *debug* from ~D to 1.~2%" *debug*)
    (finish-output nil)
    (ww-set *debug* 1))
  (if (> *threads* 0)
    (format t "~%working with ~D thread(s)...~2%" *threads*)
    (format t "~%working...~%"))
  (time (dfs))
  ;(finalize)
  (in-package :ww))

