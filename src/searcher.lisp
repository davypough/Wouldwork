;;; Filename:  searcher.lisp

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


(defstruct (node
             (:print-function
               (lambda (node stream depth)
                 ;Prints out a node. Used for debugging.
                 (declare (ignore depth) (node node) (stream stream))
                 (format stream "~&NODE: STATE=~A DEPTH=~:D"   ;PARENT=~S~%"
                   (node-state node) (node-depth node)))))
  (state (make-problem-state) :type problem-state)    ;problem state
  (depth 0 :type fixnum)           ;depth in the search tree
  (parent nil :type (or null node)))  ;this node's parent


#+:sbcl (sb-ext:define-hash-table-test state-equal-p state-equal-p-hash)
;Hash stack test for *open*.
(declaim (function state-equal-p state-equal-p-hash))


(defparameter *lock* (bt:make-lock))  ;for debugging


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
  (if (> *num-parallel-threads* 0)
    #+sbcl `(sb-ext:atomic-incf ,var-name ,delta-form)
    #+allegro `(excl:incf-atomic ,var-name ,delta-form)
    `(incf ,var-name ,delta-form)))


(defmacro push-global (item var-name)
  (if (> *num-parallel-threads* 0)
    #+sbcl `(sb-ext:atomic-push ,item ,var-name)
    #+allegro `(excl:push-atomic ,item ,var-name)
    `(push ,item ,var-name)))


(defmacro update-branching-factor (n)
  (if (> *num-parallel-threads* 0)
    #+sbcl `(sb-ext:atomic-update *average-branching-factor* #'compute-average-branching-factor ,n)
    #+allegro `(let ((abf (compute-average-branching-factor ,n 0.0)))
                 (excl:update-atomic (*average-branching-factor* *average-branching-factor*) abf))  
    `(setf *average-branching-factor* (compute-average-branching-factor ,n 0))))
       

(defparameter *-* '---------------------------------------------------------)
  ;Division marker for debugging printout convenience.


;;;;;;;;;;;;;;;;;;;; Search Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-global-fixnum (var-name val-form &optional doc-string)
  `(progn (declaim (fixnum ,var-name))
     ,(if (> *num-parallel-threads* 0)
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

(define-global-fixnum *repeated-states* 0
  "Count of the repeated states during a graph search.")

;(define-global-fixnum *unique-states-encountered-graph* 0
;  "Count of unique states encountered, only used during graph search (shared)")

(define-global-fixnum *program-cycles* 0
  "Count of complete cycles of searching (shared).")

(define-global-fixnum *max-depth-explored* 0
  "Keeps track of the maximum depth reached so far during the search (shared).")

(defparameter *average-branching-factor* 0.0)
;Average branching factor so far during search (shared).
(declaim (single-float *average-branching-factor*))

(defparameter *search-tree* nil)
  ;DFS search tree for debugging (serial processing only).
(declaim (list *search-tree*))


(defparameter *open*
  (if (and (> *num-parallel-threads* 0) (alexandria:featurep :sbcl))
    (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node-state
                       :ht-test 'state-equal-p :synchronized t)
    (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node-state
                       :ht-test 'state-equal-p)))
  ;The hash-stack structure containing the set of open nodes, local to search-parallel.
(declaim (hs::hstack *open*))


#+:sbcl (defparameter *visited*
          (if (eq *tree-or-graph* 'graph)
            (if (> *num-parallel-threads* 0)
              (make-hash-table :size *max-states*
                               :test 'state-equal-p
                               :synchronized t)
              (make-hash-table :size *max-states*
                               :test 'state-equal-p))
            (make-hash-table :size 0)))  ;unused null placeholder


#+:allegro (defparameter *visited*
             (when (eq *tree-or-graph* 'graph)
               (make-hash-table :size *max-states*
                                :test 'state-equal-p)))
;The hash-table containing the set of visited and closed nodes, global.
(declaim (hash-table *visited*))



;;; Search Functions


(defun dfs ()
  (when (eql *tree-or-graph* 'graph)
    (clrhash *visited*)
    (setf (gethash *start-state* *visited*) (list nil 0 0.0 0.0)))  ;(dead depth time value)
  (setf *open*
    (if (and (> *num-parallel-threads* 0) (member :sbcl *features*))
      (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node-state
                         :ht-test 'state-equal-p :synchronized t)
      (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node-state
                         :ht-test 'state-equal-p)))
  (hs::push-hstack (make-node :state *start-state*) *open*)
  (setf *program-cycles* 1)
  (setf *average-branching-factor* 0.0)
  (setf *total-states-processed* 1)
;  (setf *unique-states-encountered-graph* 1)  ;only used during graph search
  (setf *max-depth-explored* 0)
  (setf *num-idle-threads* 0)
  (setf *solutions* nil)
  (setf *solution-count* 0)
  (setf *search-tree* nil)
  (if (> *num-parallel-threads* 0)
    ;(with-open-stream (*standard-output* (make-broadcast-stream)) ;ignore *standard-output*
    (process-threads)
    (search-serial))
  (let ((*package* (find-package :ww)))  ;avoid printing package prefixes
    (summarize-search-results (if (eq *solution-type* 'first)
                                'first
                                'exhausted))))


(defun search-serial ()
  ;Branch & Bound DFS serial search.
  (iter
    (for succ-nodes = (df-bnb1 *open*))  ;may modify *open* via close-barren-nodes
    (when (equal succ-nodes '(first))
      (return-from search-serial 'first))
    (when (hs::empty-hstack *open*)
      (leave))  ;terminate *open*
    (when succ-nodes
      (if (fboundp 'heuristic?)
        (setf succ-nodes (sort succ-nodes #'>
                               :key (lambda (node)
                                      (problem-state-heuristic (node-state node)))))
        (when *randomize-search*
          (setf succ-nodes (alexandria:shuffle succ-nodes)))))
    (loop for succ-node in succ-nodes
          do (hs::push-hstack succ-node *open*))))  ;push lowest heuristic value last


(defun break-here ()  ;inhibits debugger printing of open on break in df-bnb1
  (break))


(defun df-bnb1 (open)
  ;Performs expansion of one node from open. Returns
  ;new successor nodes, (first), or nil if no new nodes generated.
  (declare (hs::hstack open))
  (when-debug>= 3
     (format t "~&-----------------------------------------------------------~%"))
  (when-debug>= 5
     (break-here))
  (let ((current-node (get-next-node-for-expansion open)))
    (when *probe*
      (apply #'probe current-node *probe*))
    (when-debug>= 3
      (format t "~3%Current node selected:~%~S~2%" current-node))
    (when (null current-node)  ;open is empty
      (return-from df-bnb1 nil))
    (let ((succ-states (get-successors current-node)))
      (when-debug>= 4
        (format t "~%ALL SUCCESSORS~%  ~S" succ-states))
      (update-max-depth-explored (1+ (node-depth current-node)))
      (when (null succ-states)  ;no successors
        (when-debug>= 3
          (format t "~%No successor states"))
        (update-search-tree (node-state current-node) (node-depth current-node) "No successor states")
        (close-barren-nodes current-node open)
        (return-from df-bnb1 nil))
      (ut::mvb (goal-succ-states nongoal-succ-states) (detect-goals current-node succ-states)
         (when-debug>= 3
            (format t "~%GOAL SUCCESSOR STATES~%  ~S" goal-succ-states)
            (format t "~&NONGOAL SUCCESSOR STATES~%  ~S" nongoal-succ-states))
         (cond (goal-succ-states
                  (update-search-tree (node-state current-node) (node-depth current-node) "")
                  (iter (for state in goal-succ-states)
                     (update-search-tree state (1+ (node-depth current-node)) "***goal***"))
                  (when (and goal-succ-states (eql *solution-type* 'first))
                     (return-from df-bnb1 '(first))))
               (nongoal-succ-states
                  (update-search-tree (node-state current-node) (node-depth current-node) ""))
               (t (update-search-tree (node-state current-node) (node-depth current-node) "No viable successor states")
                  (close-barren-nodes current-node open)
                  (return-from df-bnb1 nil)))
         ;nongoal successors will be updated on subsequent get-current-node
         (let ((succ-nodes (process-nongoal-succ-states current-node nongoal-succ-states)))
            (when (null succ-nodes)
              (if (and goal-succ-states (eql *tree-or-graph* 'graph))
                 (pop-discontinued-node open)
                 (close-barren-nodes current-node open)))
            (increment-global-fixnum *program-cycles*)
            (update-branching-factor (length succ-nodes))
            (return-from df-bnb1 (nreverse succ-nodes)))))))


(defun update-max-depth-explored (succ-depth)
  (when (> succ-depth *max-depth-explored*)
    (increment-global-fixnum *max-depth-explored* (- succ-depth *max-depth-explored*))))


(defun get-next-node-for-expansion (open)
  ;Returns the node at the top of open.
  (declare (hs::hstack open))
  (hs::peek-hstack open))  ;top of stack


(defun get-successors (current-node)
  ;Returns children states of current node to df-bnb1.
  (declare (node current-node))
  (expand (node-state current-node)))


(defmacro process-goal-code ()
  ;Inserts the proper code for each *solution-type* into detect-goals
  ;for each goal.
  (ecase *solution-type*
    (first
      `(register-solution current-node state))
    (every
      `(register-solution current-node state))
    (count  ;count solution, but don't save it
      `(increment-global-fixnum *solution-count*))
    (min-length
      `(when (or (null *solutions*)
                 (< (1+ (node-depth current-node))
                    (solution-depth (first *solutions*))))
         (register-solution current-node state)))
    (min-time
      `(when (or (null *solutions*)
                 (< (problem-state-time state)  ;better time found
                    (solution-time (first *solutions*))))
         (register-solution current-node state)))
    (min-value
      `(when (or (null *solutions*)
                 (< (problem-state-value state)
                    (solution-value (first *solutions*))))
         (register-solution current-node state)))
    (max-value
      `(when (or (null *solutions*)
                 (> (problem-state-value state)
                    (solution-value (first *solutions*))))
         (register-solution current-node state)))))


(defun detect-goals (current-node succ-states)
  (declare (list succ-states) (ignorable current-node))
  (iter (for state in succ-states)
     (cond ((goal state)
              (collect state into goal-succ-states)
              (process-goal-code))  ;code inserted from above
           (t (collect state into nongoal-succ-states)))
     (finally (return (values goal-succ-states nongoal-succ-states)))))


(defmacro insert-tree-or-graph-code ()
  (ecase *tree-or-graph*
    (graph `(process-successor-graph current-node state))  ;rtn new node or nil
    (tree `(process-successor-tree current-node state))))  ;rtn new node or nil  


(defun process-nongoal-succ-states (current-node nongoal-succ-states)
  ;Determines how to handle nongoal succ states for both tree or graph search.
  (declare (node current-node) (list nongoal-succ-states))
  (loop with succ-nodes
        for state in nongoal-succ-states
        do (ut::mvb (message succ-node) (insert-tree-or-graph-code)
             (if succ-node
               (push succ-node succ-nodes)
               (update-search-tree state (1+ (node-depth current-node)) message)))
      finally (return succ-nodes)))


(defun compute-average-branching-factor (n prior-value)
  ;Cumulative average of expanded states.
  (declare (fixnum n) (ignore prior-value))
  (/ (+ n (* *program-cycles* *average-branching-factor*))
     *program-cycles*))


(defmacro insert-graph-code ()
  ;Inserts the proper code for each *solution-type* into process-successor-graph.
  ;Analyzes states already visited to determine if a successor can be pruned.
  (ecase *solution-type*
    ((first every count min-length)
      `(let ((succ-depth (1+ (node-depth current-node))))
         (if (>= succ-depth (second prior-dead-depth-time-val))
           (progn (when-debug>= 3
                    (format t "~2%Same or shorter path length to state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or shorter path length to state already exists" nil))))
         (setf (second prior-dead-depth-time-val) succ-depth)))
    (min-time
      `(let ((succ-time (problem-state-time succ-state)))
         (if (>= succ-time (third prior-dead-depth-time-val))
           (progn (when-debug>= 3
                    (format t "~2%Same or shorter time to state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or shorter time to state already exists" nil)))
           (setf (third prior-dead-depth-time-val) succ-time))))
    (min-value
      `(let ((succ-value (problem-state-value succ-state)))
         (if (>= succ-value (fourth prior-dead-depth-time-val))
           (progn (when-debug>= 3
                    (format t "~2%Same or lesser value at state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or lesser value at state already exists" nil)))
           (setf (fourth prior-dead-depth-time-val) succ-value))))
    (max-value
      `(let ((succ-value (problem-state-value succ-state)))
         (if (<= succ-value (fourth prior-dead-depth-time-val))
           (progn (when-debug>= 3
                    (format t "~2%Same or greater value at state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or greater value at state already exists" nil)))
           (setf (fourth prior-dead-depth-time-val) succ-value))))))


(defun process-successor-graph (current-node succ-state)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (problem-state succ-state))
  (increment-global-fixnum *total-states-processed*)
  (print-search-progress-graph)       ;#nodes expanded so far
  (when (at-max-depth (1+ (node-depth current-node)))  ;at max depth, if *depth-cutoff* specified
    (when-debug>= 3
      (format t "~2%State at max depth:~%~A" succ-state))
    (return-from process-successor-graph (values "State at max depth" nil)))
  (let ((prior-dead-depth-time-val (gethash succ-state *visited*)))
    (when (and prior-dead-depth-time-val (first prior-dead-depth-time-val))  ;previously closed
      (increment-global-fixnum *repeated-states*)
      (when-debug>= 3
        (format t "~2%State previously closed:~%~A" succ-state))
      (return-from process-successor-graph (values "State previously closed" nil)))
    (when (and prior-dead-depth-time-val (not (first prior-dead-depth-time-val)))  ;already on open, not closed
      (increment-global-fixnum *repeated-states*)
      (insert-graph-code)))  ;from macro above depending on *solution-type*
  (values "" (generate-new-node current-node succ-state)))  ;create new node on open


(defmacro insert-tree-code ()
  ;Inserts the proper code for each *solution-type* into process-successor-graph.
  ;Analyzes states already visited to determine if a successor can be pruned.
  (ecase *solution-type*
    (first  ;present only to allow compilation with default solution-type = first, exited earlier via detect-goals
      `nil)
    ((every count min-value max-value)
      `nil)  ;continue searching
    (min-length
      `(when (and *solutions* (>= (1+ (node-depth current-node)) (solution-depth (first *solutions*))))
         (when-debug>= 3
           (format t "~2%Same or shorter path length to a solution already exists:~%~A" succ-state))
         (return-from process-successor-tree (values "Same or shorter path length to a solution already exists" nil))))
    (min-time
      `(when (and *solutions* (>= (problem-state-time succ-state) (solution-time (first *solutions*))))
         (when-debug>= 3
           (format t "~2%Same or shorter time to a solution already exists:~%~A" succ-state))
         (return-from process-successor-tree (values "Same or shorter time to a solution already exists" nil))))))


(defun process-successor-tree (current-node succ-state)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (problem-state succ-state))
  (increment-global-fixnum *total-states-processed*)
  (print-search-progress-tree)       ;#nodes expanded so far
  (when (on-current-path succ-state current-node)
    (when-debug>= 3
      (format t "~2%State already on current path:~%~A" succ-state))
    (return-from process-successor-tree (values "State already on current path" nil)))
  (when (at-max-depth (1+ (node-depth current-node)))
    (when-debug>= 3
      (format t "~2%State at max depth:~%~A" succ-state))
    (return-from process-successor-tree (values "State at max depth" nil)))
  (insert-tree-code)  ;from macro above depending on *solution-type*
  (values "" (generate-new-node current-node succ-state)))


(defun on-current-path (succ-state current-node)
  ;Determines if a successor is already on the current path from the start state.
  (iter (for node initially current-node then (node-parent node))
        (while node)  
        (thereis (state-equal-p succ-state (node-state node)))))
          

(defun generate-new-node (current-node succ-state)
  ;Produces a new node for a given successor.
  (declare (node current-node) (problem-state succ-state))
  (let ((succ-node (make-node :state succ-state
                              :depth (1+ (node-depth current-node))
                              :parent current-node)))
    (when-debug>= 3
      (format t "~2%Installing new or updated successor:~%~S" succ-node))
    (when (eql *tree-or-graph* 'graph)
      (setf (gethash succ-state *visited*) (list nil  ;whether dead or not, initially live
                                                 (1+ (node-depth current-node))
                                                 (problem-state-time succ-state)
                                                 (problem-state-value succ-state))))
    succ-node))


(defun close-barren-nodes (current-node open)
  ;Remove nodes from open & close, unless current-node has goal succ.
  (declare (node current-node) (hs::hstack open))
  (do ((node current-node) parent)
      ((or (null node) (not (eq node (hs::peek-hstack open)))))
    (hs::pop-hstack open)
    (when (eql *tree-or-graph* 'graph)
      (setf (first (gethash (node-state node) *visited*)) t))  ;close dead node
    (setq parent (node-parent node))
    (setf (node-parent node) nil)
    (when-debug>= 4
      (format t "~2%Closing barren node:~%~S~%" node))
    (setq node parent)))


(defun pop-discontinued-node (open)
  ;Discontinue node if all successors are goals
  (let ((node (hs::pop-hstack open)))
     (when-debug>= 4
                   (format t "~2%Popping discontinued node:~%~S~%" node))
     (when (= (hs::length-hstack open) 1)  ;last node on open = start node
        (close-barren-nodes (hs::pop-hstack open) open))))


(defun update-search-tree (state depth message)
  (declare (problem-state state) (fixnum depth) (string message))
  (when (and (not (> *num-parallel-threads* 0)) (>= *debug* 1))
    (push `((,(problem-state-name state) 
             ,@(problem-state-instantiations state))
           ,depth
           ,message
           ,@(case *debug*
               (1 nil)
               (2 (list (list-database (problem-state-idb state))
                        (list-database (problem-state-hidb state))))))
          *search-tree*)))


(defun at-max-depth (succ-depth)
  ;Determines if installing a nongoal successor to the current node will be
  ;pointless, based on it being at the max allowable depth.
  (declare (fixnum succ-depth))
  (let ((depth-cutoff *depth-cutoff*))
    (and (> depth-cutoff 0)
         (= succ-depth depth-cutoff))))


(defun best-states-last (state1 state2)
  ;Used to sort a list of expanded states according to the user-defined heuristic.
  (declare (problem-state state1 state2))
  (> (estimate-to-goal state1) (estimate-to-goal state2)))


(defun probe (current-node name instantiations depth &optional (count 1))
  ;Breaks when the current node matches action name, instantiations, depth, and count.
  (declare (node current-node))
  (let ((state (node-state current-node)))
    (when (and (eql (problem-state-name state) name)
               (equal (problem-state-instantiations state) instantiations)
               (= (node-depth current-node) depth))
      (if (= count *counter*)
        (ww-set *debug* 5)
        (incf *counter*)))))


;;; Solution Processing Functions


(defun record-solution-path (goal-node)
  ;Recovers a path from a goal node back to the start node following parent links.
  (declare (node goal-node))
  (let ((path nil))
    (do ((n goal-node (node-parent n)))
        ((null (node-parent n)))
      (push (record-move (node-state (node-parent n)) (node-state n))
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
        (count
          (format t "~2%Found ~:D solutions." *solution-count*))
        (every
          (format t "~2%Examined every state up to the depth cutoff."))
        ((min-length min-time max-value min-value)
          (format t "~2%Examined only worthwhile states up to the depth cutoff.")))))
  (format t "~2%Depth cutoff = ~:D" *depth-cutoff*)
  (format t "~2%Maximum depth explored = ~:D" *max-depth-explored*)
  (format t "~2%Total states processed = ~:D" *total-states-processed*)
  (when (eql *tree-or-graph* 'graph)
    (format t "~2%Repeated states = ~:D, ie, ~F percent" *repeated-states* 
                                                         (* (/ *repeated-states* *total-states-processed*)
                                                            100)))
  ;(format t "~2%Unique states encountered = ~:D" (unique-states-encountered-graph))
  (format t "~2%Program cycles (state expansions) = ~:D" *program-cycles*)
  (format t "~2%Average branching factor = ~F" *average-branching-factor*)
  (format t "~2%Start state:~%~A" (list-database (problem-state-idb *start-state*)))
  (format t "~2%Goal:~%~A~2%" (get '*goal* 'formula))
  (if *solutions*
    (let* ((shallowest-depth (reduce #'min *solutions* :key #'solution-depth))
           (shallowest-depth-solution (find shallowest-depth *solutions* :key #'solution-depth))
           (minimum-time (reduce #'min *solutions* :key #'solution-time))
           (minimum-time-solution (find minimum-time *solutions* :key #'solution-time))
           (min-max-value-solution (first *solutions*))
           (min-max-value (solution-value min-max-value-solution)))
      (format t "~2%Total solutions recorded = ~:D" (length *solutions*))
      (format t "~%(Check *solutions* for list of all solution records.)")
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
          (if (equalp shallowest-depth-solution minimum-time-solution)
            (format t "~%A shortest path solution is also a minimum duration solution.~2%")
            (progn (format t "~2%Duration of a minimum time solution = ~:D" minimum-time)
                   (format t "~2%A minimum time solution path from start state to goal state:~%")
                   (printout-solution minimum-time-solution))))))
    (unless (eq *solution-type* 'count)
      (format t "~2%No solutions found.~2%")))
  (print-search-tree))


(defun print-search-tree ()
  (when (and (not (> *num-parallel-threads* 0)) (or (= *debug* 1) (= *debug* 2)))
    (format t "~2%Search tree:~%")
    (loop for act in (reverse *search-tree*)
          do (if (= (length act) 2)
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
  ;Inserts a new solution on the list of *solutions*.
  (declare (node current-node) (problem-state goal-state))
  (let* ((state-depth (1+ (node-depth current-node)))
         (solution
           (make-solution
             :depth state-depth
             :time (problem-state-time goal-state)
             :value (problem-state-value goal-state)
             :path (let ((nominal-path (append (record-solution-path current-node)
                                               (list (record-move (node-state current-node)
                                                     goal-state)))))
                     (if (= (hash-table-count *state-codes*) 0)  ;if in backward search
                       nominal-path
                       (append nominal-path 
                               (reverse (gethash (funcall 'encode-state (list-database (problem-state-idb goal-state)))
                                                 *state-codes*)))))
             :goal goal-state)))
    (if (> *num-parallel-threads* 0)
      (progn (when-debug>= 1
               (lprt))
             (let ((ctrl-str "~&New path to goal found at depth = ~:D~%"))
               (bt:with-lock-held (*lock*)
                 (if (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
                   (format t (concatenate 'string ctrl-str "Objective value = ~:A~2%")
                             state-depth (solution-value solution))
                   (format t ctrl-str state-depth)))))
      (progn (format t "~%New path to goal found at depth = ~:D~%" state-depth)
             (when (or (eql *solution-type* 'min-value) (eql *solution-type* 'max-value))
               (format t "Objective value = ~:A~%" (solution-value solution)))))
    (when-debug>= 3
      (format t "~%New solution found:~%  ~A~%" solution))
    (push-global solution *solutions*)))


(defun printout-solution (soln)
  (declare (solution soln))
  (dolist (item (solution-path soln))
    (write item :pretty t)
    (terpri))
  (format t "~%Final state:~%~A~2%"
    (list-database (problem-state-idb (solution-goal soln)))))


(defun print-search-progress-graph ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* *progress-reporting-interval*))
    (format t "~%total states processed so far = ~:D"  ;, unique states encountered = ~:D"
      *total-states-processed*)  ; *unique-states-encountered-graph*)
    (format t "~%ht count: ~:D    ht size: ~:D" (hash-table-count *visited*) (hash-table-size *visited*))
    (format t "~%average branching factor = ~F~%" *average-branching-factor*)))


(defun print-search-progress-tree ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* *progress-reporting-interval*))
    (format t "~%total states processed so far = ~:D" *total-states-processed*)
    (format t "~%average branching factor = ~F~%" *average-branching-factor*)))


(defun solve ()
  ;Runs a branch & bound search on the problem specification.
  (initialize)
  (when (and (> *num-parallel-threads* 0) (> *debug* 1))
    (format t "ADVISORY: Running parallel threads, resetting *debug* from ~D to 1." *debug*)
    (setf *debug* 1))
  (if (> *num-parallel-threads* 0)
    (format t "~%working with ~:D thread(s)...~2%" *num-parallel-threads*)
    (format t "~%working...~%"))
  (time (dfs))
  (finalize)
  (in-package :ww))
