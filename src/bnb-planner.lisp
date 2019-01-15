;;; Filename:  bnb-planner.lisp

;;; Nonstandard Depth First Branch & Bound. Optional duplicate checking for nodes
;;; previously visited (graph search) as specified in problem spec. Open nodes are
;;; kept in an indexed stack and (optionally for graph search) closed nodes in a
;;; hash table. Search
;;; follows the scheme described in "Ch 3&4 Artificial Intelligence: A Modern
;;; Approach" by Russell & Norvig, p87 (with closing of barren nodes),
;;; but keeps nodes on OPEN until the search skeleton is pruned (either dropped for
;;; tree search or to CLOSED for graph search.
;;; A complete path from a goal to the start state is then available in OPEN.
;;; Searches up to a user-specified depth cutoff.  In graph search a visited node is either
;;; in OPEN or CLOSED at any particular time. A user-defined heuristic can be
;;; used to expand the best states first at each level (beam search), but
;;; the entire search graph may ultimately be searched (completeness).


(in-package :bnb)


(defstruct (node
             (:print-function
               (lambda (node stream depth)
                 ;Prints out a node. Used for debugging.
                 (declare (ignore depth) (node node) (stream stream))
                 (format stream "NODE: STATE=~S~&   DEPTH=~:D ~%"   ;PARENT=~S~%"
                   (node-state node)
                   (node-depth node)
                   #|(ut::if-it (node-parent node) (node-state ut::it))|#))))
  (state   nil :type ww::problem-state)    ;problem state
  (depth   0   :type fixnum)           ;depth in the search tree
  (parent  nil :type (or null node)))  ;this node's parent


(declaim
 (fixnum ww::*progress-reporting-interval* ww::*max-states* ww::*depth-cutoff*)
 (single-float *average-branching-factor*)
 (list *solutions* *succ-states* *search-tree*)
 (ww::problem-state ww::*start-state*)
 (hs::hash-stack *open*)
 (hash-table *closed*)
 (function print-node ww::state-key ww::same-state))


;;; Global Search Parameters


(defparameter *total-states-processed* 0)
  ;Count of states either newly generated, updated, or regenerated while searching.


(defparameter *succ-states* nil)
  ;List of the successor states of the current node.


(defparameter *program-cycles* 0)
  ;Increments each time the DFS program processes the next current node.


(defparameter *max-depth-explored* 0)
  ;Keeps track of the maximum depth reached so far during the search.


(defparameter *average-branching-factor* 0.0)
  ;Average branching factor so far during search.


(defvar *open*)
  ;The hash-stack structure containing the set of open nodes.


(defvar *closed*)
 ;The set of closed states.


(defparameter *search-tree* nil)



;;; Utility Functions


(defun node-key (node)
  ;Gets the hash key for looking up a node in the open set.
  (declare (node node))
  (ww::state-key (node-state node)))



;;; Solution Processing Functions


(defun record-solution-path (goal-node)
  ;Recovers a path from a goal node back to the start node following parent links.
  (declare (node goal-node))
  (let ((path nil))
    (do ((n goal-node (node-parent n)))
        ((null (node-parent n)))
      (push (ww::record-move (node-state (node-parent n)) (node-state n))
            path))
    (return-from record-solution-path path)))


  
(defun unique-states-encountered-graph ()
  ;Count of how many unique states have been encountered during searching.
  (+ (hs::hash-stack-count *open*)
     (1- (hash-table-count *closed*))
     (length ww::*solutions*)))

   
(defun summarize-search-results-graph (condition)
  (declare (symbol condition))
  (case condition
    (first (format t "~%~%Graph search ended with first solution found.~%"))
    (exhausted (format t "~%~%Graph search process completed normally,~%")
               (format t "examining every state up to the depth cutoff.~%")))
  (format t "~%Depth cutoff = ~:D~%" ww::*depth-cutoff*)
  (format t "~%Maximum depth explored = ~:D~%" *max-depth-explored*)
  (format t "~%Total states processed = ~:D~%" *total-states-processed*)
  (format t "~%Unique states encountered = ~:D~%" (unique-states-encountered-graph))
  (format t "~%Program cycles (state expansions) = ~:D~%" *program-cycles*)
  (format t "~%Average branching factor = ~F~%" *average-branching-factor*)
  (format t "~%Start state:~%~A~%" (ww::list-database (ww::problem-state-idb ww::*start-state*)))
  (format t "~%Goal:~%~A~%" (get 'ww::*goal* 'ww::formula))
  (setf ww::*solutions* (nreverse ww::*solutions*))
  (if ww::*solutions*
    (let ((shallowest-depth (reduce #'min ww::*solutions* :key #'first :from-end t))
          (minimum-time (reduce #'min ww::*solutions*
                                :key (lambda (soln)
                                       (ww::problem-state-time (third soln)))
                                :from-end t)))
      (format t "~%Total solutions found = ~:D" (length ww::*solutions*))
      (format t "~%(Check ww::*solutions* for list of all solutions.)~%")
      (in-package :ww)  ;print without package prefix
      (format t "~%Number of steps in minimum path length solution = ~:D~%" shallowest-depth)
      (format t "~%Solution path from start state to goal state:~%")
      (let ((shallowest-depth-solution (find shallowest-depth ww::*solutions* :key #'first)))
        (printout-solution shallowest-depth-solution))
      (if (and (not ww::*first-solution-sufficient*)
               (eql shallowest-depth minimum-time))
        (format t "~%Shortest path solution is also a minimum duration solution~%")
        (let ((minimum-time-solution (find minimum-time ww::*solutions*
                                           :key (lambda (soln)
                                                  (ww::problem-state-time (third soln))))))
          (format t "~%Duration of minimum time solution = ~:D~%" minimum-time)
          (format t "~%Minimum time solution path from start state to goal state:~%")
          (printout-solution minimum-time-solution)))
      (in-package :bnb)
      (terpri))
    (format t "~%No goals recorded.~2%"))
  (when (= ww::*debug* 1)
    (format t "~%Search tree:~%")
    (loop for act in (cdr (reverse *search-tree*))
          do (if (= (length act) 2)
               (format t "~vT~d:~a~%" (* 4 (second act)) (second act) (first act))
               (format t "~vT~d:~a ~a~%" (* 4 (second act)) (second act) (first act) (third act)))
          finally (terpri)))
  (when (= ww::*debug* 2)
    (format t "~%Search tree:~%")
    (loop for act in (cdr (reverse *search-tree*))
          do (if (= (length act) 2)
               (format t "~vT~d:~a~%" (* 4 (second act)) (second act) (first act))
               (progn (format t "~vT~d:~a ~a~%" 
                                (* 4 (second act)) (second act) (first act) (third act))
                      (format t "~vT  ~a~%" (* 4 (second act)) (fourth act))))
          finally (terpri))))


 
(defun summarize-search-results-tree (condition)
  (declare (symbol condition))
  (case condition
    (first
     (format t "~%~%Tree search ended with first solution found.~%"))
    (exhausted
     (format t "~%~%Tree search process completed normally,~%")
     (format t "examining every state up to the depth cutoff.~%")))
  (format t "~%Depth cutoff = ~:D~%" ww::*depth-cutoff*)
  (format t "~%Maximum depth explored = ~:D~%" *max-depth-explored*)
  (format t "~%Total states processed = ~:D~%" *total-states-processed*)
  (format t "~%Program cycles (states expanded) = ~:D~%" *program-cycles*)
  (format t "~%Average branching factor = ~F~%" *average-branching-factor*)
  (format t "~%Start state:~%~A~%" (ww::list-database (ww::problem-state-idb ww::*start-state*)))
  (format t "~%Goal:~%~A~%" (get 'ww::*goal* 'ww::formula))
  (setf ww::*solutions* (nreverse ww::*solutions*))
  (if ww::*solutions*
      (let ((shallowest-depth (reduce #'min ww::*solutions* :key #'first :from-end t))
            (minimum-time (reduce #'min ww::*solutions*
                                  :key (lambda (soln)
                                         (ww::problem-state-time (third soln)))
                                  :from-end t)))
        (format t "~%Total solutions found = ~:D" (length ww::*solutions*))
        (format t "~%(Check ww::*solutions* for list of all solutions.)~%")
        (in-package :ww)  ;print without package prefix
        (format t "~%Number of steps in minimum path length solution = ~:D~%" shallowest-depth)
        (format t "~%Solution path from start state to goal state:~%")
        (let ((shallowest-depth-solution (find shallowest-depth ww::*solutions* :key #'first)))
          (printout-solution shallowest-depth-solution))
        (if (and (not ww::*first-solution-sufficient*)
                 (eql shallowest-depth minimum-time))
            (format t "~%Shortest path solution is also a minimum duration solution~%")
          (let ((minimum-time-solution (find minimum-time ww::*solutions*
                                             :key (lambda (soln)
                                                    (ww::problem-state-time (third soln))))))
            (format t "~%Duration of minimum time solution = ~:D~%" minimum-time)
            (format t "~%Minimum time solution path from start state to goal state:~%")
            (printout-solution minimum-time-solution)))
        (in-package :bnb)
        (terpri))
    (format t "~%No goals recorded.~2%"))
  (when (= ww::*debug* 1)
    (format t "~%Search tree:~%")
    (loop for act in (cdr (reverse *search-tree*))
        do (if (= (length act) 2)
               (format t "~vT~d:~a~%"
                 (* 4 (second act)) (second act) (first act))
             (format t "~vT~d:~a ~a~%" 
               (* 4 (second act)) (second act) (first act) (third act)))
        finally (terpri)))
  (when (= ww::*debug* 2)
    (format t "~%Search tree:~%")
    (loop for act in (cdr (reverse *search-tree*))
          do (if (= (length act) 2)
               (format t "~vT~d:~a~%" (* 4 (second act)) (second act) (first act))
               (progn (format t "~vT~d:~a ~a~%" 
                                (* 4 (second act)) (second act) (first act) (third act))
                      (format t "~vT~a~%" (* 4 (second act)) (fourth act))))
          finally (terpri))))



(defun printout-solution (solution)
  (dolist (item (second solution))
    (write item :pretty t)
    (terpri))
  (format t "~%Final state:~%~A~%"
;;    (ww::list-database (ww::problem-state-db (third solution)))))
    (ww::list-database (ww::problem-state-idb (third solution)))))



(defun register-solution (current-node goal-state)
  ;Inserts a new goal on the list of solutions.
  ;A solution is a list of 3 items: depth, path, goal state.
  (declare (node current-node) (ww::problem-state goal-state))
  (let ((current-solution
          (append (list (1+ (node-depth current-node)))
                  (list (append (record-solution-path current-node)
                                (list (ww::record-move (node-state current-node)
                                                       goal-state)))) ;add final move
                  (list goal-state))))
    (format t "~%New path to goal found at depth = ~:D~%" (1+ (node-depth current-node)))
    (push current-solution ww::*solutions*)
    (when (>= ww::*debug* 3)
      (format t "~A~%" current-solution))))


(defun print-search-progress-graph ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* ww::*progress-reporting-interval*))
    ;(sb-ext:gc)
    (format t "~%total states processed so far = ~:D, unique states encountered = ~:D"
      *total-states-processed* (unique-states-encountered-graph))
    (format t "~%average branching factor = ~F~%" *average-branching-factor*)))


(defun print-search-progress-tree ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* ww::*progress-reporting-interval*))
    ;(sb-ext:gc)
    (format t "~%total states processed so far = ~:D" *total-states-processed*)
    (format t "~%average branching factor = ~F~%" *average-branching-factor*)))



;;; Search Functions


(defun initialize-search-graph ()
  (setq ww::*solutions* nil)
  (setq *open* (hs::create-hash-stack (/ ww::*max-states* 10)
                                      #'equalp #'node-key))  ;equalp required
  (hs::push-onto-hash-stack (make-node :state ww::*start-state*) *open*)
  (setq *closed* (make-hash-table :size ww::*max-states* :test #'equalp)) ;equalp required
  (setq *search-tree* nil)
  (setq *succ-states* nil)
  (setq *program-cycles* 1)  ;always process *start-state*
  (setq *average-branching-factor* 0.0)
  (setq *total-states-processed* 1))


(defun initialize-search-tree ()
  (setq ww::*solutions* nil)
  (setq *open* (hs::create-hash-stack (/ ww::*max-states* 10)
                                      #'equalp #'node-key))  ;equalp required
  (hs::push-onto-hash-stack (make-node :state ww::*start-state*) *open*)
  (setq *search-tree* nil)
  (setq *succ-states* nil)
  (setq *program-cycles* 1)
  (setq *average-branching-factor* 0.0)
  (setq *total-states-processed* 1))


(defun search-space-is-exhausted ()
  ;Determines if set of open nodes is empty.
  (hs::hash-stack-is-empty *open*))


(defun at-max-depth (current-node)
  ;Determines if installing a nongoal successor to the current node will be
  ;pointless, based on it being at the max allowable depth.
  (declare (node current-node))
  (let ((depth (node-depth current-node)))
    (when (> ww::*depth-cutoff* 0)
      (= depth (1- ww::*depth-cutoff*)))))


(defun get-next-node-for-expansion ()
  ;Returns the node at the top of open.
  (hs::peek-at-hash-stack 0 *open*))  ;top of stack


(defun best-states-last (state1 state2)
  ;Used to sort a list of expanded states according to the user-defined heuristic.
  (declare (ww::problem-state state1 state2))
  (> (ww::estimate-to-goal state1) (ww::estimate-to-goal state2)))


(defun close-barren-nodes-graph (current-node potential?)
  ;Move nodes from open to closed if barren.
  (declare (node current-node) ((or nil t) potential?))
  (do ((node current-node)
       parent)
      ((or (null node) (not (eq node (hs::peek-at-hash-stack 0 *open*)))))
    (hs::pop-from-hash-stack *open*)
    (setf (gethash (ww::state-key (node-state node)) *closed*)
      (node-depth node))
    (setq parent (node-parent node))
    (setf (node-parent node) nil)
    (when (>= ww::*debug* 4)
      (format t "~2%Node closed by ~S:~%~S~%"
        (if potential? "cutoff" "deadend") node))
    (setq node parent)))


(defun close-barren-nodes-tree (current-node potential?)
  ;Move nodes from open to closed if barren.
  (declare (node current-node) ((or nil t) potential?))
  (do ((node current-node)
       parent)
      ((or (null node) (not (eq node (hs::peek-at-hash-stack 0 *open*)))))
    (hs::pop-from-hash-stack *open*)
    (setq parent (node-parent node))
    (setf (node-parent node) nil)
    (when (>= ww::*debug* 4)
      (format t "~%Node closed by ~S:~%~S~%"
        (if potential? "cutoff" "deadend") node))
    (setq node parent)))


(defun generate-new-node (current-node succ-state)
  ;Produces a new node for a given successor.
  (declare (node current-node) (ww::problem-state succ-state))
  (let ((succ-node (make-node :state succ-state
                              :depth (1+ (node-depth current-node))
                              :parent current-node)))
    (when (>= ww::*debug* 3)
      (format t "~2%Installing new or updated successor:~%~S" succ-node))
    (hs::push-onto-hash-stack succ-node *open*)))

   
(defun process-successor-graph (current-node succ-state)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (ww::problem-state succ-state))
  (incf *total-states-processed*)
  (print-search-progress-graph)       ;#nodes expanded so far
  (cond ((at-max-depth current-node)
         (when (>= ww::*debug* 3)
           (format t "~%Deadend--state at max depth:~%~A" succ-state)) 
         "Deadend--state at max depth")
        ((hs::find-in-hash-stack (ww::state-key succ-state) *open*)
         (when (>= ww::*debug* 3)
           (format t "~%Deadend--state already waiting in stack:~%~A" succ-state))
         "Deadend--state already waiting in stack")
        ((not (gethash (ww::state-key succ-state) *closed*)) ;not on *open* or *closed*
         (generate-new-node current-node succ-state)
         t)
        ((>= (1+ (node-depth current-node))  ;depth greater than already closed state
             (gethash (ww::state-key succ-state) *closed*))
         (when (>= ww::*debug* 3)
           (format t "~%Deadend--equal or shorter path to state exists:~%~A" succ-state))
         "Deadend--equal or shorter path to state exists")
        ;  ((= 0 (gethash (ww::state-key succ-state) *closed*)) ;previously closed barren
        ;  "Deadend--previously closed barren state")
        ((< (1+ (node-depth current-node))  
            (gethash (ww::state-key succ-state) *closed*))
         (remhash (ww::state-key succ-state) *closed*) ;shallower node found
         (generate-new-node current-node succ-state) ;put back on open
         t)
        (t (error "In process-successor-graph"))))


(defun process-successor-tree (current-node succ-state)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (ww::problem-state succ-state))
  (incf *total-states-processed*)
  (print-search-progress-tree)       ;#nodes expanded so far
  (cond ((hs::find-in-hash-stack (ww::state-key succ-state) *open*)
         nil)              ;close deadend
        ((at-max-depth current-node)
         (when (>= ww::*debug* 3)
           (format t "~%Deadend--at max depth:~%~A" succ-state))
         t)
        (t (generate-new-node current-node succ-state)    ;better node found
           t)))              ;leave on open


(defun get-successors (current-node parent)
  ;Returns children states of current node (unless same as parent).
  (let ((states (coerce (if parent
                            (remove (node-state parent) (ww::expand (node-state current-node))
                                    :test #'ww::same-state :count 1)
                            (ww::expand (node-state current-node)))
                        'list)))
;    (ut::prt states) (ut::prt parent)
;    (ut::sortf states #'(lambda (x y)
;                          (declare (ignore y))
;                          (not (eq x (ww::problem-state-name (node-state parent)))))
;               :key #'ww::problem-state-name)
;    (ut::prt states)
;    (terpri) (terpri)
   states))


(defun check-for-barren-nodes (current-node potential?)
  ;Backtrack to close barrens.
  (if (eq current-node (hs::peek-at-hash-stack 0 *open*)) ;still at top of stack
    (ecase ww::*tree-or-graph*
      (ww::graph (close-barren-nodes-graph current-node potential?))
      (ww::tree (close-barren-nodes-tree current-node potential?)))
    (setq *average-branching-factor*    ;cumulative average of expanded states
          (/ (+ (length *succ-states*)
                (* *program-cycles* *average-branching-factor*))
             (incf *program-cycles*)))))


(defun df-bnb ()
  ;Branch & Bound algorithm. Keeps searching for improvements to
  ;an initial solution, at user's request.
  ;Solutions stored in ww::*solutions*.
  (ecase ww::*tree-or-graph*
    (ww::graph (initialize-search-graph))
    (ww::tree (initialize-search-tree)))
  (loop
    (let* ((current-node (get-next-node-for-expansion))
           (parent (node-parent current-node)))
      
;     Stop at specified node, for debugging <action name> <instantiations> <depth>
;     (probe current-node 'ww::wait '(1 ww::area4) 11)
;     (probe current-node 'ww::connect-to-2-terminus
;                         '(ww::CONNECTOR3 ww::TRANSMITTER1 ww::RECEIVER3 ww::AREA4) 10)
      
      (when (>= ww::*debug* 3)
        (format t "~%Current node selected:~%~S" current-node))
      (setq *succ-states* (get-successors current-node parent))
      (update1-*search-tree* current-node)
      (update-*max-depth-explored* current-node)
      (let (potential?)   ;current node's potential for further expansion
        (dolist (succ-state *succ-states*)
          (let (message)
            (if (ww::goal succ-state)
              (progn (register-solution current-node succ-state)
                     (setf message "***goal***")
                     (when ww::*first-solution-sufficient*
                       (ecase ww::*tree-or-graph*
                         (ww::graph (summarize-search-results-graph 'first))
                         (ww::tree (summarize-search-results-tree 'first)))
                       (return-from df-bnb ww::*solutions*)))
              (setf message (ecase ww::*tree-or-graph*
                              (ww::graph (process-successor-graph current-node succ-state))
                              (ww::tree (process-successor-tree current-node succ-state))))) ;string or t
            (when (eq message t)
              (setf potential? t))
            (update2-*search-tree* current-node succ-state message)))
        (check-for-barren-nodes current-node potential?))
      (when (>= ww::*debug* 3)
        (format t "-----------------------------------~%"))
      (when (= ww::*debug* 5)
        (break))
      (when (search-space-is-exhausted)
        (ecase ww::*tree-or-graph*
          (ww::graph (summarize-search-results-graph 'exhausted))
          (ww::tree (summarize-search-results-tree 'exhausted)))
        (return-from df-bnb ww::*solutions*)))))


(defun probe (current-node name instantiations depth)
  ;Breaks when the current node matches action name, instantiations, and depth.
  (let ((state (node-state current-node)))
    (when (and (eql (ww::problem-state-name state) name)
               (equal (ww::problem-state-instantiations state) instantiations)
               (= (node-depth current-node) depth))
    (setq ww::*debug* 5))))


(defun update-*max-depth-explored* (current-node)
  (let ((depth (node-depth current-node)))
    (if *succ-states*
      (when (> (1+ depth) *max-depth-explored*)
        (setf *max-depth-explored* (1+ depth)))
      (when (> depth *max-depth-explored*)
        (setf *max-depth-explored* depth)))))


(defun update1-*search-tree* (current-node)
  (when (= ww::*debug* 1)
    (let ((state (node-state current-node)))
      (push (list `(,(ww::problem-state-name state) 
                      ,@(ww::problem-state-instantiations state))
                  (node-depth current-node)
                  (if *succ-states*
                    ""
                    "No successor states"))
            *search-tree*)))
  (when (= ww::*debug* 2)
    (let ((state (node-state current-node)))
      (push (list `(,(ww::problem-state-name state) 
                    ,@(ww::problem-state-instantiations state))
                  (node-depth current-node)
                  (if *succ-states*
                    ""
                    "No successor states")
                  (ww::list-database (ww::problem-state-idb state)))
            *search-tree*))))


(defun update2-*search-tree* (current-node succ-state message)
  (when (and (= ww::*debug* 1) (stringp message))  ;only push deadend states here
    (push (list `(,(ww::problem-state-name succ-state)
                  ,@(ww::problem-state-instantiations succ-state))
                (1+ (node-depth current-node))
                message)
          *search-tree*))
  (when (and (= ww::*debug* 2) (stringp message))  ;only push deadend states here
    (push (list `(,(ww::problem-state-name succ-state)
                  ,@(ww::problem-state-instantiations succ-state))
                (1+ (node-depth current-node))
                message
                (ww::list-database (ww::problem-state-idb succ-state)))
          *search-tree*)))


(defun solve ()
  ;Runs a branch & bound search on the problem specification.
  (ww::initialize)
  (format t "~%working ...~%")
  (time (df-bnb))
  (ww::finalize)
  (in-package :ww))
