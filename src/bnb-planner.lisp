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
                 (format stream "NODE: STATE=~A~&   DEPTH=~:D ~%"   ;PARENT=~S~%"
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
 (function print-node ww::state-key))


(ecase ww::*tree-or-graph*
  (ww::graph (declaim (hash-table *closed*)))
  (ww::tree nil))


;;; Global Search Parameters


(defparameter *total-states-processed* 0)
  ;Count of states either newly generated, updated, or regenerated while searching.


(defparameter *succ-states* nil)
  ;List of the successor states of the current node.


(defparameter *program-cycles* 0)
  ;Increments each time the DFS program processes the next current node.


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

#|
(defun shallower-node (current-node succ-state)
  ;Determines if a successor state along the current path has already been visited.
  (declare (node current-node) (ww::problem-state succ-state))
  (do ((node current-node (node-parent node)))
      ((null (node-parent node)))
    (when (ww::same-state succ-state (node-state node))
      (return t))))      
|#

(ecase ww::*tree-or-graph*
  (ww::graph 
   (defun unique-states-encountered ()
     ;Count of how many unique states have been encountered during searching.
     (declare (special *open* *closed* ww::*solutions*))
     (+ (hs::hash-stack-count *open*)
        (1- (hash-table-count *closed*))
        (length ww::*solutions*))))
  (ww::tree (defun unique-states-encountered () nil)))


(ecase ww::*tree-or-graph*
  (ww::graph (defun summarize-search-results (condition)
               (declare (symbol condition) 
                       (special ww::*debug* ww::*depth-cutoff* *total-states-processed*
                                *program-cycles* *average-branching-factor* ww::*solutions*
                                *search-tree*))
               (case condition
                 (first
                  (format t "~%~%Search ended with first solution found.~%"))
                 (exhausted
                  (format t "~%~%Search process completed normally,~%")
                  (format t "examining every state up to the depth cutoff.~%")))
               (format t "~%Depth cutoff = ~:D~%" ww::*depth-cutoff*)
               (format t "~%Total states processed = ~:D~%" *total-states-processed*)
               (format t "~%Unique states encountered = ~:D~%" (unique-states-encountered))
               (format t "~%Program cycles = ~:D~%" *program-cycles*)
               (format t "~%Average branching factor = ~F~%" *average-branching-factor*)
               (if ww::*solutions*
                   (let ((shallowest-depth (reduce #'min ww::*solutions* :key #'first)))
                     (format t "~%Total solutions found = ~:D" (length ww::*solutions*))
                     (format t
                         "~%(Check ww::*solutions* for shortest solution to each distinct goal.)~%")
                     (format t "~%Shallowest solution depth = ~:D~%" shallowest-depth)
                     (format t "~%Shortest solution path length from start state to goal state:~%")
                       ;(let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
                       ;(set-pprint-dispatch 'real (lambda (stream value)
                       ;                             (format stream "~,2f" value)))
                     (in-package :ww)
                     (dolist (item (cddr (find shallowest-depth ww::*solutions* :key #'first)))
                       (write item :pretty t)
                       (terpri))
                     (in-package :bnb))
                       ;(terpri)))
                 (format t "~%No goals recorded.~2%"))
               (when (>= ww::*debug* 1)
                 (format t "~%Search tree:~%")
                 (loop for act in (cdr (nreverse *search-tree*))
                     do (if (= (length act) 2)
                            (format t "~vT~d:~a~%"
                              (* 4 (second act)) (second act) (first act))
                          (format t "~vT~d:~a ~a~%" 
                            (* 4 (second act)) (second act) (first act) (third act)))
                     finally (terpri)))))
  (ww::tree (defun summarize-search-results (condition)
              (declare (symbol condition) 
                       (special ww::*debug* ww::*depth-cutoff* *total-states-processed*
                                *program-cycles* *average-branching-factor* ww::*solutions*
                                *search-tree*))
              (case condition
                (first
                 (format t "~%~%Search ended with first solution found.~%"))
                (exhausted
                 (format t "~%~%Search process completed normally,~%")
                 (format t "examining every state up to the depth cutoff.~%")))
              (format t "~%Depth cutoff = ~:D~%" ww::*depth-cutoff*)
              (format t "~%Total states processed = ~:D~%" *total-states-processed*)
              ;  (format t "~%Unique states encountered = ~:D~%" (unique-states-encountered))
              (format t "~%Program cycles = ~:D~%" *program-cycles*)
              (format t "~%Average branching factor = ~F~%" *average-branching-factor*)
              (if ww::*solutions*
                  (let ((shallowest-depth (reduce #'min ww::*solutions* :key #'first)))
                    (format t "~%Total solutions found = ~:D" (length ww::*solutions*))
                    (format t
                        "~%(Check ww::*solutions* for list of solutions if more than one solution found.)~%")
                    (format t "~%Shallowest solution depth = ~:D~%" shallowest-depth)
                    (format t "~%Shortest solution path length from start state to goal state:")
                    ;(let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
                    ;   (set-pprint-dispatch 'real (lambda (stream value)
                    ;                                (format stream "~,2f" value)))
                    (in-package :ww)
                    (dolist (item (cddr (find shallowest-depth ww::*solutions* :key #'first)))
                      (write item :pretty t)
                      (terpri))
                    (in-package :bnb))
                       ;(terpri)))
                (format t "~%No goals recorded.~%"))
              (when (>= ww::*debug* 1)
                 (format t "~%Search tree:~%")
                 (loop for act in (cdr (nreverse *search-tree*))
                     do (if (= (length act) 2)
                            (format t "~vT~d:~a~%"
                              (* 4 (second act)) (second act) (first act))
                          (format t "~vT~d:~a ~a~%" 
                            (* 4 (second act)) (second act) (first act) (third act)))
                     finally (terpri))))))



(defun register-solution-if-new (current-node goal-state)
  ;Inserts a new goal on the list of solutions, or replaces a poorer equivalent
  ;goal. A solution is a list of 3+ items: depth, goal state, multi-item path
  ;to goal state.
  (declare (node current-node) (ww::problem-state goal-state)
           (special ww::*debug* ww::*solutions* *search-tree*))
  (let ((current-solution
         (nconc (list (1+ (node-depth current-node)))
                (list goal-state)
                (record-solution-path current-node)
                (list (ww::record-move (node-state current-node) goal-state)))) ;add final state
        (previous-solution
         (find goal-state ww::*solutions* :test #'ww::same-state :key #'second)))
    (if previous-solution
      (when (< (first current-solution) (first previous-solution))
        (format t "~%~%Shortening a previous path to the goal, depth = ~:D~%"
          (1+ (node-depth current-node)))
        (setf ww::*solutions*
          (substitute current-solution (second previous-solution) ww::*solutions*
                      :test #'ww::same-state :key #'second :count 1)))
      (progn (format t "~%~%New path to goal found at depth = ~:D~%"
                       (1+ (node-depth current-node)))
             (push current-solution ww::*solutions*))))
  (when (>= ww::*debug* 1)
    (format t "~%Goal state processed:~%~S~%" goal-state)))
 

(ecase ww::*tree-or-graph*
  (ww::graph (defun print-search-progress ()
               ;Printout # of nodes expanded so far during search modulo reporting interval.
               (declare (special *total-states-processed* ww::*progress-reporting-interval*
                                 *average-branching-factor*))
               (when (= 0 (mod *total-states-processed* ww::*progress-reporting-interval*))
                 ;(sb-ext:gc)
                 (format t "~%total states processed so far = ~:D, unique states encountered = ~:D"
                   *total-states-processed* (unique-states-encountered))
                 (format t "~%average branching factor = ~F~%" *average-branching-factor*))))
  (ww::tree (defun print-search-progress ()
              ;Printout # of nodes expanded so far during search modulo reporting interval.
              (declare (special *total-states-processed* ww::*progress-reporting-interval*
                                *average-branching-factor*))
              (when (= 0 (mod *total-states-processed* ww::*progress-reporting-interval*))
                ;(sb-ext:gc)
                (format t "~%total states processed so far = ~:D" *total-states-processed*)
                (format t "~%average branching factor = ~F~%" *average-branching-factor*)))))


;;; Search Functions


(ecase ww::*tree-or-graph*
  (ww::graph (defun initialize-search ()
               (declare (special ww::*solutions* *open* ww::*max-states* ww::*start-state*
                                *search-tree* *succ-states* *program-cycles* 
                                 *average-branching-factor* *total-states-processed*))
               (setq ww::*solutions* nil)
               (setq *open* (hs::create-hash-stack (/ ww::*max-states* 1000)
                                                   #'equalp #'node-key))
               (hs::push-onto-hash-stack (make-node :state ww::*start-state*) *open*)
               (setq *closed* (make-hash-table :size ww::*max-states* :test #'equalp))
               (setq *search-tree* nil)
               (setq *succ-states* nil)
               (setq *program-cycles* 0)
               (setq *average-branching-factor* 0.0)
               (setq *total-states-processed* 1)))
  (ww::tree (defun initialize-search ()
              (declare (special ww::*solutions* *open* ww::*max-states* ww::*start-state*
                                *search-tree* *succ-states* *program-cycles* 
                                *average-branching-factor* *total-states-processed*))
              (setq ww::*solutions* nil)
              (setq *open* (hs::create-hash-stack (/ ww::*max-states* 1000)
                                                  #'equalp #'node-key))
              (hs::push-onto-hash-stack (make-node :state ww::*start-state*) *open*)
              (setq *search-tree* nil)
              (setq *succ-states* nil)
              (setq *program-cycles* 0)
              (setq *average-branching-factor* 0.0)
              (setq *total-states-processed* 1))))



(defun search-space-is-exhausted ()
  ;Determines if set of open nodes is empty.
  (declare (special *open*))
  (hs::hash-stack-is-empty *open*))



(defun at-max-depth (current-node)
  ;Determines if installing a nongoal successor to the current node will be
  ;pointless, based on it being at the max allowable depth.
  (declare (node current-node) (special ww::*depth-cutoff*))
  (when (> ww::*depth-cutoff* 0)
    (= (node-depth current-node) (1- ww::*depth-cutoff*))))



(defun get-next-node-for-expansion ()
  ;Returns the node at the top of open.
  (declare (special *open*))
  (hs::peek-at-hash-stack 0 *open*))  ;top of stack



(defun best-states-last (state1 state2)
  ;Used to sort a list of expanded states according to the user-defined heuristic.
  (declare (ww::problem-state state1 state2))
  (> (ww::estimate-to-goal state1) (ww::estimate-to-goal state2)))



(ecase ww::*tree-or-graph*
  (ww::graph (defun close-barren-nodes (current-node potential?)
               ;Move nodes from open to closed if barren.
               (declare (node current-node) ((or nil t) potential?)
                        (special ww::*debug* *open* *closed*))
               (do ((node current-node)
                    parent)
                   ((or (null node) (not (eq node (hs::peek-at-hash-stack 0 *open*)))))
                 (hs::pop-from-hash-stack *open*)
                 (setf (gethash (ww::state-key (node-state node)) *closed*)
                   (node-depth node))
;                  (if potential? (node-depth node) 0))
                 (setq parent (node-parent node))
                 (setf (node-parent node) nil)
                 (when (>= ww::*debug* 3)
                   (format t "~2%Node closed by ~S:~%~S~%"
                     (if potential? "cutoff" "deadend") node))
                 (setq node parent))))
  (ww::tree (defun close-barren-nodes (current-node potential?)
              ;Move nodes from open to closed if barren.
              (declare (node current-node) ((or nil t) potential?)
                       (special ww::*debug* *open*))
              (do ((node current-node)
                   parent)
                  ((or (null node) (not (eq node (hs::peek-at-hash-stack 0 *open*)))))
                (hs::pop-from-hash-stack *open*)
                (setq parent (node-parent node))
                (setf (node-parent node) nil)
                (when (>= ww::*debug* 3)
                  (format t "~%Node closed by ~S:~%~S~%"
                    (if potential? "cutoff" "deadend") node))
                (setq node parent)))))



(defun generate-new-node (current-node succ-state)
  ;Produces a new node for a given successor.
  (declare (node current-node) (ww::problem-state succ-state) (special ww::*debug* *open*))
  (let ((succ-node (make-node :state succ-state
                              :depth (1+ (node-depth current-node))
                              :parent current-node)))
    (when (>= ww::*debug* 2)
      (format t "~2%Installing new or updated successor:~%~S" succ-node))
    (hs::push-onto-hash-stack succ-node *open*)))


(ecase ww::*tree-or-graph*
  (ww::graph (defun process-successor (current-node succ-state)
               ;Decides how to process the next successor state. Returns whether or not the
               ;current node still has life (ie, potential successors).
               (declare (node current-node) (ww::problem-state succ-state)
                        (special ww::*debug* *total-states-processed* *open* *closed*))
               (incf *total-states-processed*)
               (print-search-progress)       ;#nodes expanded so far
               (cond ((at-max-depth current-node)
                      (when (>= ww::*debug* 2)
                        (format t "~%Deadend--state at max depth:~%~A" succ-state)) 
                      "Deadend--state at max depth")
                     ((hs::find-in-hash-stack (ww::state-key succ-state) *open*)
                      (when (>= ww::*debug* 2)
                        (format t "~%Deadend--state already waiting in stack:~%~A" succ-state))
                      "Deadend--state already waiting in stack")
                     ((not (gethash (ww::state-key succ-state) *closed*)) ;not on *open* or *closed*
                      (generate-new-node current-node succ-state)
                      t)
                     ((>= (1+ (node-depth current-node))  ;depth greater than already closed state
                          (gethash (ww::state-key succ-state) *closed*))
                      (when (>= ww::*debug* 2)
                        (format t "~%Deadend--shorter path to state exists:~%~A" succ-state))
                      "Deadend--shorter path to state exists")
;                    ((= 0 (gethash (ww::state-key succ-state) *closed*)) ;previously closed barren
;                     "Deadend--previously closed barren state")
                     ((< (1+ (node-depth current-node))  
                         (gethash (ww::state-key succ-state) *closed*))
                      (remhash (ww::state-key succ-state) *closed*) ;shallower node found
                      (generate-new-node current-node succ-state) ;put back on open
                      t)
                     (t (error "In process-successor")))))
  (ww::tree (defun process-successor (current-node succ-state)
              ;Decides how to process the next successor state. Returns whether or not the
              ;current node still has life (ie, potential successors).
              (declare (node current-node) (ww::problem-state succ-state)
                       (special ww::*debug* *total-states-processed* *open*))
              (incf *total-states-processed*)
              (print-search-progress)       ;#nodes expanded so far
              (cond ((hs::find-in-hash-stack succ-state *open*)
                     nil)              ;close deadend
                    ((at-max-depth current-node)
                     (when (>= ww::*debug* 2)
                        (format t "~%Deadend--at max depth:~%~A" succ-state))
                     t)
                    (t (generate-new-node current-node succ-state)    ;better node found
                       t)))))              ;leave on open


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
  (declare (special *open* *average-branching-factor* *succ-states* *program-cycles*
                    *average-branching-factor*))
  (if (eq current-node (hs::peek-at-hash-stack 0 *open*)) ;still at top of stack
      (close-barren-nodes current-node potential?)
    (setq *average-branching-factor*    ;cumulative average of expanded states
          (/ (+ (length *succ-states*)
                (* *program-cycles* *average-branching-factor*))
             (incf *program-cycles*)))))


(defun df-bnb ()
  ;Branch & Bound algorithm. Keeps searching for improvements to
  ;an initial solution, at user's request.
  ;Solutions stored in ww::*solutions*.
  (declare (special ww::*debug* ww::*solutions* *succ-states* ww::*first-solution-sufficient*))
  (initialize-search)
  (loop
    (let* ((current-node (get-next-node-for-expansion))
           (parent (node-parent current-node)))
      ;Stop at specified node, for debugging
;      (when (let ((state (node-state current-node)))
;              (and (eq (ww::problem-state-name state) 'ww::move)
;                   (equal (ww::problem-state-instantiations state)
;                          '(ww::area2 ww::area4))
;                   (= (node-depth current-node) 10)))
;        (setq ww::*debug* 4))
      (when (>= ww::*debug* 2)
        (format t "~%Current node selected:~%~S" current-node))
      (when (>= ww::*debug* 1)
        (let ((state (node-state current-node)))
          (push (list `(,(ww::problem-state-name state) 
                          ,@(ww::problem-state-instantiations state))
                      (node-depth current-node))
                *search-tree*)))
      (setq *succ-states* (get-successors current-node parent))     ;find all children
      (let (potential?)   ;current node's potential for further expansion
        (dolist (succ-state *succ-states*)
          (let (message)
            (if (ww::goal succ-state)
              (progn (register-solution-if-new current-node succ-state)
                     (setf message "***goal***")
                     (when ww::*first-solution-sufficient*
                       (summarize-search-results 'first)
                       (return-from df-bnb ww::*solutions*)))
              (setf message (process-successor current-node succ-state))) ;string or t
            (when (eq message t) (setf potential? t))
            (when (and (>= ww::*debug* 1) (stringp message))  ;only push deadend states here
              (push (list `(,(ww::problem-state-name succ-state)
                               ,@(ww::problem-state-instantiations succ-state))
                          (1+ (node-depth current-node))
                          message)
                    *search-tree*))))
        (check-for-barren-nodes current-node potential?))
      (when (>= ww::*debug* 2)
        (format t "-----------------------------------~%"))
      (when (>= ww::*debug* 4)
        (break))
      (when (search-space-is-exhausted)
        (summarize-search-results 'exhausted)
        (return-from df-bnb ww::*solutions*)))))


(defun solve ()
  ;Runs a branch & bound search on the problem specification.
  (ww::initialize)
  (time (df-bnb))
  (ww::finalize))
