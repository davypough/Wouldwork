;;; Filename:  BNB.LSP

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
  (state   nil :type pl::problem_state)    ;problem state
  (depth   0   :type fixnum)           ;depth in the search tree
  (parent  nil :type (or null node)))  ;this node's parent



(declaim
 (fixnum pl::*progress_reporting_interval* pl::*max_states* pl::*depth_cutoff*)
 (single-float *average_branching_factor*)
 (list *solutions* *succ_states* *search_tree*)
 (pl::problem_state pl::*start_state*)
 (hs::hash_stack *open*)
 (function print_node pl::state_key))


(ecase pl::*tree_or_graph*
  (pl::graph (declaim (hash-table *closed*)))
  (pl::tree nil))


;;; Global Search Parameters


(defparameter *total_states_processed* 0)
  ;Count of states either newly generated, updated, or regenerated while searching.


(defparameter *solutions* nil)
  ;The resulting list of solutions found.


(defparameter *succ_states* nil)
  ;List of the successor states of the current node.


(defparameter *program_cycles* 0)
  ;Increments each time the DFS program processes the next current node.


(defparameter *average_branching_factor* 0.0)
  ;Average branching factor so far during search.


(defvar *open*)
  ;The hash-stack structure containing the set of open nodes.


(defvar *closed*)
 ;The set of closed states.


(defparameter *search_tree* nil)


;;; Utility Functions



(defun node_key (node)
  ;Gets the hash key for looking up a node in the open set.
  (declare (node node))
  (pl::state_key (node-state node)))



;;; Solution Processing Functions


(defun record_solution_path (goal_node)
  ;Recovers a path from a goal node back to the start node following parent links.
  (declare (node goal_node))
  (let ((path nil))
    (do ((n goal_node (node-parent n)))
        ((null (node-parent n)))
      (push (pl::record_move (node-state (node-parent n)) (node-state n))
            path))
    (return-from record_solution_path path)))

#|
(defun shallower_node (current_node succ_state)
  ;Determines if a successor state along the current path has already been visited.
  (declare (node current_node) (pl::problem_state succ_state))
  (do ((node current_node (node-parent node)))
      ((null (node-parent node)))
    (when (pl::same_state succ_state (node-state node))
      (return t))))      
|#

(ecase pl::*tree_or_graph*
  (pl::graph 
   (defun unique_states_encountered ()
     ;Count of how many unique states have been encountered during searching.
     (declare (special *open* *closed* *solutions*))
     (+ (hs::hash_stack_count *open*)
        (1- (hash-table-count *closed*))
        (length *solutions*))))
  (pl::tree (defun unique_states_encountered () nil)))


(ecase pl::*tree_or_graph*
  (pl::graph (defun summarize_search_results (condition)
               (declare (symbol condition) 
                       (special pl::*debug* pl::*depth_cutoff* *total_states_processed*
                                *program_cycles* *average_branching_factor* *solutions*
                                *search_tree*))
               (case condition
                 (first
                  (format t "~%~%Search ended with first solution found.~%"))
                 (exhausted
                  (format t "~%~%Search process completed normally,~%")
                  (format t "examining every state up to the depth cutoff.~%")))
               (format t "~%Depth cutoff = ~:D~%" pl::*depth_cutoff*)
               (format t "~%Total states processed = ~:D~%" *total_states_processed*)
               (format t "~%Unique states encountered = ~:D~%" (unique_states_encountered))
               (format t "~%Program cycles = ~:D~%" *program_cycles*)
               (format t "~%Average branching factor = ~F~%" *average_branching_factor*)
               (if *solutions*
                   (let ((shallowest_depth (reduce #'min *solutions* :key #'first)))
                     (format t "~%Total solutions found = ~:D" (length *solutions*))
                     (format t
                         "~%(Check bnb::*solutions* for best solution to each distinct goal.)~%")
                     (format t "~%Shallowest solution depth = ~:D~%" shallowest_depth)
                     (format t "~%Shortest solution path length from start state to goal state:~%")
                     (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
                       (set-pprint-dispatch 'real (lambda (stream value)
                                                    (format stream "~,2f" value)))
                       (in-package :pl)
                       (dolist (item (cddr (find shallowest_depth *solutions* :key #'first)))
                         (write item :pretty t)
                         (terpri))
                       (in-package :bnb)
                       (terpri)))
                 (format t "~%No goals recorded.~2%"))
               (when (>= pl::*debug* 1)
                 (format t "~%Search tree:~%")
                 (loop for act in (cdr (nreverse *search_tree*))
                     do (if (= (length act) 2)
                            (format t "~vT~d:~a~%"
                              (* 4 (second act)) (second act) (first act))
                          (format t "~vT~d:~a ~a~%" 
                            (* 4 (second act)) (second act) (first act) (third act)))
                     finally (terpri)))))
  (pl::tree (defun summarize_search_results (condition)
              (declare (symbol condition) 
                       (special pl::*debug* pl::*depth_cutoff* *total_states_processed*
                                *program_cycles* *average_branching_factor* *solutions*
                                *search_tree*))
              (case condition
                (first
                 (format t "~%~%Search ended with first solution found.~%"))
                (exhausted
                 (format t "~%~%Search process completed normally,~%")
                 (format t "examining every state up to the depth cutoff.~%")))
              (format t "~%Depth cutoff = ~:D~%" pl::*depth_cutoff*)
              (format t "~%Total states processed = ~:D~%" *total_states_processed*)
              ;  (format t "~%Unique states encountered = ~:D~%" (unique_states_encountered))
              (format t "~%Program cycles = ~:D~%" *program_cycles*)
              (format t "~%Average branching factor = ~F~%" *average_branching_factor*)
              (if *solutions*
                  (let ((shallowest_depth (reduce #'min *solutions* :key #'first)))
                    (format t "~%Total solutions found = ~:D" (length *solutions*))
                    (format t
                        "~%(Check bnb::*solutions* for list of solutions if more than one solution found.)~%")
                    (format t "~%Shallowest solution depth = ~:D~%" shallowest_depth)
                    (format t "~%Shortest solution path length from start state to goal state:")
                    (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
                       (set-pprint-dispatch 'real (lambda (stream value)
                                                    (format stream "~,2f" value)))
                       (in-package :pl)
                       (dolist (item (cddr (find shallowest_depth *solutions* :key #'first)))
                         (write item :pretty t)
                         (terpri))
                       (in-package :bnb)
                       (terpri)))
                (format t "~%No goals recorded.~%"))
              (when (>= pl::*debug* 1)
                 (format t "~%Search tree:~%")
                 (loop for act in (cdr (nreverse *search_tree*))
                     do (if (= (length act) 2)
                            (format t "~vT~d:~a~%"
                              (* 4 (second act)) (second act) (first act))
                          (format t "~vT~d:~a ~a~%" 
                            (* 4 (second act)) (second act) (first act) (third act)))
                     finally (terpri))))))



(defun register_solution_if_new (current_node goal_state)
  ;Inserts a new goal on the list of solutions, or replaces a poorer equivalent
  ;goal. A solution is a list of 3+ items: depth, goal state, multi-item path
  ;to goal state.
  (declare (node current_node) (pl::problem_state goal_state)
           (special pl::*debug* *solutions* *search_tree*))
  (let ((current_solution
         (nconc (list (1+ (node-depth current_node)))
                (list goal_state)
                (record_solution_path current_node)
                (list (pl::record_move (node-state current_node) goal_state)))) ;final state
        (previous_solution
         (find goal_state *solutions* :test #'pl::same_state :key #'second)))
    (if previous_solution
      (when (< (first current_solution) (first previous_solution))
        (format t "~%~%Shortening a previous path to the goal, depth = ~:D~%"
          (1+ (node-depth current_node)))
        (setf *solutions*
          (substitute current_solution (second previous_solution) *solutions*
                      :test #'pl::same_state :key #'second :count 1)))
      (progn (format t "~%~%New path to goal found at depth = ~:D~%"
               (1+ (node-depth current_node)))
             (push current_solution *solutions*))))
  (when (>= pl::*debug* 1)
    (format t "~%Goal state processed:~%~S~%" goal_state)))
 

(ecase pl::*tree_or_graph*
  (pl::graph (defun print_search_progress ()
               ;Printout # of nodes expanded so far during search modulo reporting interval.
               (declare (special *total_states_processed* pl::*progress_reporting_interval*
                                 *average_branching_factor*))
               (when (= 0 (mod *total_states_processed* pl::*progress_reporting_interval*))
                 ;(sb-ext:gc)
                 (format t "~%total states processed so far = ~:D, unique states encountered = ~:D"
                   *total_states_processed* (unique_states_encountered))
                 (format t "~%average branching factor = ~F~%" *average_branching_factor*))))
  (pl::tree (defun print_search_progress ()
              ;Printout # of nodes expanded so far during search modulo reporting interval.
              (declare (special *total_states_processed* pl::*progress_reporting_interval*
                                *average_branching_factor*))
              (when (= 0 (mod *total_states_processed* pl::*progress_reporting_interval*))
                ;(sb-ext:gc)
                (format t "~%total states processed so far = ~:D" *total_states_processed*)
                (format t "~%average branching factor = ~F~%" *average_branching_factor*)))))


;;; Search Functions


(ecase pl::*tree_or_graph*
  (pl::graph (defun initialize_search ()
               (declare (special *solutions* *open* pl::*max_states* pl::*start_state*
                                *search_tree* *succ_states* *program_cycles* 
                                 *average_branching_factor* *total_states_processed*))
               (setq *solutions* nil)
               (setq *open* (hs::create_hash_stack (/ pl::*max_states* 1000)
                                                   #'equalp #'node_key))
               (hs::push_onto_hash_stack (make-node :state pl::*start_state*) *open*)
               (setq *closed* (make-hash-table :size pl::*max_states* :test #'equalp))
               (setq *search_tree* nil)
               (setq *succ_states* nil)
               (setq *program_cycles* 0)
               (setq *average_branching_factor* 0.0)
               (setq *total_states_processed* 1)))
  (pl::tree (defun initialize_search ()
              (declare (special *solutions* *open* pl::*max_states* pl::*start_state*
                                *search_tree* *succ_states* *program_cycles* 
                                *average_branching_factor* *total_states_processed*))
              (setq *solutions* nil)
              (setq *open* (hs::create_hash_stack (/ pl::*max_states* 1000)
                                                  #'equalp #'node_key))
              (hs::push_onto_hash_stack (make-node :state pl::*start_state*) *open*)
              (setq *search_tree* nil)
              (setq *succ_states* nil)
              (setq *program_cycles* 0)
              (setq *average_branching_factor* 0.0)
              (setq *total_states_processed* 1))))



(defun search_space_is_exhausted ()
  ;Determines if set of open nodes is empty.
  (declare (special *open*))
  (hs::hash_stack_is_empty *open*))



(defun at_max_depth (current_node)
  ;Determines if installing a nongoal successor to the current node will be
  ;pointless, based on it being at the max allowable depth.
  (declare (node current_node) (special pl::*depth_cutoff*))
  (when (> pl::*depth_cutoff* 0)
    (= (node-depth current_node) (1- pl::*depth_cutoff*))))



(defun get_next_node_for_expansion ()
  ;Returns the node at the top of open.
  (declare (special *open*))
  (hs::peek_at_hash_stack 0 *open*))  ;top of stack



(defun best_states_last (state1 state2)
  ;Used to sort a list of expanded states according to the user-defined heuristic.
  (declare (pl::problem_state state1 state2))
  (> (pl::estimate_to_goal state1) (pl::estimate_to_goal state2)))



(ecase pl::*tree_or_graph*
  (pl::graph (defun close_barren_nodes (current_node potential?)
               ;Move nodes from open to closed if barren.
               (declare (node current_node) ((or nil t) potential?)
                        (special pl::*debug* *open* *closed*))
               (do ((node current_node)
                    parent)
                   ((or (null node) (not (eq node (hs::peek_at_hash_stack 0 *open*)))))
                 (hs::pop_from_hash_stack *open*)
                 (setf (gethash (pl::state_key (node-state node)) *closed*)
                   (node-depth node))
;                  (if potential? (node-depth node) 0))
                 (setq parent (node-parent node))
                 (setf (node-parent node) nil)
                 (when (>= pl::*debug* 3)
                   (format t "~2%Node closed by ~S:~%~S~%"
                     (if potential? "cutoff" "deadend") node))
                 (setq node parent))))
  (pl::tree (defun close_barren_nodes (current_node potential?)
              ;Move nodes from open to closed if barren.
              (declare (node current_node) ((or nil t) potential?)
                       (special pl::*debug* *open*))
              (do ((node current_node)
                   parent)
                  ((or (null node) (not (eq node (hs::peek_at_hash_stack 0 *open*)))))
                (hs::pop_from_hash_stack *open*)
                (setq parent (node-parent node))
                (setf (node-parent node) nil)
                (when (>= pl::*debug* 3)
                  (format t "~%Node closed by ~S:~%~S~%"
                    (if potential? "cutoff" "deadend") node))
                (setq node parent)))))



(defun generate_new_node (current_node succ_state)
  ;Produces a new node for a given successor.
  (declare (node current_node) (pl::problem_state succ_state) (special pl::*debug* *open*))
  (let ((succ_node (make-node :state succ_state
                              :depth (1+ (node-depth current_node))
                              :parent current_node)))
    (when (>= pl::*debug* 2)
      (format t "~2%Installing new or updated successor:~%~S" succ_node))
    (hs::push_onto_hash_stack succ_node *open*)))


(ecase pl::*tree_or_graph*
  (pl::graph (defun process_successor (current_node succ_state)
               ;Decides how to process the next successor state. Returns whether or not the
               ;current node still has life (ie, potential successors).
               (declare (node current_node) (pl::problem_state succ_state)
                        (special pl::*debug* *total_states_processed* *open* *closed*))
               (incf *total_states_processed*)
               (print_search_progress)       ;#nodes expanded so far
               (cond ((at_max_depth current_node)
                      (when (>= pl::*debug* 2)
                        (format t "~%Deadend--state at max depth:~%~A" succ_state)) 
                      "Deadend--state at max depth")
                     ((hs::find_in_hash_stack (pl::state_key succ_state) *open*)
                      (when (>= pl::*debug* 2)
                        (format t "~%Deadend--state already waiting in stack:~%~A" succ_state))
                      "Deadend--state already waiting in stack")
                     ((not (gethash (pl::state_key succ_state) *closed*)) ;not on *open* or *closed*
                      (generate_new_node current_node succ_state)
                      t)
                     ((>= (1+ (node-depth current_node))  ;depth greater than already closed state
                          (gethash (pl::state_key succ_state) *closed*))
                      (when (>= pl::*debug* 2)
                        (format t "~%Deadend--shorter path to state exists:~%~A" succ_state))
                      "Deadend--shorter path to state exists")
;                    ((= 0 (gethash (pl::state_key succ_state) *closed*)) ;previously closed barren
;                     "Deadend--previously closed barren state")
                     ((< (1+ (node-depth current_node))  
                         (gethash (pl::state_key succ_state) *closed*))
                      (remhash (pl::state_key succ_state) *closed*) ;shallower node found
                      (generate_new_node current_node succ_state) ;put back on open
                      t)
                     (t (error "In process_successor")))))
  (pl::tree (defun process_successor (current_node succ_state)
              ;Decides how to process the next successor state. Returns whether or not the
              ;current node still has life (ie, potential successors).
              (declare (node current_node) (pl::problem_state succ_state)
                       (special pl::*debug* *total_states_processed* *open*))
              (incf *total_states_processed*)
              (print_search_progress)       ;#nodes expanded so far
              (cond ((hs::find_in_hash_stack succ_state *open*)
                     nil)              ;close deadend
                    ((at_max_depth current_node)
                     (when (>= pl::*debug* 2)
                        (format t "~%Deadend--at max depth:~%~A" succ_state))
                     t)
                    (t (generate_new_node current_node succ_state)    ;better node found
                       t)))))              ;leave on open


(defun get_successors (current_node parent)
  ;Returns children states of current node (unless same as parent).
  (let ((states (coerce (if parent
                            (remove (node-state parent) (pl::expand (node-state current_node))
                                    :test #'pl::same_state :count 1)
                            (pl::expand (node-state current_node)))
                        'list)))
;    (ut::prt states) (ut::prt parent)
;    (ut::sortf states #'(lambda (x y)
;                          (declare (ignore y))
;                          (not (eq x (pl::problem_state-name (node-state parent)))))
;               :key #'pl::problem_state-name)
;    (ut::prt states)
;    (terpri) (terpri)
    states))


(defun check_for_barren_nodes (current_node potential?)
  ;Backtrack to close barrens.
  (declare (special *open* *average_branching_factor* *succ_states* *program_cycles*
                    *average_branching_factor*))
  (if (eq current_node (hs::peek_at_hash_stack 0 *open*)) ;still at top of stack
      (close_barren_nodes current_node potential?)
    (setq *average_branching_factor*    ;cumulative average of expanded states
          (/ (+ (length *succ_states*)
                (* *program_cycles* *average_branching_factor*))
             (incf *program_cycles*)))))


(defun df_bnb ()
  ;Branch & Bound algorithm. Keeps searching for improvements to
  ;an initial solution, at user's request.
  ;Solutions stored in *solutions*.
  (declare (special pl::*debug* *solutions* *succ_states* pl::*first_solution_sufficient*))
  (initialize_search)
  (loop
    (let* ((current_node (get_next_node_for_expansion))
           (parent (node-parent current_node)))
      ;Stop at specified node, for debugging
;      (when (let ((state (node-state current_node)))
;              (and (eq (pl::problem_state-name state) 'pl::move)
;                   (equal (pl::problem_state-instantiations state)
;                          '(pl::area2 pl::area4))
;                   (= (node-depth current_node) 10)))
;        (setq pl::*debug* 4))
      (when (>= pl::*debug* 2)
        (format t "~%Current node selected:~%~S" current_node))
      (when (>= pl::*debug* 1)
        (let ((state (node-state current_node)))
          (push (list `(,(pl::problem_state-name state) 
                          ,@(pl::problem_state-instantiations state))
                      (node-depth current_node))
                *search_tree*)))
      (setq *succ_states* (get_successors current_node parent))     ;find all children
      (let (potential?)   ;current node's potential for further expansion
        (dolist (succ_state *succ_states*)
          (let (message)
            (if (pl::goal succ_state)
              (progn (register_solution_if_new current_node succ_state)
                     (setf message "***goal***")
                     (when pl::*first_solution_sufficient*
                       (summarize_search_results 'first)
                       (return-from df_bnb *solutions*)))
              (setf message (process_successor current_node succ_state))) ;string or t
            (when (eq message t) (setf potential? t))
            (when (and (>= pl::*debug* 1) (stringp message))  ;only push deadend states here
              (push (list `(,(pl::problem_state-name succ_state)
                               ,@(pl::problem_state-instantiations succ_state))
                          (1+ (node-depth current_node))
                          message)
                    *search_tree*))))
        (check_for_barren_nodes current_node potential?))
      (when (>= pl::*debug* 2)
        (format t "-----------------------------------~%"))
      (when (>= pl::*debug* 4)
        (break))
      (when (search_space_is_exhausted)
        (summarize_search_results 'exhausted)
        (return-from df_bnb *solutions*)))))


(defun solve ()
  ;Runs a branch & bound search on the problem specification.
  (pl::initialize)
  (time (df_bnb))
  (pl::finalize))
