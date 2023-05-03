;;; Filename:  ww-successors.lisp

;;; Processes the successors to the current node.
;;; This file needs to be compiled with (debug 3).


(in-package :ww)


(defmacro process-goal-code ()
  "Inserts the proper code for each *solution-type* into detect-goals
   for each goal."
  (ecase *solution-type*
    (first
      `(register-solution current-node state))
    (every
      `(register-solution current-node state))
    (count  ;count solution, but don't save it
      `(increment-global-fixnum *solution-count*))
    (min-length
      `(when (or (null *solutions*)
                 (< (1+ (node.depth current-node))
                    (solution.depth (first *solutions*))))
         (register-solution current-node state)))
    (min-time
      `(when (or (null *solutions*)
                 (< (problem-state.time state)  ;better time found
                    (solution.time (first *solutions*))))
         (register-solution current-node state)))
    (min-value
      `(when (or (null *solutions*)
                 (< (problem-state.value state)
                    (solution.value (first *solutions*))))
         (register-solution current-node state)))
    (max-value
      `(when (or (null *solutions*)
                 (> (problem-state.value state)
                    (solution.value (first *solutions*))))
         (register-solution current-node state)))))


(defun detect-goals (current-node succ-states)
  ;Must be compiled with (debug 3)
  (declare (optimize (debug 3)))
  (declare (list succ-states) (ignorable current-node))
  (iter (for state in succ-states)
     (cond ((goal state)
              (collect state into goal-succ-states)
              (process-goal-code))  ;code inserted from above
           (t (collect state into nongoal-succ-states)))
     (when (null (get '*goal* 'formula))  ;looking for best results rather than goals
       (let ((current-value (problem-state.value state))
             (best-value (problem-state.value (car *best-states*))))
         (case *solution-type*
           (max-value (cond ((> current-value best-value)
                               (protect #'format t "~%Higher value state found: ~A~%" (problem-state.value state))
                               (push-global state *best-states*))
                            ((= current-value best-value)
                               (push-global state *best-states*)))
                      (when (<= current-value best-value) ;state is not strictly better
                        #+:wouldwork-debug (when (>= *debug* 3)
                                             (format t "~%Better path to state already exists:~%~A~%" state))))
                        ;(update-search-tree state (1+ (node.depth current-node)) "Better path to state already exists")))
           (min-value (cond ((< current-value best-value)
                               (protect #'format t "~%Lower value state found: ~A~%" (problem-state.value state))
                               (push-global state *best-states*))
                            ((= current-value best-value)
                               (push-global state *best-states*)))
                      (when (>= current-value best-value) ;state is not strictly better
                        #+:wouldwork-debug (when (>= *debug* 3)
                                             (format t "~2%Better path to state already exists:~%~A~%" state)))))))
                        ;(update-search-tree state (1+ (node.depth current-node)) "Better path to state already exists"))))))
     (finally (return (values goal-succ-states nongoal-succ-states)))))


(defun process-nongoal-succ-states (current-node nongoal-succ-states)
  "Determines how to handle nongoal succ states for both tree or graph search."
  (declare (node current-node) (list nongoal-succ-states))
  (loop with succ-nodes
        for state in nongoal-succ-states
        do (ut::mvb (message succ-node)   
               (ecase *tree-or-graph*
                 (graph (process-successor-graph current-node state))  ;rtn new node or nil
                 (tree (process-successor-tree current-node state)))  ;rtn new node or nil  
             (if succ-node
               (push succ-node succ-nodes)
               (when (>= *debug* 1)
                 (update-search-tree state (1+ (node.depth current-node)) message))))
        finally (return succ-nodes)))  ;rtn succ-nodes to searcher.lisp

#|
(defmacro insert-graph-code ()
  "Inserts the proper code for each *solution-type* into process-successor-graph.
   Analyzes states already visited to determine if a successor can be pruned."
  (ecase *solution-type*
    ((first every count min-length)
      `(let ((succ-depth (1+ (node.depth current-node))))
         (cond ((>= succ-depth (second prior-dead-depth-time-val))
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "~2%Same or shorter path length to state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or shorter path length to state already exists" nil)))
               (t (setf (second prior-dead-depth-time-val) succ-depth)))))
    (min-time
      `(let ((succ-time (problem-state.time succ-state)))
         (cond ((>= succ-time (third prior-dead-depth-time-val))
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "~2%Same or shorter time to state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or shorter time to state already exists" nil)))
               (t (setf (third prior-dead-depth-time-val) succ-time)))))
    (min-value
      `(let ((succ-value (problem-state.value succ-state)))
         (cond ((>= succ-value (fourth prior-dead-depth-time-val))
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "~2%Same or lesser value at state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or lesser value at state already exists" nil)))
               (t (setf (fourth prior-dead-depth-time-val) succ-value)))))
    (max-value
      `(let ((succ-value (problem-state.value succ-state)))
         (cond ((<= succ-value (fourth prior-dead-depth-time-val))
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "~2%Same or greater value at state already exists:~%~A" succ-state))
                  (return-from process-successor-graph (values "Same or greater value at state already exists" nil)))
               (t (setf (fourth prior-dead-depth-time-val) succ-value)))))))
|#

(defun process-successor-graph (current-node succ-state)
  ;Must be compiled with (debug 3)
  "Decides how to process the next successor state. Returns whether or not the
   current node still has life (ie, potential successors)."
  (declare (optimize (debug 3)))
  (declare (node current-node) (problem-state succ-state))
  (increment-global-fixnum *total-states-processed*)
  ;(print-search-progress)       ;#nodes expanded so far
  (when (at-max-depth (1+ (node.depth current-node)))  ;at max depth, if *depth-cutoff* specified
    #+:wouldwork-debug (when (>= *debug* 3)
                         (format t "~2%State at max depth:~%~A" succ-state))
    (return-from process-successor-graph (values "State at max depth" nil)))
  (let ((prior-dead-depth-time-val (gethash succ-state *visited*)))  ;previously visited, on closed or open
    (when (and prior-dead-depth-time-val (first prior-dead-depth-time-val))  ;previously closed
      (increment-global-fixnum *repeated-states*)
      #+:wouldwork-debug (when (>= *debug* 3)
                           (format t "~2%State previously closed:~%~A" succ-state))
      (return-from process-successor-graph (values "State previously closed" nil)))
    (when (and prior-dead-depth-time-val (null (first prior-dead-depth-time-val)))  ;already on open, not closed
      (increment-global-fixnum *repeated-states*)
      (ecase *solution-type*
        ((first every count min-length)
           (let ((succ-depth (1+ (node.depth current-node))))
             (cond ((>= succ-depth (second prior-dead-depth-time-val))
                    #+:wouldwork-debug (when (>= *debug* 3)
                                         (format t "~2%Same or shorter path length to state already exists:~%~A" succ-state))
                    (return-from process-successor-graph (values "Same or shorter path length to state already exists" nil)))
                   (t (setf (second prior-dead-depth-time-val) succ-depth)))))
        (min-time
          (let ((succ-time (problem-state.time succ-state)))
            (cond ((>= succ-time (third prior-dead-depth-time-val))
                   #+:wouldwork-debug (when (>= *debug* 3)
                                        (format t "~2%Same or shorter time to state already exists:~%~A" succ-state))
                   (return-from process-successor-graph (values "Same or shorter time to state already exists" nil)))
                  (t (setf (third prior-dead-depth-time-val) succ-time)))))
        (min-value
          (let ((succ-value (problem-state.value succ-state)))
            (cond ((>= succ-value (fourth prior-dead-depth-time-val))
                   #+:wouldwork-debug (when (>= *debug* 3)
                                        (format t "~2%Same or lesser value at state already exists:~%~A" succ-state))
                   (return-from process-successor-graph (values "Same or lesser value at state already exists" nil)))
                  (t (setf (fourth prior-dead-depth-time-val) succ-value)))))
        (max-value
          (let ((succ-value (problem-state.value succ-state)))
            (cond ((<= succ-value (fourth prior-dead-depth-time-val))
                   #+:wouldwork-debug (when (>= *debug* 3)
                                        (format t "~2%Same or greater value at state already exists:~%~A" succ-state))
                   (return-from process-successor-graph (values "Same or greater value at state already exists" nil)))
                  (t (setf (fourth prior-dead-depth-time-val) succ-value)
                     #+:wouldwork-debug (when (>= *debug* 0)
                                          (format t "~%Better max-value found: ~A" succ-value)))))))))
  (values "" (generate-new-node current-node succ-state)))  ;no prior, create new node on open

#|
(defmacro insert-tree-code ()
  "Inserts the proper code for each *solution-type* into process-successor-graph.
   Analyzes states already visited to determine if a successor can be pruned."
  (ecase *solution-type*
    (first  ;present only to allow compilation with default solution-type = first, exited earlier via detect-goals
      `nil)
    ((every count min-value max-value)
      `nil)  ;continue searching
    (min-length
      `(when (and *solutions* (>= (1+ (node.depth current-node)) (solution.depth (first *solutions*))))
         #+:wouldwork-debug (when (>= *debug* 3)
                              (format t "~2%Same or shorter path length to a solution already exists:~%~A" succ-state))
         (return-from process-successor-tree (values "Same or shorter path length to a solution already exists" nil))))
    (min-time
      `(when (and *solutions* (>= (problem-state.time succ-state) (solution.time (first *solutions*))))
         #+:wouldwork-debug (when (>= *debug* 3)
                              (format t "~2%Same or shorter time to a solution already exists:~%~A" succ-state))
         (return-from process-successor-tree (values "Same or shorter time to a solution already exists" nil))))))
|#

(defun process-successor-tree (current-node succ-state)
  ;Must be compiled with (debug 3)
  "Decides how to process the next successor state. Returns whether or not the
   current node still has life (ie, potential successors)."
  (declare (optimize (debug 3)))
  (declare (node current-node) (problem-state succ-state))
  (increment-global-fixnum *total-states-processed*)
  ;(print-search-progress)       ;#nodes expanded so far
  (when (on-current-path succ-state current-node)
    #+:wouldwork-debug (when (>= *debug* 3)
                         (format t "~2%State already on current path:~%~A" succ-state))
    (return-from process-successor-tree (values "State already on current path" nil)))
  (when (at-max-depth (1+ (node.depth current-node)))
    #+:wouldwork-debug (when (>= *debug* 3)
                         (format t "~2%State at max depth:~%~A" succ-state))
    (return-from process-successor-tree (values "State at max depth" nil)))
  (ecase *solution-type*
    (first  ;present only to allow compilation with default solution-type = first, exited earlier via detect-goals
      `nil)
    ((every count min-value max-value)
      `nil)  ;continue searching
    (min-length
      (when (and *solutions* (>= (1+ (node.depth current-node)) (solution.depth (first *solutions*))))
        #+:wouldwork-debug (when (>= *debug* 3)
                             (format t "~2%Same or shorter path length to a solution already exists:~%~A" succ-state))
        (return-from process-successor-tree (values "Same or shorter path length to a solution already exists" nil))))
    (min-time
      (when (and *solutions* (>= (problem-state.time succ-state) (solution.time (first *solutions*))))
        #+:wouldwork-debug (when (>= *debug* 3)
                             (format t "~2%Same or shorter time to a solution already exists:~%~A" succ-state))
        (return-from process-successor-tree (values "Same or shorter time to a solution already exists" nil)))))
  (values "" (generate-new-node current-node succ-state)))
