;;; Filename:  searcher.lisp

;;; Nonstandard Depth First Branch & Bound. Optional duplicate checking for nodes
;;; previously visited (graph search) as specified in problem spec. Open nodes are
;;; kept in an indexed stack and (optionally for graph search) closed nodes in a
;;; hash table. Search
;;; follows the scheme described in "Ch 3&4 Artificial Intelligence: A Modern
;;; Approach" by Russell & Norvig, p87 (with closing of barren nodes),
;;; but keeps nodes on OPEN until the search skeleton is pruned (either dropped for
;;; tree search or to CLOSED for graph search.
;;; A complete path from a goal to the start state is then available in OPEN.
;;; Searches up to a user-specified depth cutoff.  In graph search a visited node is
;;; either in OPEN or CLOSED at any particular time. A user-defined heuristic can be
;;; used to expand the best states first at each level (beam search), but
;;; the entire search graph may ultimately be searched (completeness).


(in-package :ww)


(defstruct (node
             (:print-function
               (lambda (node stream depth)
                 ;Prints out a node. Used for debugging.
                 (declare (ignore depth) (node node) (stream stream))
                 (format stream "~&NODE: STATE=~A DEPTH=~:D"   ;PARENT=~S~%"
                   (node-state node)
                   (node-depth node)
                   #|(ut::if-it (node-parent node) (node-state ut::it))|#))))
  (state (make-problem-state) :type problem-state)    ;problem state
  (depth 0 :type fixnum)           ;depth in the search tree
  (parent nil :type (or null node)))  ;this node's parent


(defstruct solution  ;the record of a solution
  (depth 0 :type fixnum)
  (time 0.0 :type real)
  (value 0.0 :type real)
  (path nil :type list)
  (goal (make-problem-state) :type problem-state))


#+:sbcl (sb-ext:define-hash-table-test state-equal-p state-equal-p-hash)
;Hash stack test for *open*.
(declaim (function state-equal-p state-equal-p-hash))


;;;;;;;;;;;;;;;;;;; Shared Global Update Macros ;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro define-global-fixnum (var-name val-form &optional doc-string)
  `(progn (declaim (fixnum ,var-name))
     ,(if (> *num-parallel-threads* 0)
       #+sbcl (when (not (boundp var-name))
                `(sb-ext:defglobal ,var-name ,val-form ,doc-string))
       #+allegro `(defparameter ,var-name ,val-form ,doc-string)
        `(defparameter ,var-name ,val-form ,doc-string))))


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
       

(defmacro update-best-value-so-far (value)
  (if (> *num-parallel-threads* 0)
    #+sbcl `(sb-ext:atomic-update *best-value-so-far* #'compute-best-value-so-far ,value)
    #+allegro `(excl:update-atomic (*best-value-so-far* *best-value-so-far*) ,value)
    `(setf *best-value-so-far* ,value)))


;;;;;;;;;;;;;;;;;;;;;;;; Debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


(defparameter *-* '---------------------------------------------------------)
  ;Division marker for debugging printout convenience.


;;;;;;;;;;;;;;;;;;;; Search Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-global-fixnum *solution-count* 0
  "Holds the total number of solutions found so far (shared).")

(define-global-fixnum *num-idle-threads* 0
  "Holds the number of currently idle threads (shared).")

(define-global-fixnum *total-states-processed* 0
  "Count of states either newly generated, updated, or regenerated while searching (shared).")

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

(defparameter *solutions* nil)
  ;The resulting list of solutions found.
(declaim (list *solutions*))

(defparameter *best-value-so-far* 0.0)
  ;progressively holds the best (min or max) value during search
(declaim (real *best-value-so-far*))


(defparameter *open*
  (if (and (> *num-parallel-threads* 0) (alexandria:featurep :sbcl))
    (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node-state
                       :ht-test 'state-equal-p :synchronized t)
    (hs::create-hstack :element-type '(or node null) :ht-keyfn #'node-state
                       :ht-test 'state-equal-p)))
  ;The hash-stack structure containing the set of open nodes, local to search-parallel.
(declaim (hs::hstack *open*))


#+:sbcl (defparameter *closed*
          (if (eq (ww-get 'tree-or-graph) 'graph)
            (if (> *num-parallel-threads* 0)
              (make-hash-table :size (ww-get 'max-states)
                               :test 'state-equal-p
                               :synchronized t)
              (make-hash-table :size (ww-get 'max-states)
                               :test 'state-equal-p))
            (make-hash-table :size 0)))  ;unused null placeholder


#+:allegro (defparameter *closed*
             (when (eq (ww-get 'tree-or-graph) 'graph)
               (make-hash-table :size (ww-get 'max-states)
                                :test 'state-equal-p)))
;The hash-table containing the set of closed nodes, global.
(declaim (hash-table *closed*))



;;; Search Functions


(defun dfs ()
  (when (eql (ww-get 'tree-or-graph) 'graph)
    (clrhash *closed*))
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
    (summarize-search-results (if (eq (ww-get 'solution-type) 'first)
                                'first
                                'exhausted))))


(defun process-threads ()
  "The main consumer of parallel thread processing."
  (setf lparallel:*kernel* (lparallel:make-kernel *num-parallel-threads*))
  (let ((channel (lparallel:make-channel))
        (problems (lparallel.queue:make-queue))  ;holds current problems to be solved
        (done (lparallel.queue:make-queue)))  ;signals t when all processing is done
    (setf *num-idle-threads* *num-parallel-threads*)
    (loop for i from 1 to *num-parallel-threads*  ;get all threads up & running
        do (lparallel:submit-task channel #'search-parallel problems done))
    (lparallel.queue:push-queue *open* problems)  ;setup the main problem for the thread pool
    (lparallel.queue:pop-queue done)  ;block until thread pool reports done
    (lparallel.queue:push-queue t done)  ;signal remaining threads to stop processing
    (loop for i from 1 to *num-parallel-threads*
       do (lparallel.queue:push-queue (hs::make-hstack) problems)))  ;send stop message to all threads
  (lparallel:end-kernel :wait t))


(defun search-parallel (&optional problems done)
  ;Branch & Bound DFS parallel search in each thread.
  (iter
    (let ((open (lparallel.queue:pop-queue problems)))  ;open is local for this thread
      (increment-global-fixnum *num-idle-threads* -1)
      (when-debug>= 1
        (lprt *-* 'entering)
        (let ((*package* (find-package :hs)))
          (lprt open)))
      (when (eq (hs::hstack-keyfn open) #'identity)  ;completion received from process-threads
        (when-debug>= 1
          (lprt 'completion))
        (increment-global-fixnum *num-idle-threads*)
        (return-from search-parallel))  ;complete & exit this thread
      (iter 
        (when (lparallel.queue:peek-queue done)  ;interrupt this thread when done
          (when-debug>= 1
            (lprt 'interrupted))
          (increment-global-fixnum *num-idle-threads*)
          (return-from search-parallel))  ;exit this thread
        (when (and (not (linear open))
                   (> *num-idle-threads* 0))  ;causes multiple splitting
          (let ((subopen (split-off open)))  ;split open
            (when-debug>= 1
              (lprt 'splitting)
              (let ((*package* (find-package :hs)))
                (lprt subopen open)))
            (lparallel.queue:push-queue subopen problems)))
        (let ((succ-nodes (df-bnb1 open)))  ;work a little more on current problem
          (when (equal succ-nodes '(first))  ;first solution sufficient & found detected
            (when-debug>= 1
              (lprt 'signal-first-done))
            (lparallel.queue:push-queue t done)  ;signal all done to process-threads
            (increment-global-fixnum *num-idle-threads*)
            (leave))  ;go back to top and wait for exit signal
          (when (hs::empty-hstack open)
            (when-debug>= 1
              (lprt 'exhausted-open))
            (when (= *num-idle-threads* (1- *num-parallel-threads*))  ;all other threads idle
              (when-debug>= 1
                (lprt 'signal-exhausted-done))
              (lparallel.queue:push-queue t done))  ;signal all done to process-threads
            (increment-global-fixnum *num-idle-threads*)
            (leave))  ;get next open
          (when-debug>= 1
            (lprt 'expanding (length succ-nodes)))
          (loop for succ-node in succ-nodes
            do (hs::push-hstack succ-node open)))))))



(defun split-off (open)
  ;Removes the top unexplored node on open and starts a new split-off open with it.
  (declare (hs::hstack open))
  (iter (with i = 1) (with j = 2)
        (while (< i (1- (hs::length-hstack open))))
        (when (= (node-depth (hs::nth-hstack i open)) (node-depth (hs::nth-hstack j open)))
          (let ((new-open #+:sbcl (hs::create-hstack :element-type '(or node nil) 
                                                     :ht-keyfn #'node-state
                                                     :ht-test 'state-equal-p :synchronized t)
                          #+:allegro (hs::create-hstack :element-type '(or node nil) 
                                                        :ht-keyfn #'node-state
                                                        :ht-test 'state-equal-p)))
            (iter (for n from 0 to i)
                  (hs::push-hstack (copy-node (hs::nth-hstack n open)) new-open)
                  (when (> n 0)
                    (setf (node-parent (hs::nth-hstack n new-open))
                      (hs::nth-hstack (1- n) new-open))))
            (hs::deletef-nth-hstack i open)
            (leave new-open)))
        (incf i) (incf j)))

#|
(defun split-off (open)
  ;Removes half of the nodes on open and moves them to a new open, returning both.
  (declare (hs::hstack open))
  (let* ((open-stack (hs::hstack-vector open))
         (open-table (hs::hstack-table open))
         (new-stack (make-array (length open-stack) :adjustable t :fill-pointer 0))
         (new-table #+:sbcl (make-hash-table :test #'equalp :synchronized t)
                    #+:allegro (make-hash-table :test #'equalp)))
    (iter (with i = 0) (with j = 1) (with k = 2)
          (while (< i (- (length open-stack) 2)))
          (if (= (node-depth (aref open-stack i)) (node-depth (aref open-stack j)))
            (if (= (node-depth (aref open-stack j)) (node-depth (aref open-stack k)))
              ;i = j = k
              (progn (vector-push (aref open-stack j) new-stack)
                     (setf (gethash (aref open-stack j) new-table) t)
                     (setf (aref open-stack j) nil)
                     (remhash (aref open-stack j) open-table)
                     (incf i 2) (incf j 2) (incf k 2))
              ;i = j /= k
              (progn (vector-push (aref open-stack j) new-stack)
                     (setf (gethash (aref open-stack j) new-table) t)
                     (incf i) (incf j) (incf k)))
            (if (= (node-depth (aref open-stack j)) (node-depth (aref open-stack k)))
              ;i /= j = k
              (progn (vector-push (aref open-stack i) new-stack)
                     (setf (gethash (aref open-stack i) new-table) t)
                     (vector-push (aref open-stack j) new-stack)
                     (setf (gethash (aref open-stack j) new-table) t)
                     (setf (aref open-stack j) nil)
                     (remhash (aref open-stack j) open-table)
                     (incf i 2) (incf j 2) (incf k 2))
              ;i /= j /= k
              (progn (vector-push (aref open-stack i) new-stack)
                     (setf (gethash (aref open-stack i) new-table) t)
                     (incf i) (incf j) (incf k)))))
    (alexandria:deletef open-stack nil)  ;remove nils
    (let ((new-open (hs::make-hstack :vector new-stack :table new-table :keyfn #'state-equal-p)))
      (values open new-open))))
|#

(defun search-serial ()
  ;Branch & Bound DFS serial search.
  (iter
    (for succ-nodes = (df-bnb1 *open*))  ;may modify *open* via close-barren-nodes
    (when (equal succ-nodes '(first))
      (return-from search-serial 'first))
    (when (hs::empty-hstack *open*)
      (leave))  ;terminate *open*
    (when succ-nodes
      (loop for succ-node in succ-nodes
        do (hs::push-hstack succ-node *open*)))))


(defun linear (open)
  (declare (hs::hstack open))
  (= (1+ (node-depth (hs::peek-hstack open)))
     (hs::length-hstack open)))


(defun df-bnb1 (open)
  ;Performs expansion of one node from open. Returns
  ;new nodes, (first), or nil if no new nodes generated.
  (declare (hs::hstack open))
  (when-debug>= 3
    (format t "~&-----------------------------------------------------------~%"))
  (when-debug>= 5
    (break))
  (let ((current-node (get-next-node-for-expansion open)))
     
;   Stop at specified node, for debugging <action name> <instantiations> <depth>
;   (probe current-node 'wait '(1 area4) 11)
;   (probe current-node 'pour '(jug4 9 jug2 0 4) 5)
;   (probe current-node 'move '(AREA5 AREA4) 3)
    
    (when (null current-node)
      (return-from df-bnb1 nil))  ;open is empty
    (when-debug>= 3
      (format t "~3%Current node selected:~%~S~2%" current-node))
    (let ((succ-states (get-successors current-node))
          (current-state (node-state current-node))
          (current-depth (node-depth current-node)))
      (if succ-states
        (progn (update-search-tree current-state current-depth "")
               (when (> (1+ current-depth) *max-depth-explored*)
                 (increment-global-fixnum *max-depth-explored*)))
        (progn (update-search-tree current-state current-depth "No successor states")
               (close-barren-nodes current-node open)
               (return-from df-bnb1 nil)))
      (setf succ-states (process-goals-and-prune current-node succ-states))
      ;(ut::prt succ-states 1)
      (when (null succ-states)
        (close-barren-nodes current-node open)
        (return-from df-bnb1 nil))
      ;(ut::prt succ-states 2)
      (when (eq succ-states 'one-goal-only)
        (return-from df-bnb1 '(first)))
      ;(ut::prt succ-states 3)
      (when (inferior-length/time-successors current-depth succ-states)
        (update-search-tree current-state current-depth
                            "No successor states better than best so far")
        (close-barren-nodes current-node open)
        (return-from df-bnb1 nil))
      ;(ut::prt succ-states 4)
      (when (member 'get-best-relaxed-value? *query-names*)
        (setf succ-states (prune-inferior-value-states succ-states)))
      ;(ut::prt succ-states 5)
      (let ((succ-nodes (process-succ-states current-node succ-states open)))
        ;(ut::prt succ-states 6)
        (when (null succ-nodes)
          (update-search-tree current-state current-depth "No productive successor states")
          (close-barren-nodes current-node open)
          (return-from df-bnb1 nil)) 
        ;(ut::prt succ-states 7)
        (increment-global-fixnum *program-cycles*)
        (update-branching-factor (length succ-nodes))
        (return-from df-bnb1 (nreverse succ-nodes))))))


(defun get-next-node-for-expansion (open)
  ;Returns the node at the top of open.
  (declare (hs::hstack open))
  (hs::peek-hstack open))  ;top of stack


(defun get-successors (current-node)
  ;Returns children states of current node (unless same as parent).
  (declare (node current-node))
  (let ((states (expand (node-state current-node))))
;    (ut::sortf states #'(lambda (x y)
;                          (declare (ignore y))
;                          (not (eq x (problem-state-name (node-state parent)))))
;               :key #'problem-state-name)
    states))


(defun process-goals-and-prune (current-node succ-states)
  (declare (node current-node) (list succ-states))
  (loop with kill  ;kill all sibling states if min-length & goal found
        for state in succ-states
        if (and (goal state) (not kill))
          do (ecase (ww-get 'solution-type)
               (count  ;count solution, but don't save it
                 (increment-global-fixnum *solution-count*))
               (min-length  ;no other goals will be found after first???
                 (register-solution current-node state)
                 (setq kill t))
               (min-time
                 (if *solutions*
                   (when (< (problem-state-time state)  ;better time found
                            (solution-time (first *solutions*)))
                     (register-solution current-node state))
                   (register-solution current-node state)))
               (min-value
                (if *solutions*
                  (when (< (problem-state-value state) *best-value-so-far*)
                    (update-best-value-so-far (problem-state-value state))
                    (register-solution current-node state))
                  (progn (update-best-value-so-far (problem-state-value state))
                         (register-solution current-node state))))
               (max-value
                (if *solutions*
                  (when (> (problem-state-value state) *best-value-so-far*)
                    (update-best-value-so-far (problem-state-value state))
                    (register-solution current-node state))
                  (progn (update-best-value-so-far (problem-state-value state))
                         (register-solution current-node state))))
               (first
                 (register-solution current-node state)
                 (return-from process-goals-and-prune 'one-goal-only))
               (every
                 (register-solution current-node state)))
          else collect state into remaining-states
        finally (if kill
                  (return (loop for state in remaining-states
                                do (update-search-tree state (1+ (node-depth current-node))
                                                             "State killed")))
                  (return remaining-states))))


(defun inferior-length/time-successors (current-depth succ-states)
  ;If minimizing length or time, are all nongoal successors
  ;worse than the best depth/time so far?
  (declare (fixnum current-depth) (list succ-states))
  (when *solutions*
    (case (ww-get 'solution-type)
      (min-length (>= (+ current-depth 2)
                          (solution-depth (first *solutions*))))
      (min-time (let ((min-succ-time (reduce #'min succ-states :key #'problem-state-time)))
                      (>= (+ min-succ-time *min-action-duration*)
                          (solution-time (first *solutions*))))))))


(defun prune-inferior-value-states (succ-states)
  (if *solutions*
    (case (ww-get 'solution-type)
      (min-value (loop for succ-state in succ-states
                       when (< (funcall 'get-best-relaxed-value? succ-state)
                               (solution-value (first *solutions*)))
                         collect succ-state))
      (max-value (loop for succ-state in succ-states
                       when (> (funcall 'get-best-relaxed-value? succ-state)
                               (solution-value (first *solutions*)))
                         collect succ-state))
      (t succ-states))
    succ-states))


(defun process-succ-states (current-node succ-states open)
  (declare (node current-node) (list succ-states) (hs::hstack open))     
  (loop with succ-nodes
        for state in succ-states ;nongoal states
        do (multiple-value-bind (message succ-node)
                                  (ecase (ww-get 'tree-or-graph)
                                    (graph
                                      (process-successor-graph current-node state open))
                                    (tree
                                      (process-successor-tree current-node state open)))
             (when (not (string= message ""))
               (update-search-tree state (1+ (node-depth current-node)) message))
             (when succ-node
               (push succ-node succ-nodes)))  ;collect non-dead-end succ-nodes
        finally (return succ-nodes)))


(defun compute-average-branching-factor (n prior-value)
  ;Cumulative average of expanded states.
  (declare (fixnum n) (ignore prior-value))
  (/ (+ n (* *program-cycles* *average-branching-factor*))
                       *program-cycles*))


(defun compute-best-value-so-far (value prior-value)
  ;When parallel for sbcl, returns value.
  (declare (ignore prior-value))
  value)
       

(defun process-successor-graph (current-node succ-state open)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (problem-state succ-state) (hs::hstack open))
  (increment-global-fixnum *total-states-processed*)
  (print-search-progress-graph)       ;#nodes expanded so far
  (let ((prior-succ-depth? (gethash succ-state *closed*))  ;early placement for multithreading
        (succ-depth (1+ (node-depth current-node))))
    (cond ((and (> (ww-get 'depth-cutoff) 0) (>= succ-depth (ww-get 'depth-cutoff)))
             (when-debug>= 3
               (format t "~2%State at max depth:~%~A" succ-state))
             (values "State at max depth" nil))
          ((hs::in-hstack succ-state open)
             (when-debug>= 3
               (format t "~2%State already on open:~%~A" succ-state))
             (values "State already on open" nil))
          ((null prior-succ-depth?)
             (values "" (generate-new-node current-node succ-state)))  ;not on *closed*
          ((<= succ-depth prior-succ-depth?)
             (remhash succ-state *closed*) ;equal or shallower node found, swap nodes
             (values "" (generate-new-node current-node succ-state))) ;put back on open
          (t (when-debug>= 3
               (format t "~2%Better path to state already exists:~%~A" succ-state))
             (values "Better path to state already exists" nil)))))


(defun process-successor-tree (current-node succ-state open)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (problem-state succ-state) (hs::hstack open))
  (increment-global-fixnum *total-states-processed*)
  (print-search-progress-tree)       ;#nodes expanded so far
  (cond ((at-max-depth current-node)
           (when-debug>= 3
             (format t "~2%State at max depth:~%~A" succ-state))
           (values "State at max depth" nil))
        ((hs::in-hstack succ-state open)
           (when-debug>= 3
             (format t "~2%State already on open:~%~A" succ-state))
           (values "State already on open" nil))
        (t (values "" (generate-new-node current-node succ-state)))))


(defun generate-new-node (current-node succ-state)
  ;Produces a new node for a given successor.
  (declare (node current-node) (problem-state succ-state))
  (let ((succ-node (make-node :state succ-state
                              :depth (1+ (node-depth current-node))
                              :parent current-node)))
    (when-debug>= 3
      (format t "~2%Installing new or updated successor:~%~S" succ-node))
    succ-node))


(defun close-barren-nodes (current-node open)
  ;Move nodes from open to closed if barren, unless current-node has goal succ.
  (declare (node current-node) (hs::hstack open))
  (do ((node current-node) parent)
      ((or (null node) (not (eq node (hs::peek-hstack open)))))
    (hs::pop-hstack open)
    (when (eq (ww-get 'tree-or-graph) 'graph)
      (setf (gethash (node-state node) *closed*)
        (node-depth node)))
    (setq parent (node-parent node))
    (setf (node-parent node) nil)
    (when-debug>= 4
      (format t "~2%Unproductive state:~%~S~%" node))
    (setq node parent)))


(defun update-search-tree (state depth message)
  (declare (problem-state state) (fixnum depth) (string message))
  (when (and (not (> *num-parallel-threads* 0)) (or (= *debug* 1) (= *debug* 2)))
    (push `((,(problem-state-name state) 
             ,@(problem-state-instantiations state))
           ,depth
           ,message
           ,@(case *debug*
               (1 nil)
               (2 (list (list-database (problem-state-idb state))))))
          *search-tree*)))


(defun at-max-depth (current-node)
  ;Determines if installing a nongoal successor to the current node will be
  ;pointless, based on it being at the max allowable depth.
  (declare (node current-node))
  (let ((depth (node-depth current-node)))
    (when (> (ww-get 'depth-cutoff) 0)
      (= depth (1- (ww-get 'depth-cutoff))))))


(defun best-states-last (state1 state2)
  ;Used to sort a list of expanded states according to the user-defined heuristic.
  (declare (problem-state state1 state2))
  (> (estimate-to-goal state1) (estimate-to-goal state2)))


(defun probe (current-node name instantiations depth)
  ;Breaks when the current node matches action name, instantiations, and depth.
  (declare (node current-node))
  (let ((state (node-state current-node)))
    (when (and (eql (problem-state-name state) name)
               (equal (problem-state-instantiations state) instantiations)
               (= (node-depth current-node) depth))
    (ww-set 'debug 5))))


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
;     (1- (hash-table-count *closed*))
;     (length *solutions*)))

   
(defun summarize-search-results (condition)
  (declare (symbol condition))
  (format t "~2%In problem ~A, performed ~A search for ~A solution."
            (ww-get 'problem) (ww-get 'tree-or-graph) (ww-get 'solution-type))
  (ecase condition
    (first
      (when *solutions*
        (format t "~2%Search ended with first solution found." )))
    (exhausted
      (format t "~2%Search process completed normally.")
      (ecase (ww-get 'solution-type)
        (count
          (format t "~2%Found ~:D solutions." *solution-count*))
        (every
          (format t "~2%Examined every state up to the depth cutoff."))
        ((min-length min-time max-value min-value)
          (format t "~2%Examined only worthwhile states up to the depth cutoff.")))))
  (format t "~2%Depth cutoff = ~:D" (ww-get 'depth-cutoff))
  (format t "~2%Maximum depth explored = ~:D" *max-depth-explored*)
  (format t "~2%Total states processed = ~:D" *total-states-processed*)
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
      (case (ww-get 'solution-type)
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
    (unless (eq (ww-get 'solution-type) 'count)
      (format t "~2%No solutions found.~2%")))
  (search-tree-debugging))


(defun search-tree-debugging ()
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
                              (* 4 (second act)) (fourth act)))))
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
             :path (append (record-solution-path current-node)
                           (list (record-move (node-state current-node)
                                                  goal-state))) ;add final move
             :goal goal-state)))
    (if (> *num-parallel-threads* 0)
      (progn (when-debug>= 1
               (lprt))
             (bt:with-lock-held (*lock*)
               (format t "~&New path to goal found at depth = ~:D~%" state-depth)))
      (format t "~%New path to goal found at depth = ~:D~%" state-depth))
    (when-debug>= 3
      (format t "~%New solution found:~%  ~A~%" solution))
    (push-global solution *solutions*)
    (update-search-tree goal-state state-depth "***goal***")))


(defun printout-solution (soln)
  (declare (solution soln))
  (dolist (item (solution-path soln))
    (write item :pretty t)
    (terpri))
  (format t "~%Final state:~%~A~2%"
    ;(list-database (problem-state-db (solution-goal soln)))))
    (list-database (problem-state-idb (solution-goal soln)))))


(defun print-search-progress-graph ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* (ww-get 'progress-reporting-interval)))
    (format t "~%total states processed so far = ~:D"  ;, unique states encountered = ~:D"
      *total-states-processed*)  ; *unique-states-encountered-graph*)
    (format t "~%average branching factor = ~F~%" *average-branching-factor*)))


(defun print-search-progress-tree ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* (ww-get 'progress-reporting-interval)))
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
