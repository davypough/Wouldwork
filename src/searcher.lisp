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


(in-package :bnb)


(defstruct (node
             (:print-function
               (lambda (node stream depth)
                 ;Prints out a node. Used for debugging.
                 (declare (ignore depth) (node node) (stream stream))
                 (format stream "~&NODE: STATE=~A DEPTH=~:D"   ;PARENT=~S~%"
                   (node-state node)
                   (node-depth node)
                   #|(ut::if-it (node-parent node) (node-state ut::it))|#))))
  (state   nil :type ww::problem-state)    ;problem state
  (depth   0   :type fixnum)           ;depth in the search tree
  (parent  nil :type (or null node)))  ;this node's parent


(declaim
 (fixnum ww::*progress-reporting-interval* ww::*max-states* ww::*depth-cutoff*  *num-idle-threads*
         *total-states-processed* *program-cycles* *max-depth-explored* *count*)  ;*unique-states-encountered-graph* 
 (single-float *average-branching-factor*)
 (list ww::*solutions* *search-tree*)
 (ww::problem-state ww::*start-state*)
 (hs::hstack *open*)
 (hash-table *closed*)
 (function ww::state-equal-p))


(defparameter *lock* (bt:make-lock))  ;for debugging


(defmacro tprt (thread-index &rest vars)
  "Locked version of parallel prt to print from one thread."
  `(when (= (lparallel:kernel-worker-index) ,thread-index)
     (bt:with-lock-held (*lock*) (terpri) (ut::prt ,@vars) (finish-output))))


(defmacro lprt (&rest vars)
  "Locked version of parallel prt with package binding for printout."
  `(bt:with-lock-held (*lock*) (terpri) (ut::prt ,@vars) (finish-output)))


(defparameter *-* '---------------------------------------------------------)
  ;Division marker for debugging.


#+:sbcl (sb-ext:define-hash-table-test ww::state-equal-p sxhash)



;;; Search Parameters


#+:sbcl (sb-ext:defglobal *count* 0)
#+:allegro (defparameter *count* 0)
  ;Counter for debugging thread iterations.

#+:sbcl (sb-ext:defglobal *total-states-processed* 0)
#+:allegro (defparameter *total-states-processed* 0)
  ;Count of states either newly generated, updated, or regenerated while searching.

;#+:sbcl (sb-ext:defglobal *unique-states-encountered-graph* 0)
;#+:allegro (defparameter *unique-states-encountered-graph* 0)
  ;Count of unique states encountered, only used during graph search.

#+:sbcl (sb-ext:defglobal *program-cycles* 0)
#+:allegro (defparameter *program-cycles* 0)
  ;Count of complete cycles of searching.

#+:sbcl (sb-ext:defglobal *num-idle-threads* 0)
#+:allegro (defparameter *num-idle-threads* 0)  ;holds the number of currently idle threads


(defparameter *max-depth-explored* 0)
  ;Keeps track of the maximum depth reached so far during the search.

(defparameter *average-branching-factor* 0.0)
  ;Average branching factor so far during search.

(defparameter *search-tree* nil)
  ;DFS search tree for debugging.


(defparameter *open*
  (if (and ww::*parallel* (member :sbcl *features*))
    (hs::create-hstack :element-type '(or node nil) :ht-keyfn #'node-state
                       :ht-test 'ww::state-equal-p :synchronized t)
    (hs::create-hstack :element-type '(or node nil) :ht-keyfn #'node-state
                       :ht-test 'ww::state-equal-p)))
  ;The hash-stack structure containing the set of open nodes, local to search-parallel.


;put ww::*start-state* on *open*
(hs::push-hstack (make-node :state ww::*start-state*) *open*)


#+:sbcl (defparameter *closed*
          (if (eq ww::*tree-or-graph* 'ww::graph)
            (if ww::*parallel*
              (make-hash-table :size ww::*max-states*
                               :test 'ww::state-equal-p
                               :synchronized t)
              (make-hash-table :size ww::*max-states*
                               :test 'ww::state-equal-p))
            (make-hash-table :size 0)))  ;unused null placeholder


#+:allegro (defparameter *closed*
             (when (eq ww::*tree-or-graph* 'ww::graph)
               (make-hash-table :size ww::*max-states*
                                :test 'ww::state-equal-p)))
;The hash-table containing the set of closed nodes, global.



;;; Search Functions


(defun dfs ()
  (when (eql ww::*tree-or-graph* 'ww::graph)
    (clrhash *closed*))
  (setf *open*
    (if (and ww::*parallel* (member :sbcl *features*))
      (hs::create-hstack :element-type '(or node nil) :ht-keyfn #'node-state
                         :ht-test 'ww::state-equal-p :synchronized t)
      (hs::create-hstack :element-type '(or node nil) :ht-keyfn #'node-state
                         :ht-test 'ww::state-equal-p)))
  (hs::push-hstack (make-node :state ww::*start-state*) *open*)
  (setf *program-cycles* 1)
  (setf *average-branching-factor* 0.0)
  (setf *total-states-processed* 1)
;  (setf *unique-states-encountered-graph* 1)  ;only used during graph search
  (setf *max-depth-explored* 0)
  (setf *num-idle-threads* 0)
  (setf ww::*solutions* nil)
  (if ww::*parallel*
    ;(with-open-stream (*standard-output* (make-broadcast-stream)) ;ignore *standard-output*
    (process-threads)
    (search-serial))
  (ecase ww::*tree-or-graph*
    (ww::graph (summarize-search-results-graph (if ww::*first-solution-sufficient*
                                                 'first
                                                 'exhausted)))
    (ww::tree (summarize-search-results-tree (if ww::*first-solution-sufficient*
                                               'first
                                               'exhausted)))))



(defun process-threads ()
  "The main consumer of parallel thread processing."
  (setf lparallel:*kernel* (lparallel:make-kernel ww::*num-threads*))
  (let ((channel (lparallel:make-channel))
        (problems (lparallel.queue:make-queue))  ;holds current problems to be solved
        (done (lparallel.queue:make-queue)))  ;signals t when all processing is done
    (loop for i from 1 to ww::*num-threads*  ;get all threads up & running
        do (lparallel:submit-task channel #'search-parallel problems done))
    (lparallel.queue:push-queue *open* problems)  ;setup the main problem for the thread pool
    (lparallel.queue:pop-queue done)  ;block until thread pool reports done
    (lparallel.queue:push-queue t done)  ;signal remaining threads to stop processing
    (loop for i from 1 to ww::*num-threads*
       do (lparallel.queue:push-queue (hs::make-hstack) problems)))  ;send stop message to all threads
  (lparallel:end-kernel :wait t))


(defun search-parallel (&optional problems done)
  ;Branch & Bound DFS parallel search.
  (setf *search-tree* nil)
  (iter
    #+:sbcl (sb-ext:atomic-incf *num-idle-threads*)
    #+:allegro (excl:incf-atomic *num-idle-threads*)
    (let ((open (lparallel.queue:pop-queue problems)))  ;open is local for this thread
      #+:sbcl (sb-ext:atomic-decf *num-idle-threads*)
      #+:allegro (excl:decf-atomic *num-idle-threads*)
      ;(tprt 0 *-* 'entering open)
      (when (= ww::*debug* 1)
        (let ((*package* (find-package :bnb)))
          (lprt *-* 'entering (lparallel:kernel-worker-index)))
        (let ((*package* (find-package :hs)))
          (lprt open)))
      (when (eq (hs::hstack-keyfn open) #'identity)  ;exit received from process-threads
        ;(tprt 0 'exiting)
        (when (= ww::*debug* 1)
          (let ((*package* (find-package :bnb)))
            (lprt 'returning-from-search-parallel-1 (lparallel:kernel-worker-index))))
        (return-from search-parallel))  ;exit this thread
      (iter 
        (when (lparallel.queue:peek-queue done)  ;interrupt this thread when done
          (when (= ww::*debug* 1)
            (let ((*package* (find-package :bnb)))
              (lprt 'returning-from-search-parallel-2 (lparallel:kernel-worker-index))))
          (return-from search-parallel))  ;exit this thread
        (when (and (not (linear open))
                   ;(> *num-idle-threads* 0))  ;causes multiple splitting
                   (< (lparallel.queue:queue-count problems)
                      *num-idle-threads*))  ;ie, some threads are idle
          ;(tprt 0 'pre-split open)
          (when (= ww::*debug* 1)
            (lprt 'pre-split (lparallel:kernel-worker-index))
            (let ((*package* (find-package :hs)))
              (lprt open)))
          (let ((subopen (split-off open)))  ;split open
            ;(tprt 0 subopen open)
            (when (= ww::*debug* 1)
              (lprt 'split (lparallel:kernel-worker-index))
              (let ((*package* (find-package :hs)))
                (lprt subopen open)))
            (lparallel.queue:push-queue subopen problems)))
        (let ((succ-nodes (df-bnb1 open)))  ;work a little more on current problem
          (when (equal succ-nodes '(first))  ;first solution sufficient & found detected
            ;(tprt 0 'signal-first-done)
            (when (= ww::*debug* 1)
              (lprt 'signal-first-done (lparallel:kernel-worker-index)))
            (lparallel.queue:push-queue t done)  ;signal all done to process-threads
            (leave))  ;go back to top and wait for exit signal
          (when (hs::empty-hstack open)
            ;(tprt 0 'exhausted-open)
            (when (= ww::*debug* 1)
              (lprt 'exhausted-open (lparallel:kernel-worker-index)))
            (when (= *num-idle-threads* (1- ww::*num-threads*))  ;all other threads idle
              ;(tprt 0 'signal-exhausted-done)
              (when (= ww::*debug* 1)
                (lprt 'signal-exhausted-done (lparallel:kernel-worker-index)))
              (lparallel.queue:push-queue t done))  ;signal all done to process-threads
            (leave))  ;get next open
          (loop for succ-node in succ-nodes
            do (hs::push-hstack succ-node open)))))))


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
  (= (1+ (node-depth (hs::peek-hstack open)))
     (hs::length-hstack open)))
     
  
(defun df-bnb1 (open)
  ;Performs expansion of one node from open. Returns new nodes, (first), or nil
  ;if no new nodes generated.
  (let ((current-node (get-next-node-for-expansion open)))
     
;   Stop at specified node, for debugging <action name> <instantiations> <depth>
;   (probe current-node 'ww::wait '(1 ww::area4) 11)
;   (probe current-node 'ww::pour '(ww::jug4 9 ww::jug2 0 4) 5)
    
    (when (null current-node) (return-from df-bnb1 nil))  ;open is empty
    (when (>= ww::*debug* 3)
      (format t "~%Current node selected:~%~S~2%" current-node))
    (let ((succ-states (get-successors current-node))
          succ-nodes potential?)
      (update1-*search-tree* current-node succ-states)
      (update-*max-depth-explored* current-node succ-states)
      (setf succ-states
        (loop for state in succ-states
              if (ww::goal state)
                do (register-solution current-node state)
                   (update2-*search-tree* current-node state "***goal***")
                   (when ww::*first-solution-sufficient*
                     (return-from df-bnb1 '(first)))
                else collect state))
      (loop for state in succ-states ;no goal states
         do (multiple-value-bind (message succ-node)
              (ecase ww::*tree-or-graph*
                (ww::graph (process-successor-graph current-node state open))
                (ww::tree (process-successor-tree current-node state open)))
              (when succ-node
                (push succ-node succ-nodes))  ;collect non-dead-end succ-nodes
              (when (eq message t)
                (setf potential? t))
              (update2-*search-tree* current-node state message)))
      (if (null succ-nodes)  ;no new nodes to add to open
        (ecase ww::*tree-or-graph*
          (ww::graph (close-barren-nodes-graph current-node potential? open))
          (ww::tree (close-barren-nodes-tree current-node potential? open)))
        (setq *average-branching-factor*    ;cumulative average of expanded states
          (/ (+ (length succ-states)
                (* *program-cycles* *average-branching-factor*))
             #+:sbcl (sb-ext:atomic-incf *program-cycles*)
             #+:allegro (excl:incf-atomic *program-cycles*))))
      (when (>= ww::*debug* 3) (format t "~&-----------------------------------~%"))
      (when (= ww::*debug* 5) (break))
      (return-from df-bnb1 (nreverse succ-nodes)))))


(defun split-off (open)
  ;Removes the top unexplored node on open and starts a new split-off open with it.
  (iter (with i = 1) (with j = 2)
        (while (< i (1- (hs::length-hstack open))))
        (when (= (node-depth (hs::nth-hstack i open)) (node-depth (hs::nth-hstack j open)))
          (let ((new-open #+:sbcl (hs::create-hstack :element-type '(or node nil) 
                                                     :ht-keyfn #'node-state
                                                     :ht-test 'ww::state-equal-p :synchronized t)
                          #+:allegro (hs::create-hstack :element-type '(or node nil) 
                                                        :ht-keyfn #'node-state
                                                        :ht-test 'ww::state-equal-p)))
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
    (let ((new-open (hs::make-hstack :vector new-stack :table new-table :keyfn #'ww::state-equal-p)))
      (values open new-open))))
|#

(defun get-next-node-for-expansion (open)
  ;Returns the node at the top of open.
  (hs::peek-hstack open))  ;top of stack


(defun get-successors (current-node)
  ;Returns children states of current node (unless same as parent).
  (let* ((parent (node-parent current-node))
         (states (coerce (if parent
                            (remove (node-state parent)
                                    (ww::expand (node-state current-node))
                                    :test 'ww::state-equal-p :count 1)
                            (ww::expand (node-state current-node)))
                        'list)))       
;    (ut::sortf states #'(lambda (x y)
;                          (declare (ignore y))
;                          (not (eq x (ww::problem-state-name (node-state parent)))))
;               :key #'ww::problem-state-name)
   states))


(defun process-successor-graph (current-node succ-state open)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (ww::problem-state succ-state))
  #+:sbcl (sb-ext:atomic-incf *total-states-processed*)
  #+:allegro (excl:incf-atomic *total-states-processed*)
  (print-search-progress-graph)       ;#nodes expanded so far
  (let ((prior-succ-depth? (gethash succ-state *closed*))  ;early placement for multithreading
        (succ-depth (1+ (node-depth current-node))))
    (cond ((and (> ww::*depth-cutoff* 0) (>= succ-depth ww::*depth-cutoff*))  ;(at-max-depth current-node)
             (when (>= ww::*debug* 3)
               (format t "~2%Deadend--state at max depth:~%~A" succ-state))
             (values "Deadend--state at max depth" nil))
          ((hs::key-present-hstack succ-state open)
             (when (>= ww::*debug* 3)
               (format t "~2%Deadend--state already waiting in stack:~%~A" succ-state))
             (values "Deadend--state already waiting in stack" nil))
          ((null prior-succ-depth?)
             (values t (generate-new-node current-node succ-state)))  ;not on *closed*
          ((<= succ-depth prior-succ-depth?)
             (remhash succ-state *closed*) ;equal or shallower node found, swap nodes
             (values t (generate-new-node current-node succ-state))) ;put back on open
          (t (when (>= ww::*debug* 3)
               (format t "~2%Deadend--equal or shorter path to state already exists:~%~A" succ-state))
             (values "Deadend--equal or shorter path to state already exists" nil)))))

#|
(defun process-successor-graph (current-node succ-state open)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (ww::problem-state succ-state))
  #+:sbcl (sb-ext:atomic-incf *total-states-processed*)
  #+:allegro (excl:incf-atomic *total-states-processed*)
  (print-search-progress-graph)       ;#nodes expanded so far
  (let ((succ-depth (gethash succ-state *closed*)))  ;placement for multithreading
    (cond ((at-max-depth current-node)
             (when (>= ww::*debug* 3)
               (format t "~%Deadend--state at max depth:~%~A" succ-state))
             (values "Deadend--state at max depth" nil))
          ((hs::key-present-hstack succ-state open)
             (when (>= ww::*debug* 3)
               (format t "~%Deadend--state already waiting in stack:~%~A" succ-state))
             (values "Deadend--state already waiting in stack" nil))
          ((null succ-depth)  ;not on *closed*
             (values t (generate-new-node current-node succ-state)))
          ((<= (1+ (node-depth current-node)) succ-depth)
             (remhash succ-state *closed*) ;shallower node found, remove old node
             (values t (generate-new-node current-node succ-state))) ;put back on open
          (t (when (>= ww::*debug* 3)
               (format t "~%Deadend--equal or shorter path to state already exists:~%~A" succ-state))
             (values "Deadend--equal or shorter path to state already exists" nil)))))
|#

(defun process-successor-tree (current-node succ-state open)
  ;Decides how to process the next successor state. Returns whether or not the
  ;current node still has life (ie, potential successors).
  (declare (node current-node) (ww::problem-state succ-state))
  #+:sbcl (sb-ext:atomic-incf *total-states-processed*)
  #+:allegro (excl:incf-atomic *total-states-processed*)
  (print-search-progress-tree)       ;#nodes expanded so far
  (cond ((at-max-depth current-node)
           (when (>= ww::*debug* 3)
             (format t "~2%Deadend--state at max depth:~%~A" succ-state))
           (values "Deadend--state at max depth" nil))
        ((hs::key-present-hstack succ-state open)
           (when (>= ww::*debug* 3)
             (format t "~2%Deadend--state already waiting in stack:~%~A" succ-state))
           (values "Deadend--state already waiting in stack" nil))
        (t (values t (generate-new-node current-node succ-state)))))


(defun generate-new-node (current-node succ-state)
  ;Produces a new node for a given successor.
  (declare (node current-node) (ww::problem-state succ-state))
  (let ((succ-node (make-node :state succ-state
                              :depth (1+ (node-depth current-node))
                              :parent current-node)))
    (when (>= ww::*debug* 3)
      (format t "~2%Installing new or updated successor:~%~S" succ-node))
    succ-node))


(defun close-barren-nodes-graph (current-node potential? open)
  ;Move nodes from open to closed if barren.
  (declare (node current-node) ((or nil t) potential?))
  (do ((node current-node)
       parent)
      ((or (null node) (not (eq node (hs::peek-hstack open)))))
    (hs::pop-hstack open)
    (setf (gethash (node-state node) *closed*)
      (node-depth node))
    (setq parent (node-parent node))
    (setf (node-parent node) nil)
    (when (>= ww::*debug* 4)
      (format t "~%Node closed by ~S:~%~S~%"
        (if potential? "cutoff" "deadend") node))
    (setq node parent)))


(defun close-barren-nodes-tree (current-node potential? open)
  ;Remove nodes from open if barren.
  (declare (node current-node) ((or nil t) potential?))
  (do ((node current-node)
       parent)
      ((or (null node) (not (eq node (hs::peek-hstack open)))))
    (hs::pop-hstack open)
    (setq parent (node-parent node))
    (setf (node-parent node) nil)
    (when (>= ww::*debug* 4)
      (format t "~%Node closed by ~S:~%~S~%"
        (if potential? "cutoff" "deadend") node))
    (setq node parent)))


(defun at-max-depth (current-node)
  ;Determines if installing a nongoal successor to the current node will be
  ;pointless, based on it being at the max allowable depth.
  (declare (node current-node))
  (let ((depth (node-depth current-node)))
    (when (> ww::*depth-cutoff* 0)
      (= depth (1- ww::*depth-cutoff*)))))


(defun best-states-last (state1 state2)
  ;Used to sort a list of expanded states according to the user-defined heuristic.
  (declare (ww::problem-state state1 state2))
  (> (ww::estimate-to-goal state1) (ww::estimate-to-goal state2)))


(defun probe (current-node name instantiations depth)
  ;Breaks when the current node matches action name, instantiations, and depth.
  (let ((state (node-state current-node)))
    (when (and (eql (ww::problem-state-name state) name)
               (equal (ww::problem-state-instantiations state) instantiations)
               (= (node-depth current-node) depth))
    (setq ww::*debug* 5))))


(defun update-*max-depth-explored* (current-node succ-states)
  (let ((depth (node-depth current-node)))
    (if succ-states
      (when (> (1+ depth) *max-depth-explored*)
        (setf *max-depth-explored* (1+ depth)))
      (when (> depth *max-depth-explored*)
        (setf *max-depth-explored* depth)))))


(defun update1-*search-tree* (current-node succ-states)
  (when (and (= ww::*debug* 1) (not ww::*parallel*))
    (let ((state (node-state current-node)))
      (push (list `(,(ww::problem-state-name state) 
                      ,@(ww::problem-state-instantiations state))
                  (node-depth current-node)
                  (if succ-states
                    ""
                    "No successor states"))
            *search-tree*)))
  (when (= ww::*debug* 2)
    (let ((state (node-state current-node)))
      (push (list `(,(ww::problem-state-name state) 
                    ,@(ww::problem-state-instantiations state))
                  (node-depth current-node)
                  (if succ-states
                    ""
                    "No successor states")
                  (ww::list-database (ww::problem-state-idb state)))
            *search-tree*))))


(defun update2-*search-tree* (current-node succ-state message)
  (when (and (= ww::*debug* 1) (stringp message) (not ww::*parallel*))  ;only push deadend states here
    #+:sbcl (sb-ext:atomic-push
              (list `(,(ww::problem-state-name succ-state)
                      ,@(ww::problem-state-instantiations succ-state))
                      (1+ (node-depth current-node))
                      message)
              *search-tree*)
    #+:allegro (excl:push-atomic
                 (list `(,(ww::problem-state-name succ-state)
                         ,@(ww::problem-state-instantiations succ-state))
                         (1+ (node-depth current-node))
                         message)
                 *search-tree*))
  (when (and (= ww::*debug* 2) (stringp message))  ;only push deadend states here
    #+:sbcl (sb-ext:atomic-push
              (list `(,(ww::problem-state-name succ-state)
                      ,@(ww::problem-state-instantiations succ-state))
                      (1+ (node-depth current-node))
                      message
                      (ww::list-database (ww::problem-state-idb succ-state)))
               *search-tree*)
    #+:allegro (excl:push-atomic
                 (list `(,(ww::problem-state-name succ-state)
                         ,@(ww::problem-state-instantiations succ-state))
                         (1+ (node-depth current-node))
                         message
                         (ww::list-database (ww::problem-state-idb succ-state)))
                       *search-tree*)))


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

  
;(defun unique-states-encountered-graph (open)
;  ;Count of how many unique states have been encountered during searching.
;  (+ (hs::length-hstack open)
;     (1- (hash-table-count *closed*))
;     (length ww::*solutions*)))

   
(defun summarize-search-results-graph (condition)
  (declare (symbol condition))
  (case condition
    (first (format t "~%~%Graph search ended with first solution found.~%"))
    (exhausted (format t "~%~%Graph search process completed normally,~%")
               (format t "examining every state up to the depth cutoff.~%")))
  (format t "~%Depth cutoff = ~:D~%" ww::*depth-cutoff*)
  (format t "~%Maximum depth explored = ~:D~%" *max-depth-explored*)
  (format t "~%Total states processed = ~:D~%" *total-states-processed*)
  ;(format t "~%Unique states encountered = ~:D~%" (unique-states-encountered-graph))
  (format t "~%Program cycles (state expansions) = ~:D~%" *program-cycles*)
  (format t "~%Average branching factor = ~F~%" *average-branching-factor*)
  (format t "~%Start state:~%~A~%"
            (ww::list-database (ww::problem-state-idb ww::*start-state*)))
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
      (format t "~%Number of steps in minimum path length solution = ~:D~%"
                shallowest-depth)
      (format t "~%Solution path from start state to goal state:~%")
      (let ((shallowest-depth-solution (find shallowest-depth ww::*solutions*
                                             :key #'first)))
        (printout-solution shallowest-depth-solution))
      (if (and (not ww::*first-solution-sufficient*)
               (eql shallowest-depth minimum-time))
        (format t "~%Shortest path solution is also a minimum duration solution~%")
        (let ((minimum-time-solution
               (find minimum-time ww::*solutions*
                                  :key (lambda (soln)
                                         (ww::problem-state-time (third soln))))))
          (format t "~%Duration of minimum time solution = ~:D~%" minimum-time)
          (format t "~%Minimum time solution path from start state to goal state:~%")
          (printout-solution minimum-time-solution)))
      (in-package :bnb)
      (terpri))
    (format t "~%No goals recorded.~2%"))
  (when (and (= ww::*debug* 1) (not ww::*parallel*))
    (format t "~%Search tree:~%")
    (loop for act in (cdr (reverse *search-tree*))
          do (if (= (length act) 2)
               (format t "~vT~d:~a~%" (* 4 (second act)) (second act) (first act))
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
  (format t "~%Start state:~%~A~%"
            (ww::list-database (ww::problem-state-idb ww::*start-state*)))
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
        (format t "~%Number of steps in minimum path length solution = ~:D~%"
                  shallowest-depth)
        (format t "~%Solution path from start state to goal state:~%")
        (let ((shallowest-depth-solution (find shallowest-depth ww::*solutions*
                                               :key #'first)))
          (printout-solution shallowest-depth-solution))
        (if (and (not ww::*first-solution-sufficient*)
                 (eql shallowest-depth minimum-time))
            (format t "~%Shortest path solution is also a minimum duration solution~%")
          (let ((minimum-time-solution
                  (find minimum-time ww::*solutions*
                                     :key (lambda (soln)
                                            (ww::problem-state-time (third soln))))))
            (format t "~%Duration of minimum time solution = ~:D~%" minimum-time)
            (format t "~%Minimum time solution path from start state to goal state:~%")
            (printout-solution minimum-time-solution)))
        (in-package :bnb)
        (terpri))
    (format t "~%No goals recorded.~2%"))
  (when (and (= ww::*debug* 1) (not ww::*parallel*))
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
    ;(ww::list-database (ww::problem-state-db (third solution)))))
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
    (when (>= ww::*debug* 3)
      (format t "~%Next solution found:~%  ~A~%" current-solution))
    (bt:with-lock-held (*lock*)
        (format t "~%New path to goal found at depth = ~:D~%" (1+ (node-depth current-node)))
        (pushnew current-solution ww::*solutions* :test #'equalp))))


(defun print-search-progress-graph ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* ww::*progress-reporting-interval*))
    (format t "~%total states processed so far = ~:D"  ;, unique states encountered = ~:D"
      *total-states-processed*)  ; *unique-states-encountered-graph*)
    (format t "~%average branching factor = ~F~%" *average-branching-factor*)))


(defun print-search-progress-tree ()
  ;Printout # of nodes expanded so far during search modulo reporting interval.
  (when (= 0 (mod *total-states-processed* ww::*progress-reporting-interval*))
    (format t "~%total states processed so far = ~:D" *total-states-processed*)
    (format t "~%average branching factor = ~F~%" *average-branching-factor*)))


(defun solve ()
  ;Runs a branch & bound search on the problem specification.
  (ww::initialize)
  (when (and ww::*parallel* (> ww::*debug* 1))
    (format t "ADVISORY: Running parallel threads, resetting *debug* from ~D to 1." ww::*debug*)
    (setq ww::*debug* 1))
  (if ww::*parallel*
    (format t "~%working with ~:D thread(s)...~%" ww::*num-threads*)
    (format t "~%working...~%"))
  (time (dfs))
  (ww::finalize)
  (in-package :ww))
