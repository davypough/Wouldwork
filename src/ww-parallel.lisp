;;; Filename:  ww-parallel.lisp

;;; Collection of functions for managing parallelism (when requested by user).


(in-package :ww)


(defun process-threads ()
  "The main consumer of parallel thread processing."
  (setf lparallel:*kernel* (lparallel:make-kernel *threads*))
  (let ((channel (lparallel:make-channel))
        (problems (lparallel.queue:make-queue))  ;holds current problems to be solved
        (first (lparallel.queue:make-queue)))  ;signals first solution sufficient & found
    (setf *num-idle-threads* *threads*)  ;this main thread for control only, not one of the search *threads*
    (iter (for i from 1 to *threads*)  ;get all solving threads up & running
      (lparallel:submit-task channel #'search-parallel problems first))  ;start each thread running search-parallel
    (lparallel.queue:push-queue *open* problems)  ;setup the main problem for the thread pool
    (iter (sleep 0.1)  ;let threads work on problems
          (when (= *num-idle-threads* *threads*)  ;all threads are idle signifying end of search
            (leave)))
    (lparallel.queue:push-queue 'stop problems))  ;send stop message to all idle threads
  (lparallel:end-kernel :wait t))


(defun search-parallel (problems first)  ;follows outline in search-serial
  "Branch & Bound DFS parallel search in each thread."
  (iter
    (let ((open (lparallel.queue:pop-queue problems)))  ;open is local for this thread, blocks idling for a problem
      (when (eql open 'stop)
        (lparallel.queue:push-queue 'stop problems)  ;reinstate for other threads
        (return-from search-parallel))  ;close thread
      (increment-global *num-idle-threads* -1)  ;not idle any more
      #+:ww-debug (when (>= *debug* 1)
                    (let ((*package* (find-package :hs)))
                      (lprt *-* 'entering open)))

      (iter  ;process all nodes on this open
        (when (lparallel.queue:peek-queue first)  ;interrupt this thread when a thread signals first found
          #+:ww-debug (when (>= *debug* 1)
                        (lprt 'interrupted))
          (increment-global *num-idle-threads*)
          (leave))  ;don't continue processing, return to top to either pop next problem, idle, or stop
        (when (and (> (hs::length-hstack open) 1)   ;(not (linear open))
                   (> *num-idle-threads* 0))  ;causes multiple splitting
          (let ((subopen (split-off open)))   ;split open
            (when subopen
              #+:ww-debug (when (>= *debug* 1)
                            (let ((*package* (find-package :hs)))
                              (lprt 'splitting subopen open)))
              (lparallel.queue:push-queue subopen problems))))  ;continue processing this open after split-off
        (let ((current-node (hs::peek-hstack open))
              (succ-nodes (df-bnb1 open)))  ;work a little more on current problem
          (declare (ignorable current-node))
          (when (= *program-cycles* 0)  ;ie, expanding the start state
            (setf *num-init-successors* (length succ-nodes))
            (setf *rem-init-successors* (reverse succ-nodes)))
          #+:ww-debug (when (>= *debug* 1)
                        (lprt current-node))
          (when (equal succ-nodes '(first))  ;first solution sufficient & found
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'first-solution-found))
            (lparallel.queue:push-queue 'found first)  ;signal all threads to go to top (to pop or idle)
            (increment-global *num-idle-threads*)
            (leave))  ;go back to top
          (when succ-nodes
            (if (fboundp 'heuristic?)
              (setf succ-nodes
                (sort (copy-list succ-nodes) #'>
                      :key (lambda (node)
                             (problem-state.heuristic (node.state node)))))
              (when *randomize-search*
                (setf succ-nodes (alexandria:shuffle succ-nodes)))))
          #+:ww-debug (when (>= *debug* 1)
                        (lprt 'expanding (length succ-nodes) succ-nodes *-*)
                        (terpri))
          (iter (for succ-node in succ-nodes)
            (hs::push-hstack succ-node open))
          (when (hs::empty-hstack open)  ;nothing left on open to explore
            #+:ww-debug (when (>= *debug* 1)
                          (lprt 'open-exhausted))
            ;(when (= *num-idle-threads* (1- *threads*))  ;all other threads idle
            ;  #+:ww-debug (when (>= *debug* 1)
            ;                       (lprt 'signal-exhausted-done))
            ;  (lparallel.queue:push-queue t done))  ;signal all done to process-threads
            (increment-global *num-idle-threads*)
            (leave))) ;go back to top
        (increment-global *program-cycles* 1)  ;finished with this cycle
        (setf *average-branching-factor* (compute-average-branching-factor))))))


(defun split-off (open)
  "Removes the bottom node on open and returns a new split-off subopen with it."
  (let ((subopen (hs::make-hstack :table (make-hash-table :test (if (fixedp *relations*)
                                                                  'fixed-keys-ht-equal
                                                                  'equalp)
                                                          :rehash-size 2.0
                                                          :rehash-threshold 0.8
                                                          :synchronized (> *threads* 0))
                                  :keyfn (hs::hstack.keyfn open)))
        (bottom-node (hs::deletef-nth-hstack 0 open)))  ;pops bottom node from open
    (hs::push-hstack bottom-node subopen)))
