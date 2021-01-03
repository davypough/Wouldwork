;;; Filename:  parallel.lisp

;;; Collection of functions for managing parallelism (when requested by user).


(in-package :ww)


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
      ;(when-debug>= 1
      ;  (lprt *-* 'entering)
      ;  (let ((*package* (find-package :hs)))
      ;    (lprt open)))
      (when (eq (hs::hstack-keyfn open) #'identity)  ;completion received from process-threads
        ;(when-debug>= 1
        ;  (lprt 'completion))
        (increment-global-fixnum *num-idle-threads*)
        (return-from search-parallel))  ;complete & exit this thread
      (iter 
        (when (lparallel.queue:peek-queue done)  ;interrupt this thread when done
          ;(when-debug>= 1
          ;  (lprt 'interrupted))
          (increment-global-fixnum *num-idle-threads*)
          (return-from search-parallel))  ;exit this thread
        (when (and (not (linear open))
                   (> *num-idle-threads* 0))  ;causes multiple splitting
          (let ((subopen (split-off open)))  ;split open
            ;(when-debug>= 1
            ;  (lprt 'splitting)
            ;  (let ((*package* (find-package :hs)))
            ;    (lprt subopen open)))
            (lparallel.queue:push-queue subopen problems)))
        (let ((succ-nodes (df-bnb1 open)))  ;work a little more on current problem
          (when (equal succ-nodes '(first))  ;first solution sufficient & found detected
            ;(when-debug>= 1
            ;  (lprt 'signal-first-done))
            (lparallel.queue:push-queue t done)  ;signal all done to process-threads
            (increment-global-fixnum *num-idle-threads*)
            (leave))  ;go back to top and wait for exit signal
          (when (hs::empty-hstack open)
            ;(when-debug>= 1
            ;  (lprt 'exhausted-open))
            (when (= *num-idle-threads* (1- *num-parallel-threads*))  ;all other threads idle
              ;(when-debug>= 1
              ;  (lprt 'signal-exhausted-done))
              (lparallel.queue:push-queue t done))  ;signal all done to process-threads
            (increment-global-fixnum *num-idle-threads*)
            (leave))  ;get next open
          ;(when-debug>= 1
          ;  (lprt 'expanding (length succ-nodes)))
          (when succ-nodes
            (if (fboundp 'heuristic?)
              (setf succ-nodes (sort succ-nodes #'>
                                     :key (lambda (node)
                                            (problem-state-heuristic (node-state node)))))
              (when *randomize-search*
                (setf succ-nodes (alexandria:shuffle succ-nodes)))))
          (loop for succ-node in succ-nodes
            do (hs::push-hstack succ-node open)))))))


(defun linear (open)
  (declare (hs::hstack open))
  (= (1+ (node-depth (hs::peek-hstack open)))
     (hs::length-hstack open)))


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
