;;; Filename: planner.lisp

;;; Setup to solve planning problems.


(in-package :ww)


;;;;;;;;;;;;;;;;;;;Search Functions;;;;;;;;;;;;;;;;;;;


(defun state-equal-p (state1 state2)
  (declare (problem-state state1 state2))
  (equalp (problem-state-idb state1) (problem-state-idb state2)))


(defun state-equal-p-hash (state)
  (declare (problem-state state))
  (sxhash (problem-state-idb state)))


(defun initialize ()
  ;Initialize problem.
  )


(defun finalize ()
  ;Cleanup after problem.
  )


(defun record-move (parent state)
  ;Returns some user-friendly representation of the move from the parent state
  ;to the current state.
  (declare (problem-state state parent) (ignore parent))
  (with-slots (name instantiations time) state
    (list time (cons name instantiations))))

(defun goal (state)
  ;Returns t or nil depending on if state is a goal state.
  (declare (problem-state state))
  (funcall (symbol-function '*goal*) state))

 
(defun do-init-action-updates (state)  ;add init actions to start-state
  ;Checks precondition of each init-action,
  ;and if true, then updates db and static-db according to each init-action effect.
  (declare (problem-state state))
  (when *init-actions*
    (format t "~&Adding init-action propositions to initial database...~%"))
  (iter (for init-action in *init-actions*)
    (with-slots (name precondition-params precondition-types precondition-instantiations
                      precondition-lambda effect-lambda)
        init-action
      (format t "~&~A...~%" name)
      (setf precondition-instantiations
            (or (type-instantiations precondition-types
                                     (case (car precondition-params)
                                       (combinations 'combinations)
                                       (dot-products 'dot-products))
                                     state)
                     '(nil)))
      (let ((pre-fn (compile nil precondition-lambda))
            (eff-fn (compile nil effect-lambda))
            pre-results db-updates)
        (setf pre-results
             (remove-if #'null (mapcar (lambda (pinsts)
                                        (apply pre-fn state pinsts))
                                       precondition-instantiations)))
        (when (null pre-results)
          (next-iteration))
        (setf db-updates  ;returns list of update structures
              (mapcan (lambda (pre-result)
                        (if (eql pre-result t)
                          (funcall eff-fn state)
                          (apply eff-fn state pre-result)))
                      pre-results))
        (dolist (db-update db-updates)
          (loop for literal in (update-changes db-update)
                do (if (eq (car literal) 'not)
                     (if (gethash (caadr literal) *relations*)
                       (delete-proposition (second literal) *db*)  ;dynamic relation
                       (delete-proposition (second literal) *static-db*))
                     (if (gethash (car literal) *relations*)
                       (add-proposition literal *db*)  ;dynamic relation
                       (add-proposition literal *static-db*)))))))))


(defun order-propositions (db-update)
  ;NOTs first so addhash db not removed by later remhash
  (ut::sortf (update-changes db-update) #'(lambda (x y) 
                                            (declare (ignore y))
                                            (and (listp x) (eq (car x) 'not))))
  db-update)


(defun generate-children (state)
  ;Returns the legitimate children of a state. Checks precondition of each action,
  ;and if true, then updates db according to action effects.
  (declare (problem-state state))
  (let (children)
    (iter (for action in *actions*)
      (with-slots (name iprecondition precondition-params precondition-variables
                   precondition-types dynamic precondition-instantiations ieffect)
          action
          (when dynamic
            (setf precondition-instantiations
                 (or (type-instantiations precondition-types
                                          (case (car precondition-params)
                                            (combinations 'combinations)
                                            (dot-products 'dot-products))
                                          state)
                     '(nil))))
          (let (pre-results db-updates)  ;process this action
            (when-debug>= 4 (format t "~&~A" name))
            (setf pre-results
              (remove-if #'null (mapcar (lambda (pinsts)  ;nil = failed precondition
                                          (apply iprecondition state pinsts))
                                        precondition-instantiations)))
            (when-debug>= 5
              (let ((*package* (find-package :ww)))
                (ut::prt precondition-types precondition-variables
                         precondition-instantiations pre-results)))
            (when (null pre-results)
              (next-iteration))
            (setf db-updates  ;returns list of update structures
              (mapcan (lambda (pre-result)
                        (if (eql pre-result t)
                          (funcall ieffect state)
                          (apply ieffect state pre-result)))
                      pre-results))
            ;(ut::prt db-updates)
            (setf db-updates 
              (delete-duplicates db-updates
                                 :test (lambda (upd1 upd2)
                                         (alexandria:set-equal upd1 upd2 :test #'equal))
                                 :key #'update-changes))
            (setf db-updates
              (iter (for db-update in db-updates)
                    (collect (order-propositions db-update))))
            (when-debug>= 4
              (let ((*package* (find-package :ww)))
                (ut::prt db-updates)))
            (let ((child-states (get-new-states state action db-updates)))
              (when (fboundp 'heuristic?)
                (dolist (child-state child-states)
                  (setf (problem-state-heuristic child-state) (funcall 'heuristic? child-state))))
              (alexandria:appendf children child-states)))))
    (nreverse children)))  ;put first action child states first


(defun get-new-states (state action db-updates)
  ;Creates new states given current state and the new updates.
  (mapcan
      (lambda (db-update)
        (let ((act-state (initialize-act-state action state db-update))  ;act-state from action
              net-state new-state)
          (when act-state  ;no new act-state if wait action was cancelled
            (if *happenings*
              (ut::mvs (net-state new-state) (amend-happenings state act-state))  ;net-state idb includes happenings
              (if (and *constraint* 
                       (not (funcall (symbol-function '*constraint*) act-state))) ;violated
                (setf new-state nil)
                (setf new-state act-state)))
            (when-debug>= 4
              (if net-state
                (when *constraint*
                  (format t "~&  ***NO CONSTRAINT VIOLATION"))
                (when *constraint* 
                  (format t "~&  ***CONSTRAINT VIOLATED"))))
            (when new-state
              (list (setf new-state (process-followup-updates act-state db-update)))))))
      db-updates))


(defun initialize-act-state (action state db-update)
  ;Returns a new child of state incorporating action db-update list,
  ;or nil if repeating previous wait action.
  (declare (action action) (problem-state state) (update db-update))
  (unless (and *happenings*
               (eql (action-name action) 'wait)
               (eql (problem-state-name state) 'wait))
    (create-action-state action state db-update)))  ;create non-wait state


(defun create-action-state (action state db-update)
  ;Creates a new wait or non-wait state.
  (let* ((new-state-idb (copy-db (problem-state-idb state)))
         (new-action-duration (if (eql (action-name action) 'wait)
                                (- (get-next-event-time state) (problem-state-time state))
                                (action-duration action)))
         (new-state-instantiations (if (eql (action-name action) 'wait)
                                     (list new-action-duration)
                                     (update-instantiations db-update))))
    (remhash (gethash 'waiting *constant-integers*) new-state-idb)  ;if prior was wait
    (make-problem-state
       :name (action-name action)
       :instantiations new-state-instantiations
       :happenings nil  ;to be updated by happenings
       :time (+ (problem-state-time state) new-action-duration)
       :value (update-value db-update)
       :idb (revise new-state-idb (update-changes db-update))
       :hidb (copy-db (problem-state-hidb state)))))  ;to be updated by happenings


(defun get-wait-happenings (state)
  (iter (for (object (index time direction)) in (problem-state-happenings state))
    (for event in (problem-state-happenings state))
    (for ref-time = (car (aref (get object :events) index)))
    (if (<= time (problem-state-time state))
      (collect (get-following-happening state object index time direction ref-time))
      (collect event))))

           
(defun get-next-event-time (state)
  ;Returns the time of the next happening event, considering all objects.
  (declare (problem-state state))
  (loop for (nil (nil time nil)) in (problem-state-happenings state)
        minimize time))


(defun process-followup-updates (net-state db-update)
  ;triggering forms saved previously during effect apply
  (iter (for followup in (update-followups db-update))
        (when-debug>= 4
          (ut::prt followup))
        (for returns = (sort (apply (car followup) net-state (cdr followup))
                                   #'(lambda (x y) 
                                       (declare (ignore y))
                                       (and (listp x) (eq (car x) 'not)))))  ;get ordered updates
        (when-debug>= 4
          (ut::prt returns))
        (revise (problem-state-idb net-state) returns)
        ;(when-debug>= 4 (ut::prt state))
        (finally (return-from process-followup-updates net-state))))


(defun expand (state)
  ;Returns the new states.
  (declare (problem-state state))   
  (unless (and (fboundp 'prune?) (funcall 'prune? state)) ;don't expand state further if bounded 
    (generate-children state)))


(defun estimate-to-goal (state)
  ;Heuristic (h) for estimating distance to a goal state from this state.
  ;Return 0 to use no heuristic.
  (declare (problem-state state) (ignore state))
  0)
