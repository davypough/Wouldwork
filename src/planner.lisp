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
  (achieve-goal state))

 
(defun do-init-action-updates (state)  ;add init actions to start-state
  ;Checks precondition of each init-action,
  ;and if true, then updates db and static-db according to each init-action effect.
  (declare (problem-state state))
  (when *init-actions*
    (format t "~&Adding init-action propositions to initial database...~%"))
  (dolist (init-action *init-actions*)
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
            (eff-fn (compile nil effect-lambda)))
        (mapcar
          (lambda (pinsts)
            (let (pre-result db-update)
              (when (setf pre-result (apply pre-fn state pinsts))
                (setf db-update 
                  ;(order-propositions 
                  (apply eff-fn state pre-result))
                (loop for literal in (update-changes db-update)
                    do (if (eq (car literal) 'not)
                         (if (gethash (caadr literal) *relations*)
                           (delete-proposition (second literal) *db*)  ;dynamic relation
                           (delete-proposition (second literal) *static-db*))
                         (if (gethash (car literal) *relations*)
                           (add-proposition literal *db*)  ;dynamic relation
                           (add-proposition literal *static-db*)))))))
          precondition-instantiations)))))


(defun generate-children (state)
  ;Returns the legitimate children of a state. Checks precondition of each action,
  ;and if true, then updates db according to action effects.
  (declare (problem-state state))
  (let (children)
    (dolist (action *actions*)
      (with-slots (name iprecondition precondition-params precondition-variables
                   precondition-types dynamic precondition-instantiations ieffect)
          action
        ;(if (equal precondition-instantiations '(nil))
          ;(when-debug>= 4 (format t "~&~A - skipping" name))  ;get next action
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
              (mapcar (lambda (pinsts)
                        (apply iprecondition state pinsts))
                precondition-instantiations))
            (when-debug>= 5
              (let ((*package* (find-package :ww)))
                (ut::prt precondition-types precondition-variables
                         precondition-instantiations pre-results)))
            (when precondition-variables
              (alexandria:deletef pre-results nil))  ;all nils -> nil
            ;(setf pre-results '(nil))
            (setf db-updates  ;returns list of update structures
              (mapcar (lambda (pre-result)
                        (apply ieffect state pre-result))
                pre-results))
            ;(ut::prt 'first db-updates)
            (setf db-updates 
              (delete-duplicates db-updates
                                 :test (lambda (upd1 upd2)
                                         (alexandria:set-equal upd1 upd2 :test #'equal))
                                 :key #'update-changes))
            ;(ut::prt 'second db-updates)
            (setf db-updates
              (iter (for db-update in db-updates)
                    (collect (order-propositions db-update))))
            ;(ut::prt 'third db-updates)
            (when-debug>= 4
              (let ((*package* (find-package :ww)))
                (ut::prt db-updates)))
            (alexandria:appendf children (get-new-states state action db-updates)))))
    (nreverse children)))


(defun get-new-states (state action db-updates)
  ;Creates new states given current state and the new updates.
  (mapcan
      (lambda (db-update)
        (let ((act-state (initialize-act-state action state db-update))
              net-state)
          (when act-state  ;no new act-state if wait action is cancelled
            (if *happenings*
                (setf net-state (amend-happenings state act-state))
              (if (and *constraint* 
                       (not (funcall (symbol-function '*constraint*) act-state))) ;violated
                  (setf net-state nil)
                (setf net-state act-state)))
            (when-debug>= 4
              (if net-state
                  (when *constraint*
                    (format t "~&    ***NO CONSTRAINT VIOLATION***"))
                (when *constraint* 
                  (format t "~&    ***CONSTRAINT VIOLATED***"))))
            (when net-state
              (list (setf net-state (process-followup-updates net-state db-update)))))))
    db-updates))


(defun initialize-act-state (action state db-update)
  ;Returns a new child of state incorporating action db-update list,
  ;or nil if wait action and no point in waiting.
  (declare (action action) (problem-state state) (update db-update))
  (if (eql (action-name action) 'wait)
    (if (and *happenings* (not (eql (problem-state-name state) 'wait))) ;previous act not wait
      (let ((next-event-time (get-next-event-time state)))
        (if (>= (problem-state-time state) next-event-time)  ;cancel wait action
          (return-from initialize-act-state nil)  ;no more exogenous events to wait for
          (progn (setf (action-duration action) (- next-event-time (problem-state-time state)))
                 (create-action-state action state db-update))))
      (return-from initialize-act-state nil))  ;no exogenous events to wait for, or previous wait
    (let ((new-state (create-action-state action state db-update)))  ;create non-wait state
      new-state)))  
      
           
(defun get-next-event-time (state)
  ;Returns the time of the next happening event, considering all objects.
  (declare (problem-state state))
  (let ((next-event-time (loop for (nil (nil time nil)) in (problem-state-happenings state)
                               minimize time)))
    next-event-time))


(defun process-followup-updates (state db-update)
  ;triggering forms saved previously during effect apply
  (iter (for followup in (update-followups db-update))
        (when-debug>= 4 (ut::prt followup))
        (for returns = (sort (apply (car followup) state (cdr followup))
                                   #'(lambda (x y) 
                                       (declare (ignore y))
                                       (and (listp x) (eq (car x) 'not)))))  ;get ordered updates
        (when-debug>= 4 (ut::prt returns))
        (revise (problem-state-idb state) returns)
        ;(when-debug>= 4 (ut::prt state))
        (finally (return-from process-followup-updates state))))


(defun create-action-state (action state db-update)
  ;Creates a new wait or non-wait state.
  (let ((new-state-idb (alexandria:copy-hash-table (problem-state-idb state)
                                                   :key (lambda (value)
                                                          (if (listp value)
                                                            (copy-list value)
                                                            value)))))
    (remhash (gethash 'waiting *constant-integers*) new-state-idb)
    (with-slots (name duration) action
      (make-problem-state
       :name name
       :instantiations (if (eql name 'wait)
                         (cons duration (update-instantiations db-update))
                         (update-instantiations db-update))
       :happenings (copy-tree (problem-state-happenings state))  ;to be updated by happenings
       :time (+ (problem-state-time state) duration)
       :value (update-value db-update)
       :idb (revise new-state-idb (update-changes db-update))))))


(defun bounded (state)
  ;Determines whether to bound (prune) a state
  ;eg, when (<= (problem-state-value state)
  ;             (problem-state-value *best-state*))
  (declare (problem-state state) (ignore state))
  nil)


(defun expand (state)
  ;Returns the new states.
  (declare (problem-state state))   
  (when (not (bounded state))        ;don't expand state further if bounded 
    (generate-children state)))


(defun estimate-to-goal (state)
  ;Heuristic (h) for estimating distance to a goal state from this state.
  ;Return 0 to use no heuristic.
  (declare (problem-state state) (ignore state))
  ;(ignore-delete-lists-heuristic state)
  0)
