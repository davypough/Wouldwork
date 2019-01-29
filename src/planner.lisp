;;; Filename: planner.lisp

;;; Setup to solve planning problems.


(in-package :ww)


(declaim
 ;Type specs. Put after defstruct forms.
 (problem-state *start-state*)
 (inline state-key))


;;;;;;;;;;;;;;;;;;;Search Functions;;;;;;;;;;;;;;;;;;;


(defun state-key (state)  ;used for determining if two states are the same for tree search
  ;Default will just return entire state. Specify state access key
  ;for greater efficiency to avoid searching redundant states.  States are
  ;the same depending on same-state below.
  (declare (problem-state state))
  (problem-state-idb state))


(defun same-state (state1 state2)
  ;Test to determine if two states are the same.
  (declare (problem-state state1 state2))
  (hash-table-key-equality (problem-state-idb state1) (problem-state-idb state2)))


(defun hash-table-key-equality (ht1 ht2)
  ;Returns t if two db hash tables have the same keys.
  (declare (hash-table ht1 ht2))
  (when (= (hash-table-count ht1) (hash-table-count ht2))
    (maphash (lambda (ht1-key ht1-value)
               (declare (ignore ht1-value))
               (unless (gethash ht1-key ht2)
                 (return-from hash-table-key-equality nil)))
             ht1)
    t))
                

(defun initialize ()
  ;Initialize problem.
  )


(defun finalize ()
  ;Return final answer to problem.
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

 
(defun bounded (state)
  ;Determines whether to bound (prune) a state
  ;eg, when (<= (problem-state-value state)
  ;             (problem-state-value *best-state*))
  (declare (problem-state state) (ignore state))
  nil)


(defun do-init-action-updates (state)  ;add init actions to *start-state*
  ;Checks precondition of each init-action,
  ;and if true, then updates *db* according to each init-action effect.
  (declare (problem-state state))
  (when *init-actions*
    (format t "~&Adding init-action propositions to initial database...~%"))
  (let ()  ;no children generated
    (dolist (init-action *init-actions*)
      (with-slots (name duration precondition precondition-variables precondition-types
                        precondition-instantiations precondition-lambda effect effect-variables effect-types
                        effect-instantiations effect-lambda)
          init-action
        (when (>= *debug* 4)
          (format t "~%~A ~A" name effect-variables))
        (let ((precondition (compile nil (coerce precondition-lambda 'function)))
              (effect (compile nil (coerce effect-lambda 'function)))
              pre-result)
          (dolist (pinsts (or precondition-instantiations '(nil)))
            ;precondition will evaluate to true or false given current db
            ;apply calls an action lambda expression & evaluates it
            (when (setq pre-result (apply precondition state pinsts))
             (when (= *debug* 5)
                (ut::prt precondition-types precondition-variables 
                         pre-result))
              ;next update db since precondition satisfied
              (let* ((einstantiations (append pre-result effect-instantiations))
                     (bag-einstantiations (or (apply #'alexandria:map-product 'list einstantiations)
                                              '(nil)))
                     (set-einstantiations (remove-duplicates bag-einstantiations :test #'equal))
                     db-update db-updates)
               (when (= *debug* 5)
                  (ut::prt effect-types effect-variables einstantiations set-einstantiations))
                (dolist (einsts set-einstantiations)
                  ;each db update is a successful instantiation of the current action effect
                  (setq db-update   ;((literal1 literal2 ...) (inst1 inst2 ...))
                        (apply effect state einsts))
                  (when (car db-update) ;otherwise skip update if no updates available
                    (setq db-update (list (order-propositions db-update) einsts)) ;add instantiations
                    (push db-update db-updates)))
                (when (>= *debug* 4) 
                  (ut::prt db-updates))
                (dolist (db-update db-updates)
                    (loop for literal in (first db-update)
                        do (if (eq (car literal) 'not)
                             (if (gethash (caadr literal) *relations*)  ;dynamic relation
                               (delete-proposition (second literal) *db*)
                               (delete-proposition (second literal) *static-db*))
                             (if (gethash (car literal) *relations*)  ;dynamic relation
                               (add-proposition literal *db*)
                               (add-proposition literal *static-db*)))))))))))))


(defun generate-children (state)
  ;Returns the legitimate children of a state. Checks precondition of each action,
  ;and if true, then updates db according to action effects.
  (declare (problem-state state))
  (let (children)
    (dolist (action *actions*)
      (with-slots (name duration precondition iprecondition precondition-variables precondition-types
                        precondition-instantiations effect ieffect effect-variables effect-types
                        effect-instantiations)
          action
        (when (>= *debug* 4) 
          (format t "~%~A" name))
        ;---precondition---------------------------------------------------
        (let (pre-result)
          (dolist (pinsts (if (equal precondition-instantiations '(nil))
                            nil
                            precondition-instantiations))
            ;each pinsts is a list of var bindings associated with list of types
            ;apply calls an action lambda expression with var bindings & evaluates it
            (when (setq pre-result (apply iprecondition state pinsts))
             (when (= *debug* 5)
                (ut::prt precondition-types precondition-variables pre-result))
              ;next update db since precondition satisfied
              ;---effect----------------------------------------------------
              (let* ((einstantiations (or (mapcar (lambda (instantiation)
                                                    (nreverse (cons instantiation (reverse pre-result))))
                                            effect-instantiations)
                                          (list pre-result)))
                     (set-products (when einstantiations
                                     (mapcar (lambda (instantiation)
                                               (car (apply #'alexandria:map-product 'list instantiation)))
                                       einstantiations)))
                     db-update db-updates)
               (when (= *debug* 5)
                  (ut::prt effect-types effect-variables effect-instantiations einstantiations set-products))
                (dolist (product set-products)
                  ;each db update is a successful instantiation of the current action effect
                  (remhash (gethash 'waiting *constant-integers*) (problem-state-idb state))  ;cancel any waiting before processing this action
                  (setq db-update   ;((literal1 literal2 ...) (inst1 inst2 ...))
                       (apply ieffect state product))
                  (when (car db-update) ;otherwise skip update if no updates available
                    (setq db-update (list (order-propositions db-update) product)) ;add new instantiations
                    (push db-update db-updates)))
                (when (>= *debug* 4)
                  (ut::prt db-updates))
                (dolist (db-update db-updates)
                  (let ((act-state (initialize-act-state action state db-update))
                        net-state)
                     ;(ut::prt act-state)
                     (when act-state  ;no new act-state if wait action is cancelled
                       (if *happenings*
                         (setf net-state (update-happenings state act-state))
                         (if (and *constraint* 
                                  (not (funcall (symbol-function '*constraint*) act-state))) ;violated
                           (setf net-state nil)
                           (setf net-state act-state)))
                       ;(ut::show (problem-state-idb net-state))
                       (when (>= *debug* 4)
                         (if net-state
                           (format t "~&    ***NO CONSTRAINT VIOLATION***")
                           (format t "~&    ***CONSTRAINT VIOLATION***")))
                       (when net-state
                         (setf net-state (process-trigger-updates net-state))
                         (pushnew net-state children :test #'same-state)))))))))))
    children))


(defun initialize-act-state (action state db-update)
  ;Returns a new child of state incorporating action db-update,
  ;or nil if wait action and no point in waiting.
  (declare (action action) (problem-state state) (list db-update))
  (if (eql (action-name action) 'wait)
    (if (and *happenings* (not (eql (problem-state-name state) 'wait))) ;previous act not wait
      (let ((next-event-time (get-next-event-time state)))
        (if (>= (problem-state-time state) next-event-time)  ;cancel wait action
          (return-from initialize-act-state nil)  ;no more exogenous events to wait for
          (progn (setf (action-duration action) (- next-event-time (problem-state-time state)))
                 (create-action-state action state db-update))))
      (return-from initialize-act-state nil))  ;no exogenous events to wait for, or previous wait
    (create-action-state action state db-update)))  ;create non-wait state   
      
           
(defun get-next-event-time (state)
  ;Returns the time of the next happening event, considering all objects.
  (declare (problem-state state))
  (let ((next-event-time (loop for (nil (nil time nil)) in (problem-state-happenings state)
                               minimize time)))
    next-event-time))


(defun process-trigger-updates (state)
  ;triggering forms saved previously during effect apply
  (iter (for trigger in (nreverse *current-action-triggers*))
        (for db-update = (apply (car trigger) state (cdr trigger)))  ;get update literals
        (when (>= *debug* 4)
          (ut::prt trigger db-update))
        (revise (problem-state-idb state) db-update)
        (when (>= *debug* 4)
          (ut::prt state))
        (finally (setf *current-action-triggers* nil)
                 (return-from process-trigger-updates state))))


(defun create-action-state (action state db-update)
  ;Creates a new wait or non-wait state.
  (with-slots (name duration) action
    (make-problem-state
     :name name
     :instantiations (if (eql name 'wait)
                       (cons duration (second db-update))
                       (eff-param-instantiations (action-precondition-variables action)
                                                 (action-effect-variables action)
                                                 (second db-update)))
     :happenings (copy-tree (problem-state-happenings state))  ;to be updated by happenings
     :time (+ (problem-state-time state) duration)
     :idb (revise (alexandria:copy-hash-table (problem-state-idb state)) (first db-update)))))


(defun eff-param-instantiations (precondition-variables effect-variables pre-instantiations)
  ;Returns list of effect variable instantiations.
  (iter (for evar in effect-variables)
        (collect (iter (for pvar in precondition-variables)
                       (for pinst in pre-instantiations)
                       (when (eql pvar evar)
                         (leave pinst))))))


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
