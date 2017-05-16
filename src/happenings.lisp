;;; Filename: happenings.lisp

;;; Functions for processing happening events in planning.


(in-package :pl)


(defun update_happenings (state act_state)
  ;Updates act_state with happenings up through the action completion time for all happenings,
  ;and checks for constraint violation along the way.
  (declare (problem_state state act_state) (special *happenings*))
  (when (null *happenings*)
    (return-from update_happenings act_state))
  (let* ((hap_state (copy_problem_state state))  ;initialization
         (net_state (copy_problem_state act_state))  ;initialization
         (obj_hap_updates (iter (for object in *happenings*)
                                (collect (get_object_happening_update object state
                                                (problem_state-time act_state)))))
         (next_hap_update (iter (for obj_update in obj_hap_updates)
                                (finding obj_update maximizing (second (second obj_update)))))
         (hap_updates (loop for obj_update in obj_hap_updates
                          append (second obj_update))))
    ;(ut::prt obj_hap_updates next_hap_update hap_updates)
    (setf (problem_state-happenings net_state) (car next_hap_update))
    (when (null hap_updates)
      (when (>= *debug* 3) (ut::prt act_state hap_state net_state))
      (if (constraint_violated act_state hap_state net_state)
          (return-from update_happenings nil)
        (return-from update_happenings net_state)))
    (setf hap_updates (sort hap_updates #'< :key #'car))
    (iter (for hap_update in hap_updates)  ;compute final net_state
          (revise (problem_state-db net_state) (cdr hap_update)))
    (iter (for hap_update in hap_updates)
          (revise (problem_state-db hap_state) (cdr hap_update))
          (when (>= *debug* 3) (ut::prt act_state hap_state net_state))
          (when (constraint_violated act_state hap_state net_state)
            (return-from update_happenings nil))  ;cancel action and exit
          (finally (return net_state)))))  ;no constraint violations encountered
    

(defun get_object_happening_update (object state action_completion_time)
  ;Incrementally updates state with the happenings for object up thru completion time,
  ;and determines if a constraint is violated along the way.
  (declare (symbol object) (problem_state state))
  (when (interrupt_condition object state)  ;inactive object causes no updates
    (return-from get_object_happening_update 
      (list (problem_state-happenings state) nil))) ;no hap updates
  (destructuring-bind (index time direction) (getf (problem_state-happenings state) object)
    (let* ((events (get object :events))
           (n (1- (length events)))  ;maximum index
           (repeat (get object :repeat))
           (hap_state (copy_problem_state state))
           updates)
      (when (>= *debug* 3)
        (ut::prt action_completion_time events n index direction time repeat) (terpri))
      (loop while (<= time action_completion_time) do  ;(ut::prt time index)
        (destructuring-bind (event_time . literals) (aref events index)
          (let ((comp_lits (complement_literals literals)))
            (ecase direction
              (+1 (push (aref events index) updates)
                  (revise (problem_state-db hap_state) literals))
              (-1 (push (cons event_time comp_lits) updates)
                  (revise (problem_state-db hap_state) comp_lits)))))  ;(ut::prt updates)
        (cond ((rebound_condition object hap_state) ;(print 0)  ;put before other conditions below
               (ecase direction   ;time to rebound
                 (1  (incf time (- (first (aref events (1+ index)))
                                   (first (aref events index)))))
                 (-1 (incf time (- (first (aref events index))
                                   (first (aref events (1- index)))))))
               (setf direction (- direction)))  ;keep index the same but reverse direction
              ;(ut::prt 'rebounded index time direction action_completion_time)
              ((and (= index n) (= direction 1))  ;(print 1) ;at last index going forward
               (if repeat 
                   (progn (incf time (first (aref events 0)))
                     (setf index 0))  ;(ut::prt time index))  ;setup for next update
                 (loop-finish)))
              ((and (= index 0) (= direction -1)) ;(print 2) ;at first index going backward
               (if repeat 
                   (progn (incf time (first (aref events 0)))
                     (setf index n)  ;(ut::prt time index)
                     (loop-finish))))
              ((and (>= index 0) (= direction 1))  ;(print 3)
               (incf time (- (first (aref events (1+ index)))
                             (first (aref events index))))
               (incf index))  ;(ut::prt time index))
              ((and (<= index n) (= direction -1))   ;(print 4)
               (incf time (- (first (aref events index))
                             (first (aref events (1- index)))))
               (decf index))))  ;(ut::prt time index))
      `((,object (,index ,time ,direction)) ,updates))))  ;prefix is next possible update


(defun interrupt_condition (object state)
  ;Determines if the interrupt function for object is satisfied in this state.
  ;Eg, if the object is currently being jammed, and therefore disabled.
  (declare (symbol object) (problem_state state))
  (ut::if-it (get object :interrupt_fn)
             (funcall ut::it state)))


(defun rebound_condition (object new_state)
  ;Determines if a rebound condition is satisfied in this state.
  (declare (symbol object) (problem_state new_state))
  (ut::if-it (get object :rebound_fn)
             (funcall ut::it new_state)))
 

(defun constraint_violated (act_state hap_state net_state)
  ;Determines whether the input states violate a constraint or not.
  (declare (problem_state act_state hap_state net_state) (special *constraint*))
  (or (and (funcall (symbol-function '*constraint*) act_state)
           (funcall (symbol-function '*constraint*) hap_state))
       (funcall (symbol-function '*constraint*) net_state)))
