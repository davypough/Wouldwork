;;; Filename: happenings.lisp

;;; Functions for processing happening events in planning.


(in-package :ww)


(defun update-happenings (state act-state)
  ;Updates act-state with happenings up through the action completion time for all happenings,
  ;and checks for constraint violation along the way.
  (declare (problem-state state act-state) (special *happenings*))
  (when (null *happenings*)
    (return-from update-happenings act-state))
  (let* ((hap-state (copy-problem-state state))  ;initialization
         (net-state (copy-problem-state act-state))  ;initialization
         (obj-hap-updates (iter (for object in *happenings*)
                                (collect (get-object-happening-update object state
                                                (problem-state-time act-state)))))
         (next-hap-update (iter (for obj-update in obj-hap-updates)
                                (finding obj-update maximizing (second (second obj-update)))))
         (hap-updates (loop for obj-update in obj-hap-updates
                          append (second obj-update))))
    ;(ut::prt obj-hap-updates next-hap-update hap-updates)
    (setf (problem-state-happenings net-state) (car next-hap-update))
    (when (null hap-updates)
      (when (>= *debug* 3) (ut::prt act-state hap-state net-state))
      (if (constraint-violated act-state hap-state net-state)
          (return-from update-happenings nil)
        (return-from update-happenings net-state)))
    (setf hap-updates (sort hap-updates #'< :key #'car))
    (iter (for hap-update in hap-updates)  ;compute final net-state
          (revise (problem-state-db net-state) (cdr hap-update)))
    (iter (for hap-update in hap-updates)
          (revise (problem-state-db hap-state) (cdr hap-update))
          (when (>= *debug* 3) (ut::prt act-state hap-state net-state))
          (when (constraint-violated act-state hap-state net-state)
            (return-from update-happenings nil))  ;cancel action and exit
          (finally (return net-state)))))  ;no constraint violations encountered
    

(defun get-object-happening-update (object state action-completion-time)
  ;Incrementally updates state with the happenings for object up thru completion time,
  ;and determines if a constraint is violated along the way.
  (declare (symbol object) (problem-state state))
  (when (interrupt-condition object state)  ;inactive object causes no updates
    (return-from get-object-happening-update 
      (list (problem-state-happenings state) nil))) ;no hap updates
  (destructuring-bind (index time direction) (getf (problem-state-happenings state) object)
    (let* ((events (get object :events))
           (n (1- (length events)))  ;maximum index
           (repeat (get object :repeat))
           (hap-state (copy-problem-state state))
           updates)
      (when (>= *debug* 3)
        (ut::prt action-completion-time events n index direction time repeat) (terpri))
      (loop while (<= time action-completion-time) do  ;(ut::prt time index)
        (destructuring-bind (event-time . literals) (aref events index)
          (let ((comp-lits (complement-literals literals)))
            (ecase direction
              (+1 (push (aref events index) updates)
                  (revise (problem-state-db hap-state) literals))
              (-1 (push (cons event-time comp-lits) updates)
                  (revise (problem-state-db hap-state) comp-lits)))))  ;(ut::prt updates)
        (cond ((rebound-condition object hap-state) ;(print 0)  ;put before other conditions below
               (ecase direction   ;time to rebound
                 (1  (incf time (- (first (aref events (1+ index)))
                                   (first (aref events index)))))
                 (-1 (incf time (- (first (aref events index))
                                   (first (aref events (1- index)))))))
               (setf direction (- direction)))  ;keep index the same but reverse direction
              ;(ut::prt 'rebounded index time direction action-completion-time)
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


(defun interrupt-condition (object state)
  ;Determines if the interrupt function for object is satisfied in this state.
  ;Eg, if the object is currently being jammed, and therefore disabled.
  (declare (symbol object) (problem-state state))
  (ut::if-it (get object :interrupt-fn)
             (funcall ut::it state)))


(defun rebound-condition (object new-state)
  ;Determines if a rebound condition is satisfied in this state.
  (declare (symbol object) (problem-state new-state))
  (ut::if-it (get object :rebound-fn)
             (funcall ut::it new-state)))
 

(defun constraint-violated (act-state hap-state net-state)
  ;Determines whether the input states violate a constraint or not.
  (declare (problem-state act-state hap-state net-state) (special *constraint*))
  (or (and (not (funcall (symbol-function '*constraint*) act-state))
           (not (funcall (symbol-function '*constraint*) hap-state)))
      (not (funcall (symbol-function '*constraint*) net-state))))
