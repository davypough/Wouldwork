;;; Filename: planner.lisp

;;; Setup to solve planning problems.


(in-package :ww)


(declaim
 ;Type specs. Put after defstruct forms.
 (problem-state *start-state*)
 (inline state-key))


;;;;;;;;;;;;;;;;;;;Search Functions;;;;;;;;;;;;;;;;;;;


(defun state-key (state)    ;used for determining if two states are the same
  ;Default will just return entire state. Specify state access key
  ;for greater efficiency to avoid searching redundant states.  States are
  ;the same if state-keys are equalp.
  (declare (problem-state state))
  (problem-state-db state))


(defun same-state (state1 state2)
  ;Test to determine if two states are the same.
  (declare (problem-state state1 state2))
  (equalp (problem-state-db state1) (problem-state-db state2)))


(defun initialize ()
  ;Initialize problem.
  (declare (special *start-state* *db* *happenings*))
  (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
  (with-slots (name instantiations time happenings db) *start-state*
    (let ((first-event-time (loop for object in *happenings* 
                              minimize (car (aref (get object :events) 0)))))
      (setf db (alexandria:copy-hash-table *db*)
            happenings (loop for object in *happenings* ;property list of happening objects
                           append (list object 
                                        (list 0 first-event-time +1)))  ;next (index time direction)
            time 0
            instantiations nil
            name nil))))


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


(defun generate-children (state)
  ;Returns the legitimate children of a state. Checks precondition of each action,
  ;and if true, then updates db according to action effects.
  (declare (problem-state state) (special *debug* *actions* *types*))
  (let (children)
    (dolist (action *actions*)
      (with-slots (name duration precondition precondition-variables precondition-types
                        effect effect-variables effect-types)
          action
        (when (>= *debug* 3) (format t "~%~A ~A" name effect-variables))
        (let (pre-result fluent-values)
          (dolist (pinsts (distinct-instantiations precondition-types))
            ;precondition will evaluate to true or false given current db
            ;apply instantiates an action lambda expression & evaluates it
            (when (setq pre-result (apply precondition (cons state pinsts)))
              (when (listp pre-result)
                (setq fluent-values pre-result))
              ;(ut::prt precondition-types pre-result pinsts fluent-values)
              ;update db since precondition satisfied
              (let* ((eobjects (map 'list 
                                 #'(lambda (etype evar)
                                     (ut::if-it (position evar precondition-variables :test #'eq)
                                                (list (nth ut::it pinsts))
                                                (gethash etype *types*)))
                                 effect-types
                                 effect-variables))
                     (all-einstantiations (if eobjects
                                              (apply #'alexandria:map-product
                                                     #'list
                                                     eobjects)
                                            nil))
                     (distinct-einstantiations (delete-if-not #'alexandria:setp 
                                                              all-einstantiations))
                     db-update db-updates)
                ;(ut::prt effect-variables effect-types distinct-einstantiations)
                (when (null distinct-einstantiations)
                  (setf distinct-einstantiations (list nil)))
                (dolist (einsts distinct-einstantiations)
                  ;each db update is a successful instantiation of the current action effect
                  (setq db-update   ;((literal1 literal2 ...) (inst1 inst2 ...))
                        (apply-effect einsts fluent-values action state))
                  (when (car db-update) ;otherwise skip update if no updates available
                    (setq db-update (order-update db-update))
                    (pushnew db-update db-updates :test #'equal))) ;throw out redundancies
                (when (>= *debug* 3) (ut::prt distinct-einstantiations db-updates))
                (dolist (db-update db-updates)
                  (let ((act-state (initialize-act-state action state db-update)))
                     ;(ut::prt act-state)
                     (when act-state
                       (let ((net-state (update-happenings state act-state)))
                         ;(ut::show (problem-state-db net-state))
                         (when (>= *debug* 3)
                           (if net-state
                               (format t "~&    NO CONSTRAINT VIOLATION")
                             (format t "~&    CONSTRAINT VIOLATION")))
                         (when net-state
                           (pushnew net-state children :test #'same-state))))))))))))
    children))


(defun initialize-act-state (action state db-update)
  ;Returns a new child of state incorporating action db-update,
  ;or nil if wait action and no point in waiting.
  (declare (action action) (problem-state state) (list db-update)
           (special *happenings*))
  (if (eql (action-name action) 'wait)
      (if (and *happenings* (not (eql (problem-state-name state) 'wait))) ;previous act not wait
          (let ((next-event-time (get-next-event-time state)))
            (if (>= (problem-state-time state) next-event-time)  ;cancel wait action
                nil  ;no more exogenous events to wait for
              (progn (setf (action-duration action)
                       (- next-event-time (problem-state-time state)))
                (create-act-state action state db-update))))
        nil)  ;no exogenous events to wait for, or previous wait
    (create-act-state action state db-update)))  ;create non-wait state   
      
           
(defun get-next-event-time (state)
  ;Returns the time of the next event, considering all objects.
  (declare (problem-state state) (special *happenings*))
  (let ((next-event-time (loop for (nil (nil time nil)) 
                             on (problem-state-happenings state) by #'cddr
                               minimize time)))
    (when (>= *debug* 3) (ut::prt next-event-time))
    next-event-time))


(defun create-act-state (action state db-update)
  ;Creates a new wait or non-wait state.
  (with-slots (name duration) action
    (make-problem-state
     :name name
     :instantiations (if (eql name 'wait) (list duration) (second db-update))
     :happenings nil  ;TBD by happenings
     :time (+ (problem-state-time state) duration)
     :db (revise (alexandria:copy-hash-table (problem-state-db state)) (first db-update)))))


(defun expand (state)
  ;Returns the new states.
  (declare (problem-state state))   
  (when (not (bounded state))        ;don't expand state further if bounded 
    (generate-children state)))


(defun estimate-to-goal (state)
  ;Heuristic (h) for estimating distance to a goal state from this state.
  ;Return 0 to use no heuristic.
  (declare (problem-state state) (ignore state))
  0)
