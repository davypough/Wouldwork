;;; Filename: planner.lisp

;;; Setup to solve planning problems.


(in-package :pl)


(declaim
 ;Type specs. Put after defstruct forms.
 (problem_state *start_state*)
 (inline state_key))


;;;;;;;;;;;;;;;;;;;Search Functions;;;;;;;;;;;;;;;;;;;


(defun state_key (state)    ;used for determining if two states are the same
  ;Default will just return entire state. Specify state access key
  ;for greater efficiency to avoid searching redundant states.  States are
  ;the same if state_keys are equalp.
  (declare (problem_state state))
  (problem_state-db state))


(defun same_state (state1 state2)
  ;Test to determine if two states are the same.
  (declare (problem_state state1 state2))
  (equalp (problem_state-db state1) (problem_state-db state2)))


(defun initialize ()
  ;Initialize problem.
  (declare (special *start_state* *db* *happenings*))
  (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
  (with-slots (name instantiations time happenings db) *start_state*
    (let ((first_event_time (loop for object in *happenings* 
                              minimize (car (aref (get object :events) 0)))))
      (setf db (alexandria:copy-hash-table *db*)
            happenings (loop for object in *happenings* ;property list of happening objects
                           append (list object 
                                        (list 0 first_event_time +1)))  ;next (index time direction)
            time 0
            instantiations nil
            name nil))))


(defun finalize ()
  ;Return final answer to problem.
  )

(defun record_move (parent state)
  ;Returns some user-friendly representation of the move from the parent state
  ;to the current state.
  (declare (problem_state state parent) (ignore parent))
  (with-slots (name instantiations time) state
    (list time (cons name instantiations))))


(defun goal (state)
  ;Returns t or nil depending on if state is a goal state.
  (declare (problem_state state))
  (achieve_goal state))

 
(defun bounded (state)
  ;Determines whether to bound (prune) a state
  ;eg, when (<= (problem_state-value state)
  ;             (problem_state-value *best_state*))
  (declare (problem_state state) (ignore state))
  nil)


(defun generate_children (state)
  ;Returns the legitimate children of a state. Checks precondition of each action,
  ;and if true, then updates db according to action effects.
  (declare (problem_state state) (special *debug* *actions* *types*))
  (let (children)
    (dolist (action *actions*)
      (with-slots (name duration precondition precondition_variables precondition_types
                        effect effect_variables effect_types)
          action
        (when (>= *debug* 3) (format t "~%~A ~A" name effect_variables))
        (let (pre_result fluent_values)
          (dolist (pinsts (distinct_instantiations precondition_types))
            ;precondition will evaluate to true or false given current db
            ;apply instantiates an action lambda expression & evaluates it
            (when (setq pre_result (apply precondition (cons state pinsts)))
              (when (listp pre_result)
                (setq fluent_values pre_result))
              ;(ut::prt precondition_variables precondition_types pre_result pinsts fluent_values)
              ;update db since precondition satisfied
              (let* ((eobjects (map 'list 
                                 #'(lambda (etype evar)
                                     (ut::if-it (position evar precondition_variables :test #'eq)
                                                (list (nth ut::it pinsts))
                                                (gethash etype *types*)))
                                 effect_types
                                 effect_variables))
                     (all_einstantiations (if eobjects
                                              (apply #'alexandria:map-product
                                                     #'list
                                                     eobjects)
                                            nil))
                     (distinct_einstantiations (delete-if-not #'alexandria:setp 
                                                              all_einstantiations))
                     db_update db_updates)
                ;(ut::prt effect_variables effect_types distinct_einstantiations)
                (when (null distinct_einstantiations)
                  (setf distinct_einstantiations (list nil)))
                (dolist (einsts distinct_einstantiations)
                  ;each db update is a successful instantiation of the current action effect
                  (setq db_update   ;((literal1 literal2 ...) (inst1 inst2 ...))
                        (apply_effect einsts fluent_values action state))
                  (when (car db_update) ;otherwise skip update if no updates available
                    (setq db_update (order_update db_update))
                    (pushnew db_update db_updates :test #'equal))) ;throw out redundancies
                (when (>= *debug* 3) (ut::prt distinct_einstantiations db_updates))
                (dolist (db_update db_updates)
                  (let ((act_state (initialize_act_state action state db_update)))
                     ;(ut::prt act_state)
                     (when act_state
                       (let ((net_state (update_happenings state act_state)))
                         (when (>= *debug* 3)
                           (if net_state
                               (format t "~&    NO CONSTRAINT VIOLATION")
                             (format t "~&    CONSTRAINT VIOLATION")))
                         (when net_state
                           (pushnew net_state children :test #'same_state))))))))))))
    children))


(defun initialize_act_state (action state db_update)
  ;Returns a new child of state incorporating action db_update,
  ;or nil if wait action and no point in waiting.
  (declare (action action) (problem_state state) (list db_update)
           (special *happenings*))
  (if (eql (action-name action) 'wait)
      (if (and *happenings* (not (eql (problem_state-name state) 'wait))) ;previous act not wait
          (let ((next_event_time (get_next_event_time state)))
            (if (>= (problem_state-time state) next_event_time)  ;cancel wait action
                nil  ;no more exogenous events to wait for
              (progn (setf (action-duration action)
                       (- next_event_time (problem_state-time state)))
                (create_act_state action state db_update))))
        nil)  ;no exogenous events to wait for, or previous wait
    (create_act_state action state db_update)))  ;create non-wait state   
      
           
(defun get_next_event_time (state)
  ;Returns the time of the next event, considering all objects.
  (declare (problem_state state) (special *happenings*))
  (let ((next_event_time (loop for (nil (nil time nil)) 
                             on (problem_state-happenings state) by #'cddr
                               minimize time)))
    (when (>= *debug* 3) (ut::prt next_event_time))
    next_event_time))


(defun create_act_state (action state db_update)
  ;Creates a new wait or non-wait state.
  (with-slots (name duration) action
    (make-problem_state
     :name name
     :instantiations (if (eql name 'wait) (list duration) (second db_update))
     :happenings nil  ;TBD by happenings
     :time (+ (problem_state-time state) duration)
     :db (revise (alexandria:copy-hash-table (problem_state-db state)) (first db_update)))))


(defun expand (state)
  ;Returns the new states.
  (declare (problem_state state))   
  (when (not (bounded state))        ;don't expand state further if bounded 
    (generate_children state)))


(defun estimate_to_goal (state)
  ;Heuristic (h) for estimating distance to a goal state from this state.
  ;Return 0 to use no heuristic.
  (declare (problem_state state) (ignore state))
  0)
