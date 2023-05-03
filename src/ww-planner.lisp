;;; Filename: ww-planner.lisp

;;; Setup to solve planning problems.


(in-package :ww)


;;;;;;;;;;;;;;;;;;;Search Functions;;;;;;;;;;;;;;;;;;;


(defun state-equal-p (state1 state2)
  (declare (problem-state state1 state2))
  (equalp (problem-state.idb state1) (problem-state.idb state2)))


(defun unique-states (states &optional (n (length states)))
  "Collects the first n states that are unique."
  (let ((uniques (remove-duplicates states :from-end t :test #'state-equal-p)))
    (subseq uniques 0 n)))
  
#|
(defun equal-idb (idb1 idb2)
  (declare (hash-table idb1 idb2))
  (and (= (hash-table-count idb1) (hash-table-count idb2))
       (loop for key1 fixnum being the hash-keys in idb1 using (hash-value val1)
             always (let ((val2 (gethash key1 idb2)))
                      (when val2
                        (or (eql val1 val2)  ;for val1 = val2 = t
                            (every (lambda (x y)
                                     (or (equalp x y)
                                         (and (consp x)
                                              (consp y)
                                              (alexandria:set-equal x y))))
                                   val1 val2)))))))


(defun state-equal-p (state1 state2)
  (declare (problem-state state1 state2))
  (equal-idb (problem-state.idb state1) (problem-state.idb state2)))
|#

;(defun state-equal-p-hash (state)
;  (declare (problem-state state))
;  ;can't use sxhash for a hash table object
;  (sxhash (problem-state.idb state)))


(defun state-equal-p-hash (state)
  (declare (problem-state state))
  (let ((sum 0))
    (declare (fixnum sum))
    (maphash (lambda (key value)
               (declare (fixnum key) (ignore value))
               (incf sum key))
      (problem-state.idb state))
    sum))


(defun initialize ()
  "Initialize problem."
  )


;(defun finalize ()
;  "Cleanup after problem."
;  )


(defun record-move (parent state)
  "Returns some user-friendly representation of the move from the parent state
   to the current state."
  (declare (problem-state state parent) (ignore parent))
  (with-slots (name instantiations time) state
    (list time (cons name instantiations))))

(defun goal (state)
  "Returns t or nil depending on if state is a goal state."
  (declare (problem-state state))
  (funcall (symbol-function '*goal*) state))

 
(defun do-init-action-updates (state)  ;add init actions to start-state
  "Checks precondition of each init-action,
   and if true, then updates db and static-db according to each init-action effect."
  (declare (problem-state state))
  (when *init-actions*
    (format t "~&Adding init-action propositions to initial database...~%"))
  (iter (for init-action in *init-actions*)
    (with-slots (name precondition-params precondition-types precondition-instantiations
                 precondition-lambda effect-lambda)
        init-action
      (format t "~&~A...~%" name)
      (let ((pre-fn (compile nil precondition-lambda))
            (eff-fn (compile nil effect-lambda))
            pre-results updated-dbs)
        (setf pre-results
             (remove-if #'null (mapcar (lambda (pinsts)
                                         (apply pre-fn state pinsts))
                                       precondition-instantiations)))
        (when (null pre-results)
          (next-iteration))
        (setf updated-dbs  ;returns list of update structures
              (mapcan (lambda (pre-result)
                        (if (eql pre-result t)
                          (funcall eff-fn state)
                          (apply eff-fn state pre-result)))
                pre-results))
        (dolist (updated-db updated-dbs)  ;merge updates into *db* and *static-db*
          (maphash (lambda (key val)
                     (let ((proposition (convert-to-proposition key)))
                       (if (gethash (car proposition) *relations*)
                         (setf (gethash proposition *db*) val)
                         (setf (gethash proposition *static-db*) val))))
                   (update.changes updated-db)))))))


;(defun order-propositions (updated-db)
;  "NOTs first so addhash db not removed by later remhash."
;  (ut::sortf (update.changes updated-db)
;    #'(lambda (x y) 
;        (declare (ignore y))
;        (and (listp x) (eql (car x) 'not))))
;  updated-db)


(defun generate-children (state)
  "Returns the legitimate children of a state. Checks precondition of each action,
   and if true, then updates db according to action effects."
  (declare (problem-state state))
  (let (children)
    (iter (for action in *actions*)
      (with-slots (name iprecondition precondition-params precondition-variables
                   precondition-types restriction dynamic precondition-instantiations ieffect)
                  action
        #+:wouldwork-debug (when (>= *debug* 4)
                             (format t "~%~A" name))
        (when dynamic  ;holds the pre-types = lists + query calls
          (setf precondition-instantiations  ;overrides previous instantiations if dynamic
            (instantiate-types (mapcar (lambda (inst)
                                         (if (member (car inst) *query-names*)
                                           (apply (car inst) state (cdr inst))  ;eg, (fn state ?x ?y)
                                           inst))
                                       dynamic)
                               restriction))
          (when (some #'null precondition-instantiations)  ;no full instantiation possible
            (next-iteration)))
        (let (pre-results updated-dbs)  ;process this action
          (setf pre-results
            (remove-if #'null (mapcar (lambda (pinsts)  ;nil = failed precondition
                                        (apply iprecondition state pinsts))
                                      precondition-instantiations)))
          #+:wouldwork-debug (when (>= *debug* 5)
                               (let ((*package* (find-package :ww)))
                                 (ut::prt precondition-types precondition-variables
                                          precondition-instantiations pre-results)))
          (when (null pre-results)
            #+:wouldwork-debug (when (>= *debug* 5)
                                 (terpri))
            (next-iteration))
          (setf updated-dbs  ;returns list of update structures
            (mapcan (lambda (pre-result)
                      (if (eql pre-result t)
                        (funcall ieffect state)
                        (apply ieffect state pre-result)))
              pre-results))
          #+:wouldwork-debug (when (>= *debug* 4)
                               (let ((*package* (find-package :ww)))
                                 (format t "  UPDATED-DBS/~D =>~%" (length updated-dbs))
                                 (dolist (updated-db updated-dbs)
                                   (format t "~A,~A~%" (or (list-database (update.changes updated-db)) nil)
                                                       (update.value updated-db)))
                                 (terpri)))
          (let ((child-states (get-new-states state action updated-dbs)))  ;keep idb & hidb separate
            (when (fboundp 'heuristic?)
              (dolist (child-state child-states)
                (setf (problem-state.heuristic child-state)
                  (funcall 'heuristic? child-state))))
            (alexandria:appendf children child-states)))))
    (nreverse children)))  ;put first action child states first


(defun get-new-states (state action updated-dbs)
  "Creates new states given current state and the new updates."
  (mapcan
      (lambda (updated-db)
        (let ((act-state (initialize-act-state action state updated-db))  ;act-state from action
              net-state new-state)
          (when act-state  ;no new act-state if wait action was cancelled
            (if *happenings*
              (ut::mvs (net-state new-state) (amend-happenings state act-state))  ;check for violation
              (if (and *constraint* 
                       (not (funcall (symbol-function '*constraint*) act-state))) ;violated
                (setf new-state nil)
                (setf new-state act-state)))
            #+:wouldwork-debug (when (>= *debug* 4)
                                 (if net-state
                                   (when *constraint*
                                     (format t "~&  ***NO CONSTRAINT VIOLATION"))
                                   (when *constraint* 
                                     (format t "~&  ***CONSTRAINT VIOLATED"))))
                               (when new-state
                                 (list (setf new-state 
                                             (process-followup-updates act-state updated-db)))))))
      updated-dbs))


(defun initialize-act-state (action state updated-db)
  "Returns a new child of state incorporating action updated-db list,
   or nil if repeating previous wait action."
  (declare (action action) (problem-state state) (update updated-db))
  (unless (and *happenings*
               (eql (action.name action) 'wait)
               (eql (problem-state.name state) 'wait))  ;previous state is also wait
    (create-action-state action state updated-db)))  ;create non-wait state


(defun create-action-state (action state updated-db)  ;if action is non-wait
  "Creates a new wait or non-wait state."
  (let* ((new-state-idb (update.changes updated-db))
         (new-action-duration (if (eql (action.name action) 'wait)
                                (- (get-next-event-time state) (problem-state.time state))
                                (action.duration action)))
         (new-state-instantiations (if (eql (action.name action) 'wait)
                                     (list new-action-duration)
                                     (update.instantiations updated-db))))
    (when (eql (problem-state.name state) 'wait)
      (remhash (gethash 'waiting *constant-integers*) new-state-idb))  ;if prior was wait
    (make-problem-state
       :name (action.name action)
       :instantiations new-state-instantiations
       :happenings nil  ;to be updated by happenings
       :time (+ (problem-state.time state) new-action-duration)
       :value (update.value updated-db)
       :idb new-state-idb
       :hidb (copy-idb (problem-state.hidb state)))))  ;to be updated by happenings


(defun get-wait-happenings (state)
  (iter (for (object (index time direction)) in (problem-state.happenings state))
    (for event in (problem-state.happenings state))
    (for ref-time = (car (aref (get object :events) index)))
    (if (<= time (problem-state.time state))
      (collect (get-following-happening state object index time direction ref-time))
      (collect event))))

           
(defun get-next-event-time (state)
  "Returns the time of the next happening event, considering all objects."
  (declare (problem-state state))
  (iter (for (nil (nil time nil)) in (problem-state.happenings state))
    (minimizing time)))


(defun process-followup-updates (net-state updated-db)
  "Triggering forms are saved previously during effect apply."
  (declare (ignorable updated-db))
  (iter (for followup in nil)   ;(update.followups updated-db))
    #+:wouldwork-debug (when (>= *debug* 4)
                         (ut::prt followup))
    (for returns = (sort (apply (car followup) net-state (cdr followup))
                         #'(lambda (x y) 
                             (declare (ignore y))
                             (and (listp x)
                                  (eql (car x) 'not)))))  ;get ordered updates
    #+:wouldwork-debug (when (>= *debug* 4)
                         (ut::prt returns))
    (revise (problem-state.idb net-state) returns)
    (finally (return-from process-followup-updates net-state))))


(defun expand (state)
  "Returns the new states."
  (declare (problem-state state))   
  (unless (and (fboundp 'prune?) (funcall 'prune? state)) ;don't expand state further if bounded 
    (generate-children state)))


(defun estimate-to-goal (state)
  "Heuristic (h) for estimating distance to a goal state from this state;
   Return 0 to use no heuristic."
  (declare (problem-state state) (ignore state))
  0)
