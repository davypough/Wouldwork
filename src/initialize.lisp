;;; Filename: initialize.lisp

;;; Initialization after installation.

(in-package :ww)

(defun init ()
  (format t "~&Setting up...~%")
  (setf *query-names* (nreverse *query-names*))
  (setf *update-names* (nreverse *update-names*))
  (setf *init-actions* (nreverse *init-actions*))
;  (dolist (action *actions*)  ;future backchaining
;    (install-precondition-lits action)
;    (install-effect-adds action))
  (with-slots (name instantiations happenings time value heuristic) *start-state*
    (let ((first-event-time (loop for object in *happenings* 
                              minimize (car (aref (get object :events) 0)))))
      (setf happenings (loop for object in *happenings* ;property list of happening objects
                             collect (list object 
                                          (list 0 first-event-time +1))))  ;next (index time direction)
      (setf time 0.0)
      (setf value 0.0)
      (setf heuristic 0.0)
      (setf instantiations nil)
      (setf name nil)))
  (do-init-action-updates *start-state*)  ;updates db & static-db, but not start-state idb yet
  (when (> (length *happenings*) 1)
    (setq *happenings* (sort *happenings* #'< :key (lambda (object)
                                                     (first (aref (get object :events) 0))))))
  (format t "Converting propositions to integers...~%")
  (do-integer-conversion)  ;allows integer hashtable db lookups, adds start-state idb
  (convert-ilambdas-to-fns) ;and compile
  (if *actions*
    (progn (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
           (setf *min-action-duration* (reduce #'min *actions* :key #'action-duration)))
    (format t "~%NOTE: There are no defined actions.~%"))
  (iter (for action in *actions*)
        (setf (action-precondition-instantiations action)  ;previous setting, nil or restriction
              (or (type-instantiations (action-precondition-types action)
                                       (action-precondition-instantiations action)
                                       *start-state*)
                  '(nil))))
   (when (fboundp 'heuristic?)
    (setf (problem-state-heuristic *start-state*) (funcall 'heuristic? *start-state*)))
  (display-parameter-settings))


(defun display-parameter-settings ()
  (format t "~%Current parameter settings:")
  (ut::prt (ww-get 'problem) (ww-get 'tree-or-graph) (ww-get 'solution-type)
           (ww-get 'depth-cutoff) (ww-get 'progress-reporting-interval) 
           *num-parallel-threads* *debug*)
  (when (fboundp 'heuristic?) (format t "~&  HEURISTIC? YES"))
  (when (fboundp 'get-best-relaxed-value?) (format t "~&  GET-BEST-RELAXED-VALUE? YES"))
  (terpri) (terpri))
  

(init)
