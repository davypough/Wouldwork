;;; Filename: initialize.lisp

;;; Initialization after installation.

(in-package :ww)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for qname in *query-names*
        do (proclaim `(ftype function ,qname)))
  (loop for uname in *update-names*
        do (proclaim `(ftype function ,uname))))


(defun init ()
  (format t "~&Initializing...~%")
  (setf *query-names* (nreverse *query-names*))
  (setf *update-names* (nreverse *update-names*))
  (setf *init-actions* (nreverse *init-actions*))
  (init-start-state)  ;finish start-state init later in converter.lisp
; (when (> (length *happenings*) 1)
  (setq *happenings* (sort *happenings* #'< :key (lambda (object)
                                                   (first (aref (get object :events) 0)))))
  (format t "Converting propositions to integers...~%")
  (do-integer-conversion)  ;allows integer hashtable db lookups, adds start-state idb
  (convert-ilambdas-to-fns) ;and compile
  (cond (*actions*
           (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
           (setf *min-action-duration* (reduce #'min *actions* :key #'action.duration)))
        (t (format t "~%NOTE: There are no defined actions.~%")))
  (when (fboundp 'heuristic?)
    (setf (problem-state.heuristic *start-state*) (funcall 'heuristic? *start-state*))
    (when *randomize-search*
      (format t "~%NOTE: Defining a heuristic? search function is incompatible with randomize-search setting.")
      (format t "~%Ignoring randomization.~%")))
  (when (fboundp 'bounding-function?)
    (multiple-value-setq (*cost* *upper*)
                         (funcall (symbol-function 'bounding-function?) *start-state*)))
  (when (and *happenings* (eql *tree-or-graph* 'graph))
    (format t "~%ERROR: Graph search is incompatible with exogenous happenings, since states cannot be closed.~%"))
  (iter (for (key value) in-hashtable *db*)
        (when (and (listp value) (listp (first value)))
          (format t "~%CAUTION: One or more arguments in a dynamic relation is a list: ~A~%"
                    (cons (first key) (gethash (first key) *relations*)))
          (format t "~&If this is intended as a set, recommend changing to a hash-table.~%")
          (return)))
  (display-parameter-settings))


(defun init-start-state ()
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
      (setf name 'start)))
  (do-init-action-updates *start-state*))  ;updates start-state db & static-db, but not idb & hidb yet


(defun convert-ilambdas-to-fns ()
  (format t "~&Converting lambda expressions to functions...~%")
  (iter (for fname in (append *query-names* *update-names*))
        (format t "~&~A...~%" fname)
        (when (fboundp fname)
          (fmakunbound fname))
        (setf (symbol-function fname) (compile nil (symbol-value fname))))
  (when *constraint*
    (format t "~&~A...~%" '*constraint*)
    (setf (symbol-function '*constraint*)
      (compile nil *constraint*)))
  (iter (for object in *happenings*)
        (format t "~&~A...~%" object)
        (setf (get object :interrupt-fn) (compile nil (symbol-value object))))
  (format t "~&~A...~%" '*goal*)
  (setf (symbol-function '*goal*)
    (compile nil *goal*))
  (dolist (action *actions*)
    (format t "~&~A...~%" (action.name action))
    (with-slots (iprecondition-lambda iprecondition ieffect-lambda ieffect) action
      (setf iprecondition (compile nil iprecondition-lambda))
      (setf ieffect (compile nil ieffect-lambda)))))


(defun display-parameter-settings ()
  (format t "~%Current parameter settings:")
  (ut::prt *problem* *tree-or-graph* *solution-type*
           *depth-cutoff* *progress-reporting-interval*
           *threads* *randomize-search* *debug* *probe*)
  (format t "~&  HEURISTIC? => ~A" (when (fboundp 'heuristic?) 'YES))
  (format t "~&  EXOGENOUS HAPPENINGS => ~A" (when *happenings* 'YES))
  (format t "~&  BOUNDING FUNCTION => ~A" (when (fboundp 'bounding-function?) 'YES))
  (terpri) (terpri))
  

(init)
