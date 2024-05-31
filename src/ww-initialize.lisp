;;; Filename: ww-initialize.lisp

;;; Initialization after installation.

(in-package :ww)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for qname in *query-names*
        do (proclaim `(ftype function ,qname)))
  (loop for uname in *update-names*
        do (proclaim `(ftype function ,uname))))


(defun init ()
  (format t "~&Initializing...")
  (setf *query-names* (nreverse *query-names*))
  (setf *update-names* (nreverse *update-names*))
  (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
  (setf *init-actions* (nreverse *init-actions*))
  (init-start-state)  ;finish start-state init later in converter.lisp
  (setq *happenings* (sort (copy-list *happenings*) #'< :key (lambda (object)
                                                   (first (aref (get object :events) 0)))))
  (format t "~&Converting propositions to integers...")
  (do-integer-conversion)  ;allows integer hashtable db lookups, adds start-state idb
  (convert-ilambdas-to-fns) ;and compile
  (if *actions*
    (setf *min-action-duration* (reduce #'min *actions* :key #'action.duration))
    (format t "~%NOTE: There are no defined actions.~%"))
  (when (fboundp 'heuristic?)
    (format t "~&Applying heuristic function to start state...")
    (setf (problem-state.heuristic *start-state*) (funcall 'heuristic? *start-state*))
    (format t "done~%")
    (when *randomize-search*
      (format t "~%NOTE: Defining a heuristic? search function is incompatible with randomize-search setting.")
      (format t "~%Ignoring randomization.~%")))
  (when (fboundp 'bounding-function?)
    (format t "~&Applying bounding function to start state...")
    (multiple-value-setq (*cost* *upper*)
                         (funcall (symbol-function 'bounding-function?) *start-state*))
    (format t "done~%"))
  (when (and *happenings* (eql *tree-or-graph* 'graph))
    (format t "~%ERROR: Graph search is incompatible with exogenous happenings, since states cannot be closed.~%"))
  (iter (for (key value) in-hashtable *db*)
        (when (and (listp value) (or (listp (first value)) (vectorp (first value))))
          (format t "~%CAUTION: One or more $ arguments in a dynamic relation is a list or vector: ~A"
                  (cons (first key) (gethash (first key) *relations*)))
          (format t "~&Two lists or vectors are the same only if their elements occur in exactly the same order.")
          (format t "~&Make sure the list or vector elements are always ordered canonically (eg, lexicographically),")
          (format t "~&so that Wouldwork can tell if two states are the same or not.~%")
         (return)))
  (when (and (eql *tree-or-graph* 'graph) (not (fixedp *relations*)))
    (format t "~%NOTE: In graph search, you could significantly improve search efficiency by")
    (format t "~&changing each dynamic relation to include at least one fluent ($) variable.~%"))
  (when (and (> *threads* 0) (> *debug* 1))
    (setf *debug* 1)
    (format t "~%ADVISORY: Currently set to run parallel threads. Resetting *debug* to 1.~%"))
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
  (format t "~&Converting lambda expressions to functions...")
  (iter (for fname in (append *query-names* *update-names*))
        (format t "~&  ~A..." fname)
        (when (fboundp fname)
          (fmakunbound fname))
        (finish-output nil)
        (setf (symbol-function fname) (compile nil (symbol-value fname)))
        (format t "done~%"))
  (format t "~&Compiling lambda functions...")
  (when *constraint*
    (format t "~&  ~A..." '*constraint*)
    (setf (symbol-function '*constraint*)
      (compile nil *constraint*))
    (format t "done~%"))
  (iter (for object in *happenings*)
        (format t "~&  ~A..." object)
        (setf (get object :interrupt-fn) (compile nil (symbol-value object)))
        (format t "done~%"))
  (format t "~&  ~A..." '*goal*)
  (setf (symbol-function '*goal*)
    (compile nil *goal*))
  (format t "done~%")
  (dolist (action *actions*)
    (format t "~&  ~A..." (action.name action))
    (with-slots (iprecondition-lambda iprecondition ieffect-lambda ieffect) action
      (setf iprecondition (compile nil iprecondition-lambda))
      (setf ieffect (compile nil ieffect-lambda)))
    (format t "done~%"))
  (format t "~&  done compiling~%"))


(defun display-parameter-settings ()
  (format t "~%Current parameter settings:")
  (ut::prt *problem* *tree-or-graph* *solution-type*
           *depth-cutoff* *progress-reporting-interval*
           *threads* *randomize-search* *debug* *probe*)
  (format t "~&  HEURISTIC? => ~A" (when (fboundp 'heuristic?) 'YES))
  (format t "~&  EXOGENOUS HAPPENINGS => ~A" (when *happenings* 'YES))
  (format t "~&  BOUNDING-FUNCTION? => ~A" (when (fboundp 'bounding-function?) 'YES))
  (terpri) (terpri))
  

(init)
