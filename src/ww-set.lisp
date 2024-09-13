;;; Filename: ww-set.lisp

;;; User interface for resetting Wouldwork's search control parameters.

(in-package :ww)


;(defun function-lambda-expression@ (fn)
;  "Wrapper for function-lambda-expression which compiles it with (debug 3)."
;  (let ((incoming-debug (assoc 'debug (sb-cltl2:declaration-information 'optimize))))
;    (proclaim '(optimize (debug 3)))
;    (let ((lambda-expression (function-lambda-expression fn)))
;      (proclaim `(optimize ,incoming-debug))
;      lambda-expression)))


(defmacro ww-set (param val)
  ;Allows resetting of user parameters after loading.
  (case param
    ((*problem* *problem-type* *depth-cutoff* *tree-or-graph* *solution-type* *progress-reporting-interval* *randomize-search* *branch*)
     `(setq ,param ',val))
    (*debug*
     `(progn (setq *debug* ',val)
	     (if (or (> *debug* 0) *probe*)
                 (pushnew :ww-debug *features*)  ;allows inserting debug code
                 (setf *features* (delete :ww-debug *features*)))
	     (with-silenced-compilation (asdf:load-system :wouldwork :force t) (in-package :ww))
	     ',val))
    (*probe*
     `(progn (setq *probe* ',val)
	     (setf *debug* 0)
	     (setq *counter* 1)
	     ',val))
    (*threads*
     `(progn (format t "~%*threads* cannot be changed with ww-set.")
	     (format t "~%Instead, set its value in the file settings.lisp, and then exit and restart SBCL.~2%")))
    (otherwise
     (format t "~%~A is not a valid parameter name in ww-set.~%" param))))
