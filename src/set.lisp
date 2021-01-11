;;; Filename: set.lisp

;;; User interface for resetting Wouldwork's search control parameters.

(in-package :ww)


(defmacro ww-set (param val)
  ;Allows resetting of user parameters after loading.
  (declare (optimize (debug 3)))
  (case param
    ((*problem* *depth-cutoff* *max-states* *progress-reporting-interval* *randomize-search*)
       `(setq ,param ',val))
    (*solution-type* 
       `(progn (setq *solution-type* ',val)
               (iter (for fn-name in '(process-successor-graph process-successor-tree detect-goals))
                 (setf (symbol-function fn-name)
                   (compile nil (function-lambda-expression (symbol-function fn-name)))))
               ',val))
    (*debug*
       `(progn (setq *debug* ',val)
               (iter (for fn-name in '(df-bnb1 process-successor-graph process-successor-tree
                                       generate-new-node close-barren-nodes pop-discontinued-node
                                       register-solution amend-happenings generate-children commit1
                                       get-new-states update-search-tree process-followup-updates))
                 (setf (symbol-function fn-name)
                   (compile nil (function-lambda-expression (symbol-function fn-name)))))
               ',val))
    (*tree-or-graph*
       `(progn (setq *tree-or-graph* ',val)
               (setf (symbol-function 'process-nongoal-succ-states)
                 (compile nil (function-lambda-expression #'process-nongoal-succ-states)))
               ',val))
    (*probe*
       `(progn (setq *probe* ',val)
               (ww-set *debug* 0)
               (setq *counter* 1)
               ',val))
    (*num-parallel-threads*
       `(progn (format t "~%*num-parallel-threads* cannot be changed with ww-set.")
               (format t "~%Instead, set its value in the file settings.lisp, and then recompile.~2%")))
    (otherwise
       (format t "~%~A is not a valid parameter name in ww-set.~%" param))))
