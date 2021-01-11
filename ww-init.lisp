;;; Filename: ww-init.lisp

;;; Initializes a lisp image, before installing the Wouldwork System.

(in-package :cl-user)


(setq *default-pathname-defaults*
      #p"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\")


#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp\\setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


;(asdf:initialize-source-registry
;  `(:source-registry
;     (:tree "D:\\Users Data\\Dave\\SW Library\\AI\\")  ;,*default-pathname-defaults*)
;     :inherit-configuration))


(pushnew #p"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\" asdf:*central-registry* :test #'equal)
(pushnew #p"D:\\Users Data\\Dave\\SW Library\\AI\\Lisp\\Common Lisp Libraries\\Lisp Critic\\" asdf:*central-registry* :test #'equal)


#+:sbcl
(setq sb-ext:*debug-print-variable-alist*
  '((*print-length* . 70) (*print-level* . 6)))
(setq *compile-verbose* nil)
(setq *compile-print* nil)


(ql:quickload :alexandria)
(ql:quickload :iterate)
(ql:quickload :lparallel)


;(asdf:load-system :lisp-critic)
;(defpackage :lisp-critic
;  (:nicknames :lc))
;(use-package :lisp-critic)


(defpackage :utilities-pkg
  (:use :cl)
  (:nicknames :ut))

(defpackage :hstack-pkg
  (:use :cl)
  (:nicknames :hs))

;(when (find-package :wouldwork-pkg)
;  (in-package :cl-user)
;  (delete-package :wouldwork-pkg))

(defpackage :wouldwork-pkg
  (:use :cl :iterate)
  (:shadowing-import-from :iterate)
  (:nicknames :ww))


;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;

(in-package :ww)


(defparameter *problem-files*
  '("problem-blocks3.lisp" "problem-blocks4.lisp" "problem-boxes.lisp" "problem-jugs2.lisp" "problem-jugs4.lisp"
    "problem-queens8.lisp" "problem-captjohn.lisp" "problem-crater.lisp" "problem-graveyard.lisp"
    "problem-knap19.lisp" "problem-sentry.lisp" "problem-smallspace.lisp"
    "problem-triangle.lisp" "problem-triangle-xyz.lisp" "problem-triangle-heuristic.lisp"
    "problem-triangle-macros-one.lisp" "problem-tsp.lisp" "problem-u2.lisp"))


(declaim (ftype (function () t) solve))


(defun test ()
  (when (probe-file "problem.lisp")
    (error "Rename problem.lisp before testing."))
  (loop for filename in *problem-files*
        when (y-or-n-p "Proceed with testing ~A?:" filename)
          do (rename-file filename "problem.lisp")
             (with-open-stream (*standard-output* (make-broadcast-stream)) ;ignore *standard-output*
               (asdf:load-system :wouldwork-planner :force t))
             (solve)
        (rename-file "problem.lisp" filename)))
