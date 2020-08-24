(setq *default-pathname-defaults*
        #p"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\")
        ;#p"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\archive\\212-update triangle-new coords\\")

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp\\setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:initialize-source-registry
  `(:source-registry
     (:tree ,*default-pathname-defaults*)
     :inherit-configuration))

(ql:quickload :alexandria)
(ql:quickload :iterate)
(ql:quickload :lparallel)

#+:sbcl (setq sb-ext:*debug-print-variable-alist*
          '((*print-length* . 70) (*print-level* . 6) (*print-pretty* . t)))

(setq *compile-verbose* nil)

(defpackage :utilities-pkg
  (:use :cl)
  (:nicknames :ut))

(defpackage :hstack-pkg
  (:use :cl)
  (:nicknames :hs))

(defpackage :wouldwork-pkg
  (:use :cl :iterate)
  (:shadowing-import-from :iterate)   ; :sum)
  (:nicknames :ww))

;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;

(in-package :ww)

(defparameter *problem-files*
  '("problem-blocks.lisp" "problem-boxes.lisp" "problem-4jugs.lisp" "problem-quern.lisp"
    "problem-8queens.lisp" "problem-captjohn.lisp" "problem-crater.lisp" "problem-graveyard.lisp"
    "problem-hanoi.lisp" "problem-knap4.lisp" "problem-sentry.lisp" "problem-smallspace.lisp"
    "problem-triangle.lisp" "problem-u2.lisp" "problem-socrates1.lisp" "problem-socrates2.lisp"))

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