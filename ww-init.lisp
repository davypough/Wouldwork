;;; Filename: ww-init.lisp

;;; Initializes a lisp image, before installing the Wouldwork System.

(in-package :cl-user)


#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(setq *default-pathname-defaults*
     #p"D:/Users Data/Dave/SW Library/AI/Planning/Wouldwork Planner/")
     ;#p"D:/Users Data/Dave/SW Library/AI/Planning/Wouldwork Planner/temp-21apr/")
     ;#p"D:/Users Data/Dave/SW Library/AI/Planning/Wouldwork Planner/archive/200-add combinations/")
      

(asdf:initialize-source-registry
  `(:source-registry
     (:directory ,*default-pathname-defaults*)
     (:directory #p"D:/Users Data/Dave/SW Library/AI/Lisp/Common Lisp Libraries/Lisp Critic/")
     :inherit-configuration))


;#+:sbcl
;(setq sb-ext:*debug-print-variable-alist*
;  '((*print-length* . 70) (*print-level* . 6)))

(setq *compile-verbose* nil)

(setq *compile-print* nil)


(ql:quickload :alexandria)
(ql:quickload :iterate)
(ql:quickload :lparallel)

;(ql:quickload :cl-cuda)


;(require :sb-cltl2)  ;enable access to declarations in .asd file


(defpackage :utilities-pkg
  (:use :cl)
  (:nicknames :ut))

(defpackage :hstack-pkg
  (:use :cl)
  (:nicknames :hs))

(defpackage :wouldwork-pkg
  (:use :cl :iterate :sb-ext)
  (:shadowing-import-from :iterate)
  (:nicknames :ww))


(setq sb-ext:*debug-print-variable-alist*
      '((*print-length* . 30) (*print-level* . 6) (*print-pretty* . T)))


;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;


(in-package :ww)


(defparameter *problem-files*
  '(
    "problem-blocks3.lisp" "problem-blocks4.lisp" "problem-boxes.lisp"
    "problem-jugs2.lisp" "problem-jugs4.lisp" "problem-queens4.lisp"
    "problem-queens8.lisp" "problem-captjohn-csp.lisp" "problem-quern.lisp" 
    "problem-graveyard.lisp" "problem-sentry.lisp" "problem-crossword5-11.lisp"
    "problem-array-path.lisp" "problem-tiles1a-heuristic.lisp" "problem-tiles7a-heuristic.lisp"
    "problem-triangle-xy.lisp" "problem-triangle-xyz.lisp" "problem-triangle-heuristic.lisp"
    "problem-triangle-macros.lisp" "problem-triangle-macros-one.lisp"
    "problem-tsp.lisp" "problem-u2.lisp" "problem-donald.lisp"
    "problem-knap4a.lisp" "problem-knap4b.lisp" "problem-knap19.lisp"
    "problem-smallspace.lisp" "problem-crater.lisp"))


(declaim (ftype (function () t) solve))  ;function solve located in searcher.lisp


(defun test ()
  (when (probe-file "problem.lisp")
    (error "Rename problem.lisp before testing."))
  (loop for filename in *problem-files*
        if (y-or-n-p "Proceed with testing ~A?: " filename)
          do (rename-file filename "problem.lisp")
             (handler-bind ((sb-sys:interactive-interrupt
                             (lambda (c)
                               (declare (ignore c))
                               (throw 'my-loop nil))))
               (catch 'my-loop
                 (labels ((my-loop ()
                            (with-open-stream (*standard-output* (make-broadcast-stream))
                              (asdf:load-system :ww-wouldwork-planner :force t))
                            (solve)))
                   (my-loop))))
             (rename-file "problem.lisp" filename)
        else do (return)))


(format t "~A~2%" *default-pathname-defaults*)
