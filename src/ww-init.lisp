;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp\\setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(asdf:initialize-source-registry
  '(:source-registry
     (:tree "D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\")
     :inherit-configuration))


(ql:quickload :alexandria)

(ql:quickload :iterate)

(ql:quickload :lparallel)


;; To instruct the compiler globally to produce the fastest but least safe and least
;; debuggable compiled code, evaluate the following before compiling:
;; 
;;    (proclaim '(optimize (speed 3) (safety 1) (space 0) (debug 0)))
;; 
;; It is recommended that a combination of (speed 3) and (safety 0)
;; never be used globally.
;; 
;; For the most debuggable (and yet reasonably fast) code, use
;; 
;;    (proclaim '(optimize (speed 2) (safety 1) (space 1) (debug 3)))
