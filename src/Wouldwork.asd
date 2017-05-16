;;;; Filename: Wouldwork.asd

;Enable for debugging.
;(proclaim '(optimize (safety 3) (space 0) (speed 0) (debug 3) (compilation-speed 0)))


(ql:quickload :alexandria)
(ql:quickload :iterate)


(defpackage :utilities-pkg
  (:use :cl)
  (:nicknames :ut))


(defpackage :planner-pkg
  (:use :cl :iterate)
  (:nicknames :pl))


(defpackage :branch&bound-pkg
  (:use :cl)
  (:nicknames :bnb))


(setq *compile-verbose* nil)


(asdf:defsystem "Wouldwork"
  :depends-on ("alexandria" "iterate")
  :components
   ((:file "utilities")
    (:file "hash_stack")
    (:file "setup" :depends-on ("utilities"))
    (:file "support" :depends-on ("setup"))
    (:file "translator" :depends-on ("support"))
    (:file "happenings" :depends-on ("support"))
    (:file "installer" :depends-on ("translator"))
    (:file "planner" :depends-on ("installer" "happenings"))
    (:file "bnb" :depends-on ("hash_stack" "planner"))
    (:file "problem" :depends-on ("translator" "bnb"))
   ))
