;;; Filename: wouldwork planner.asd


;(proclaim '(optimize (safety 3) (space 0) (speed 0) (debug 3)
;                     (compilation-speed 0)))

;(proclaim '(optimize (safety 1) (space 0) (speed 3) (debug 0)
;                     (compilation-speed 0)))


(ql:quickload :alexandria)


(ql:quickload :iterate)


(defpackage :utilities-pkg
  (:use :cl)
  (:nicknames :ut))


(defpackage :wouldwork-pkg
  (:use :cl :iterate)
  (:nicknames :ww))


(defpackage :branch&bound-pkg
  (:use :cl)
  (:nicknames :bnb))


(setq *compile-verbose* nil)


(asdf:defsystem "wouldwork planner"
; :depends-on ("utilities")   ;loads all generic utilities
  :components
   ((:file "hash-stack")
    (:file "utilities")   ;loads only local planner utilities
    (:file "setup")
    (:file "support" :depends-on ("setup"))
    (:file "translator" :depends-on ("support"))
    (:file "happenings" :depends-on ("support"))
    (:file "installer" :depends-on ("translator"))
    (:file "backchain" :depends-on ("installer"))
    (:file "converter" :depends-on ("support"))
    (:file "planner" :depends-on ("installer" "happenings"))
    (:file "bnb-planner" :depends-on ("hash-stack" "planner"))
    (:file "problem" :depends-on ("translator" "bnb-planner"))
   ))
