;;; Filename: wouldwork-planner.asd

;max debugging
(proclaim '(optimize (safety 3) (space 0) (speed 0) (debug 3)))

;max speed
;(proclaim '(optimize (safety 1) (space 0) (speed 3) (debug 0)))


#+:sbcl (setq sb-ext:*debug-print-variable-alist* '((*print-length* . 70) (*print-level* . 6) (*print-pretty* . t)))
(setq *compile-verbose* nil)


;(ql:quickload :alexandria)
;(ql:quickload :iterate)
;(ql:quickload :lparallel)
;(ql:quickload :stmx)


(defpackage :utilities-pkg
  (:use :cl)
  (:nicknames :ut))


(defpackage :hstack-pkg
  (:use :cl)
  (:nicknames :hs))


(defpackage :wouldwork-pkg
  (:use :cl :iterate)
  (:shadowing-import-from :iterate :sum)
  (:nicknames :ww))


(defpackage :branch&bound-pkg
  (:use :cl :iterate)
  (:nicknames :bnb))



(asdf:defsystem "wouldwork-planner"
; :depends-on ("utilities")   ;loads all generic utilities
  :components
   ((:file "utilities")   ;loads only local planner utilities
    (:file "hstack")
    (:file "setup")
    (:file "support" :depends-on ("setup"))
    (:file "translator" :depends-on ("support"))
    (:file "happenings" :depends-on ("support"))
    (:file "installer" :depends-on ("translator"))
    (:file "backchain" :depends-on ("installer"))
    (:file "converter" :depends-on ("support"))
    (:file "planner" :depends-on ("installer" "happenings"))
    (:file "searcher" :depends-on ("hstack" "planner"))
    (:file "problem" :depends-on ("translator" "searcher"))
   ))

