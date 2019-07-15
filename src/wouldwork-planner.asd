;;; Filename: wouldwork-planner.asd


(asdf:defsystem "wouldwork-planner"
; :depends-on ("generic-utilities")
  :around-compile
    (lambda (next)
      (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
      (funcall next))
  :components
   ((:file "utilities") ;loads only local planner utilities
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
    (:file "problem" :depends-on ("searcher"))
   ))

