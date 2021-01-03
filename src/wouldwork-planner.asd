;;; Filename: wouldwork-planner.asd


(asdf:defsystem "wouldwork-planner"
  :around-compile
    (lambda (next)
      (proclaim '(optimize (debug 3)))  ; (safety 3) (speed 0)))
      (funcall next))
  :components
   ((:file "utilities") ;load only local planner utilities
    (:file "hstack")
    (:file "settings")
    (:file "structures"      :depends-on ("settings"))
    (:file "type-specifiers" :depends-on ("settings"))
    (:file "converter"       :depends-on ("settings"))
    (:file "frequencies"     :depends-on ("utilities" "settings" "structures"))
    (:file "support"         :depends-on ("utilities" "settings" "structures" "converter"))
    (:file "happenings"      :depends-on ("utilities" "settings" "structures" "support"))
    (:file "translator"      :depends-on ("utilities" "settings" "structures" "type-specifiers" "support"))
    (:file "installer"       :depends-on ("utilities" "settings" "structures" "support" "translator"))
    (:file "planner"         :depends-on ("settings" "structures" "support" "happenings"))
    (:file "searcher"        :depends-on ("hstack" "settings" "structures" "support"))
    (:file "parallel"        :depends-on ("hstack" "settings" "structures" "searcher"))
    (:file "problem"         :depends-on ("settings" "installer"))
    (:file "initialize"      :depends-on ("settings" "structures" "converter" "support" "problem"))
   ))
