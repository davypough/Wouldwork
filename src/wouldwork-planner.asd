;;; Filename: wouldwork-planner.asd


(asdf:defsystem "wouldwork-planner"
  :components
    ((:file "utilities")
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
     (:file "set"             :depends-on ("settings" "support"))
     (:file "searcher"        :depends-on ("hstack" "settings" "structures" "support"))
     (:file "successors"      :depends-on ("settings" "structures" "searcher"))
     (:file "parallel"        :depends-on ("hstack" "settings" "structures" "searcher"))
     (:file "problem"         :depends-on ("settings" "installer" "set"))
     (:file "initialize"      :depends-on ("set" "problem"))
))
