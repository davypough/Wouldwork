;;; Filename: wouldwork-planner.asd


(asdf:defsystem "ww-wouldwork-planner"
  :serial t
;  :around-compile (lambda (next)
;                    (proclaim '(optimize (debug 3)))
;                    (funcall next))
  :components
    ((:file "ww-utilities")
     (:file "ww-hstack")
     (:file "ww-settings")
     (:file "ww-structures"      :depends-on ("ww-settings"))
     (:file "ww-type-specifiers" :depends-on ("ww-settings"))
     (:file "ww-converter"       :depends-on ("ww-settings"))
     (:file "ww-frequencies"     :depends-on ("ww-utilities" "ww-settings" "ww-structures"))
     (:file "ww-support"         :depends-on ("ww-utilities" "ww-settings" "ww-structures" "ww-converter"))
     (:file "ww-happenings"      :depends-on ("ww-utilities" "ww-settings" "ww-structures" "ww-support"))
     (:file "ww-translator"      :depends-on ("ww-utilities" "ww-settings" "ww-structures" "ww-type-specifiers" "ww-support"))
     (:file "ww-installer"       :depends-on ("ww-utilities" "ww-settings" "ww-structures" "ww-support" "ww-translator"))
     (:file "ww-planner"         :depends-on ("ww-settings" "ww-structures" "ww-support" "ww-happenings"))
     (:file "ww-set"             :depends-on ("ww-settings" "ww-support"))
     (:file "ww-searcher"        :depends-on ("ww-hstack" "ww-settings" "ww-structures" "ww-support"))
     (:file "ww-successors"      :depends-on ("ww-settings" "ww-structures" "ww-searcher"))
     (:file "ww-parallel"        :depends-on ("ww-hstack" "ww-settings" "ww-structures" "ww-searcher"))
     (:file "problem"            :depends-on ("ww-settings" "ww-installer" "ww-set"))
     (:file "ww-initialize"      :depends-on ("ww-set" "problem"))
))
