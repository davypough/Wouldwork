(in-package :asdf-user)
(defsystem "wouldwork-tests"
  :description "Test suite for the wouldwork system"
  :author "Dave Brown <davypough@gmail.com>"
  :version "0.0.1"
  :depends-on (:wouldwork
               :fiveam)
  :license "BSD"
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test-wouldwork"))))

  ;; The following would not return the right exit code on error, but still 0.
  ;; :perform (test-op (op _) (symbol-call :fiveam :run-all-tests))
  )
