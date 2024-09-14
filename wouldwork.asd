(in-package :asdf-user)

(defsystem "wouldwork"
  :author ("Dave Brown <davypough@gmail.com>"
           "Gwang-Jin Kim <gwang.jin.kim.phd@gmail.com>")
  :version "0.0.1"
  :license "MIT"
  :description "classical planning with the wouldwork planner"
  :homepage "https://github.com/davypough/Wouldwork"
  :bug-tracker "https://github.com/davypough/Wouldwork/issues"
  :source-control (:git "https://github.com/davypough/Wouldwork.git")

  ;; Dependencies.
  :depends-on (:alexandria :iterate :lparallel)

  ;; Project stucture.
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "wouldwork")
				     (:file "ww-utilities")
				     (:file "ww-hstack")
				     (:file "ww-settings")
				     (:file "ww-structures")
				     (:file "ww-converter")
				     (:file "ww-validator")
				     (:file "ww-frequencies")
				     (:file "ww-support")
				     (:file "ww-happenings")
				     (:file "ww-translator")
				     (:file "ww-installer")
				     (:file "ww-set")
				     (:file "problem")
				     (:file "ww-searcher")
				     (:file "ww-planner")
				     (:file "ww-parallel")
				     (:file "ww-initialize"))))

  ;; Build a binary:
  ;; don't change this line.
  :build-operation "program-op"
  ;; binary name: adapt.
  :build-pathname "wouldwork"
  ;; entry point: here "main" is an exported symbol. Otherwise, use a double ::
  :entry-point "wouldwork:main")
