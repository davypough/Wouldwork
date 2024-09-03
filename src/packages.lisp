
(ql:quickload :alexandria)
(ql:quickload :iterate)
(ql:quickload :lparallel)

(defpackage :utilities
  (:use :cl)
  (:nicknames :ut))

(defpackage :hstack
  (:use :cl)
  (:nicknames :hs))

(defpackage :wouldwork
  (:use :cl :iterate :sb-ext)
  (:nicknames :ww)
  (:shadowing-import-from :iterate)
  (:export #:main
           #:help
           #:run-test-problems
           #:run-all
           #:list-all
           #:run
           #:*problem-names*
           #:*problem-folder-paths*
	   #:get-src-folder-path
	   #:add-problem-folder
           #:remove-problem-folder))
