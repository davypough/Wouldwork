
(load "wouldwork.asd")
(load "wouldwork-tests.asd")

(ql:quickload "wouldwork-tests")

(in-package :wouldwork-tests)

(uiop:quit (if (run-all-tests) 0 1))
