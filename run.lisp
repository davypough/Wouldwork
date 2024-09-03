"
Usage:

rlwrap sbcl --load run.lisp

This loads the project's asd, loads the quicklisp dependencies, and
calls the main function.

Then, we are given the lisp prompt.

If you don't want to land in the REPL, you can (quit) below or call lisp with the --non-interactive flag.

Another solution to run the app is to build and run a binary (see README).
"


;;; Initialize a lisp image before inistalling the Wouldwork system

;;; Quicklisp is required and assumed to exist in the system

(in-package :cl-user)

(load "wouldwork.asd")

(ql:quickload "wouldwork")

(in-package :wouldwork)

(handler-case
    (main)
  (error (c)
    (format *error-output* "~&An error occured: ~a~&" c)
    (uiop:quit 1)))
