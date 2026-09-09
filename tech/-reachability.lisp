;;; Filename: -reachability.lisp

;;; Reachability substrate: the baseline meaning of physical reach for manipulation
;;; actions.  Objects at the same location are always mutually reachable.  The public
;;; reachability technology overrides this query to add authored reach-via edges.
;;;
;;; Nested-only; included by technologies that call reachable.
;;;
;;; REQUIRES:
;;;   type  : location; switch is optional
;;; PROVIDES:
;;;   query : reachable  --  identity default, overridden by reachability

(in-package :ww)


(define-optional-types switch)


(define-types
  reach-target (either location switch))


(define-query reachable (?target reach-target ?reacher location)
  (eql ?target ?reacher))
