;;;; Filename: problem-blocks4.lisp


;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, C, and D on a table named T.


(in-package :ww)  ;required

(ww-set *problem* blocks4)

(ww-set *solution-type* min-length)


(define-types
    block (A B C D)
    table (T)
    support (either block table))


(define-dynamic-relations
    (on block support))


(define-static-relations
    (height support $real))




(define-query cleartop? (?block)
  (not (exists (?b block)
         (on ?b ?block))))


(define-action put
    1
  (?block block (?block-support ?support) support)
  (and (cleartop? ?block)
       (on ?block ?block-support)
       (cleartop? ?support)
       (different ?block ?support))
  (?block block ?support support)
  (assert (on ?block ?support)
          (not (on ?block ?block-support))))


(define-init
  (on A T)
  (on B T)
  (on C T)
  (on D T))


(define-goal
  (and (on D T)
       (on C D)
       (on B C)
       (on A B)))
