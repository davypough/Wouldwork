;;;; Filename: problem-blocks3.lisp


;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, and C on a table named T.


(in-package :ww)  ;required


(ww-set *problem* blocks3)

(ww-set *solution-type* every)

(ww-set *tree-or-graph* tree)


(define-types
    block (A B C)
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
  (on C T))


(define-goal
  (and (on C T)
       (on B C)
       (on A B)))
