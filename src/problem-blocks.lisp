;;;; Filename: problem-blocks.lisp


;;; Problem specification for a blocks world problem:
;;; stack blocks named A, B, and C, on a table named T.


(in-package :ww)  ;required


(define-types
    block (A B C)
    table (T)
    support (either block table))


(define-dynamic-relations
    (on block support))


(define-static-relations
    (height support $real))




(define-query cleartop! (?block)
  (not (exists (?b block)
         (on ?b ?block))))


(define-action put
    1
  (?block block ?support support)
  (and (cleartop! ?block)
       (cleartop! ?support)
       (different ?block ?support))
  (?block block ?support support)
  (do (assert (on ?block ?support))
      (if (bind (on ?block $s))
        (assert (not (on ?block $s))))))


(define-init
  (on A T)
  (on B T)
  (on C T))


(define-goal
  (and (on C T)
       (on B C)
       (on A B)))
