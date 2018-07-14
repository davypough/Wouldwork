;;;; Filename: problem-blocks.lisp

;;; Problem specification for a blocks world problem for stacking 3 blocks
;;; named A, B, and C, on a table named T.  See Wouldwork Planner Manual.


(in-package :ww)  ;required


(define-types
    block (A B C)
    table (T)
    support (either block table))


(define-dynamic-relations
    (on block support))

(define-static-relations
    (height support $real))


(define-derived-relations
  (cleartop* ?block)  (not (exists (?b block)
                             (on ?b ?block))))


(define-action put
    1
    (?block block ?support support)
    (and (cleartop* ?block)
         (or (table ?support)
             (and (block ?support)
                  (cleartop* ?support))))
   (?block block ?support support)
   (and (on ?block ?support)
        (exists (?s support)
          (if (on ?block ?s)
            (not (on ?block ?s))))))


(define-init
  (on A T)
  (on B T)
  (on C T))


(define-goal
  (and (on C T)
       (on B C)
       (on A B)))
