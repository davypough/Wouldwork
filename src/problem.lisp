;;;; Filename: blocks-problem.lisp

;;; Problem specification for a blocks world problem for stacking 3 blocks
;;; named A, B, and C, on a table named T. See user manual.


(in-package :pl)  ;required


(setq *depth_cutoff* 2)  ;max # of steps to goal


(define-types
    block (A B C)
    table (T)
    support (either block table))


(define-base-relations
    (on block support)
    (height support !real))


(define-action put
    0
    (?block block ?support support)
    (and (not (exists (?b block)
                      (on ?b ?block)))
         (or (table ?support)
             (and (block ?support)
                  (not (exists (?b block)
                         (on ?b ?support))))))
   (?block block ?support support)
   (and (on ?block ?support)
        (exists (?s support)
          (if (on ?block ?s))
            (not (on ?block ?s)))))


(define-init
  (on A T)
  (on B T)
  (on C T))


(define-goal
  (and (on C T)
       (on B C)
       (on A B)))
