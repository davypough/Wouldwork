;;;; Filename: problem-4queens.lisp

;;; Problem specification for 4-queens.


(in-package :ww)  ;required


(setq *depth-cutoff* 4)


(setq *tree-or-graph* 'tree)


(define-types
    queen   (queen1 queen2 queen3 queen4)
    column  (1 2 3 4))


(define-dynamic-relations
  (loc queen $integer column)
  (placed queen)
  (next-row $integer))


(define-action put
    1
  (?queen queen $row fluent ?column column)
  (and (not (placed ?queen))
       (next-row $row)
       (not (exists (?q queen $r fluent ?c column)
              (and (placed ?q)
                   (loc ?q $r ?c)
                   (or (= $r $row)
                       (= ?c ?column)
                       (= (- $r $row) (- ?c ?column))
                       (= (- $r $row) (- ?column ?c)))))))
  (?queen queen $row fluent ?column column)
  (assert (loc ?queen $row ?column)
          (placed ?queen)
          (not (next-row $row))
          (next-row (1+ $row))))
   

(define-init
    (next-row 1))


(define-goal
  (next-row 5))
