;;;; Filename: problem-8queens.lisp

;;; Problem specification for 8-queens.


(in-package :ww)  ;required


(setq *depth-cutoff* 8)

(setq *tree-or-graph* 'tree)

(setq *first-solution-sufficient* t)


(define-types
    queen   (queen1 queen2 queen3 queen4 queen5 queen6 queen7 queen8)
    column  (1 2 3 4 5 6 7 8))


(define-dynamic-relations
  (loc queen $fixnum column)
  (placed queen)
  (next-row $fixnum))


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
  (next-row 9))
