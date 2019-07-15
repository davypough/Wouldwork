;;;; Filename: problem-8queens.lisp

;;; Problem specification for 8-queens.


(in-package :ww)  ;required

(ww-set 'problem '8queens)

(ww-set 'depth-cutoff 8)

(ww-set 'solution-type 'first)

(ww-set 'tree-or-graph 'tree)


(define-types
    queen   (queen1 queen2 queen3 queen4 queen5 queen6 queen7 queen8)
    column  (1 2 3 4 5 6 7 8))


(define-dynamic-relations
  (loc queen $fixnum column)
  (placed queen)
  (next-row $fixnum))


(define-action put
    1
  (?queen queen ?column column)
  (and (not (placed ?queen))
       (bind (next-row $row))
       (not (exists (?q queen ?c column)
              (and (placed ?q)
                   (bind (loc ?q $r ?c))
                   (or (= $r $row)
                       (= ?c ?column)
                       (= (- $r $row) (- ?c ?column))
                       (= (- $r $row) (- ?column ?c)))))))
  (?queen queen $row fluent ?column column)
  (assert (loc ?queen $row ?column)
          (placed ?queen)
          (next-row (1+ $row))))
   

(define-init
    (next-row 1))


(define-goal
  (next-row 9))
