;;;; Filename: problem-knap4.lisp

;;; Problem specification for a 4-item knapsack problem.
;;; Simple brute-force search for small problem.


(in-package :ww)  ;required

(ww-set 'problem 'knap4)

(ww-set 'depth-cutoff 0)

(ww-set 'solution-type 'max-value)

(ww-set 'tree-or-graph 'tree)


(define-types
  knapsack (knapsack1)
  item (item1 item2 item3 item4))


(define-dynamic-relations
  (in item knapsack)
  (load knapsack $fixnum)
  (worth knapsack $fixnum))


(define-static-relations
  (capacity knapsack $fixnum)
  (value item $fixnum)
  (weight item $fixnum))


(define-action put
    1
  (?knapsack knapsack ?item item)
  (and (not (in ?item ?knapsack))
       (bind (weight ?item $item-weight))
       (bind (load ?knapsack $knapsack-load))
       (setq $new-knapsack-load (+ $knapsack-load $item-weight))
       (bind (capacity ?knapsack $knapsack-capacity))
       (<= $new-knapsack-load $knapsack-capacity)
       (bind (worth ?knapsack $knapsack-worth))
       (bind (value ?item $item-value))       
       (setq $new-knapsack-worth (+ $knapsack-worth $item-value))
       (setq $objective-value $new-knapsack-worth))
  (?item item ?knapsack knapsack)
  (assert (in ?item ?knapsack)
          (load ?knapsack $new-knapsack-load)
          (worth ?knapsack $new-knapsack-worth)))


(define-init
  (capacity knapsack1 11)
  (load knapsack1 0)
  (worth knapsack1 0)
  (value item1 8)
  (weight item1 4)
  (value item2 10)
  (weight item2 5)
  (value item3 15)
  (weight item3 8)
  (value item4 4)
  (weight item4 3))


(define-goal
  (not (exists (?knapsack knapsack ?item item)
         (and (not (in ?item ?knapsack))
              (bind (weight ?item $item-weight))
              (bind (capacity ?knapsack $knapsack-capacity))
              (bind (load ?knapsack $knapsack-load))
              (<= (+ $knapsack-load $item-weight) $knapsack-capacity)))))
