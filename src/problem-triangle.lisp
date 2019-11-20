;;;; Filename: problem-triangle.lisp


;;; Problem specification for triangle peg problem with peg-count.

;;; The peg board holes have coordinates measured inward 
;;; from each of the triangle's sides: left ($x), right ($y),
;;; bottom ($z) varying from 1 to *N*.


(in-package :ww)  ;required

(ww-set 'problem 'triangle)

(ww-set 'solution-type 'first)

(ww-set 'depth-cutoff 13)

(ww-set 'tree-or-graph 'tree)


(defparameter *N* 5)  ;the number of pegs on a side


(define-types
  peg (compute (loop for i from 1  ;peg1, peg2, ...
                     below (/ (* *N* (1+ *N*)) 2)
                 collect (intern (format nil "PEG~D" i)))))


(define-dynamic-relations
    (loc peg $x $y $z)  ;location of a peg
    (peg-count $integer))  ;pegs remaining on the board


(define-query empty? (?x ?y ?z)  ;empty hole location
  (not (exists (?p peg)
         (loc ?p ?x ?y ?z))))


(define-action jump
    1
  ((?peg1 ?peg2) peg)
  (and (bind (loc ?peg1 $x1 $y1 $z1))
       (bind (loc ?peg2 $x2 $y2 $z2))
       (or (and (= $x1 $x2)  ;aligned in x direction
                (<= $y2 (- *N* $x2))
                (> $y2 1)
                (setq $delta (- $y2 $y1))
                (= (abs $delta) 1)
                (setq $target-x $x2)
                (setq $target-y (+ $y2 $delta))
                (setq $target-z (- $z2 $delta))
                (empty? $target-x $target-y $target-z))
           (and (= $y1 $y2)  ;aligned in y direction
                (<= $z2 (- *N* $y2))
                (> $z2 1)
                (setq $delta (- $z2 $z1))
                (= (abs $delta) 1)
                (setq $target-x (- $x2 $delta))
                (setq $target-y $y2)
                (setq $target-z (+ $z2 $delta))
                (empty? $target-x $target-y $target-z))
           (and (= $z1 $z2)  ;aligned in z direction
                (<= $x2 (- *N* $z2))
                (> $x2 1)
                (setq $delta (- $x2 $x1))
                (= (abs $delta) 1)
                (setq $target-x (+ $x2 $delta))
                (setq $target-y (- $y2 $delta))
                (setq $target-z $z2)
                (empty? $target-x $target-y $target-z)))
       (bind (peg-count $peg-count)))
  (?peg1 peg ($x1 $y1 $z1) fluent
   ?peg2 peg ($x2 $y2 $z2) fluent)
  (assert (not (loc ?peg2 $x2 $y2 $z2))
          (loc ?peg1 $target-x $target-y $target-z)
          (peg-count (1- $peg-count))))


(progn (format t "~&Initializing database...~%")
  (loop with pegs = (gethash 'peg *types*)
    ;*db* is the name of the initial database
    ;update is the function that asserts a proposition
    ;into the database
    initially (update *db* `(peg-count ,(length pegs)))
    for ?x from 1 to *N*
    do (loop with max = (1+ (- *N* ?x))
           for ?y from 1 to max
           for ?z from max downto 1
           unless (and (= ?x 1) (= ?y 1) (= ?z *N*))
           do (update *db* `(loc ,(pop pegs) ,?x ,?y ,?z)))))


(define-goal  ;only one peg left
  (peg-count 1))
