;;;; Filename: problem-triangle-peg.lisp


;;; Problem specification for triangle peg problem.


(in-package :ww)  ;required


(setq *tree-or-graph* 'graph)


(setq *first-solution-sufficient* nil)


(defparameter *N* 6)  ;the number of pegs on a side


(define-types
  peg (compute (loop for i from 1 
                     below (/ (* *N* (1+ *N*)) 2)
                 collect (intern (format nil "PEG~D" i))))
  coord (compute (loop for i from 1 to *N* 
                   collect i)))


(define-dynamic-relations
  (loc peg $coord $coord $coord))


(define-query empty! (?x ?y ?z)  ;coordinates of a hole
  (not (exists (?p peg)
               (loc ?p ?x ?y ?z))))  ;x=row from left,
                                     ;y=row from right,
                                     ;z=row from bottom

(define-action jump
    1
  ((?peg1 ?peg2) peg)
  (and (bind (loc ?peg1 $x1 $y1 $z1))
       (bind (loc ?peg2 $x2 $y2 $z2))
       (or (and (= $x1 $x2)  ;aligned in x direction
                (< $x2 (1- *N*))
                (<= $y2 (- *N* $x2))
                (> $y2 1)
                (setq $delta (- $y2 $y1))
                (= (abs $delta) 1)
                (setq $target-x $x2)
                (setq $target-y (+ $y2 $delta))
                (setq $target-z (- $z2 $delta))
                (empty! $target-x $target-y $target-z))
           (and (= $y1 $y2)  ;aligned in y direction
                (< $y2 (1- *N*))
                (<= $z2 (- *N* $y2))
                (> $z2 1)
                (setq $delta (- $z2 $z1))
                (= (abs $delta) 1)
                (setq $target-x (- $x2 $delta))
                (setq $target-y $y2)
                (setq $target-z (+ $z2 $delta))
                (empty! $target-x $target-y $target-z))
           (and (= $z1 $z2)  ;aligned in z direction
                (< $z2 (1- *N*))
                (<= $x2 (- *N* $z2))
                (> $x2 1)
                (setq $delta (- $x2 $x1))
                (= (abs $delta) 1)
                (setq $target-x (+ $x2 $delta))
                (setq $target-y (- $y2 $delta))
                (setq $target-z $z2)
                (empty! $target-x $target-y $target-z))))
  (?peg1 peg ($x1 $y1 $z1) fluent ?peg2 peg
   ($x2 $y2 $z2) fluent)
  (assert (not (loc ?peg1 $x1 $y1 $z1))
          (not (loc ?peg2 $x2 $y2 $z2))
          (loc ?peg1 $target-x $target-y $target-z)))


(progn (format t "~&Initializing database...~%")
  (loop with pegs = (gethash 'peg *types*)
    for ?x from 1 to *N*
    do (loop with max = (1+ (- *N* ?x))
           for ?y from 1 to max
           for ?z from max downto 1
           unless (and (= ?x 1) (= ?y 1) (= ?z *N*))
           ;*db* is the name of the initial database
           ;update is the function that asserts a proposition
           ;into the database
           do (update *db* `(loc ,(pop pegs) ,?x ,?y ,?z)))))


(define-goal  ;only one peg left
  (exists (?p1 peg)
    (and (bind (loc ?p1 $x1 $y1 $z1))
         (not (exists (?p2 peg)
                (and (different ?p2 ?p1)
                     (bind (loc ?p2 $x2 $y2 $z2))))))))
