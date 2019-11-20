;;;; Filename: problem-triangle2.lisp

;;; Note:  with *n* = 6, program runs out of memory, CTRL-C to interrupt processing.

;;; Problem specification for triangle peg problem with triples generator.
;;; Faster that original problem-triangle.

;;; The peg board holes have coordinates measured inward 
;;; from each of the triangle's sides: left ($x), right ($y),
;;; bottom ($z) varying from 1 to *n*.


(in-package :ww)  ;required

(ww-set 'problem 'triangle2)

(ww-set 'solution-type 'count)

(ww-set 'depth-cutoff 19)

(ww-set 'tree-or-graph 'graph)


(defparameter *n* 6)  ;the number of pegs on a side


(defun adjacent&colinear (?pos1 ?pos2 ?pos3)
  (destructuring-bind ((?x1 ?y1 ?z1) (?x2 ?y2 ?z2) (?x3 ?y3 ?z3))
      (list ?pos1 ?pos2 ?pos3)
  (or (and (= ?x1 ?x2 ?x3)  ;aligned in x direction
           (= 2 (abs (- ?y3 ?y1))) (= 1 (abs (- ?y2 ?y1)) (abs (- ?y3 ?y2))))
      (and (= ?y1 ?y2 ?y3)  ;aligned in y direction
           (= 2 (abs (- ?x3 ?x1))) (= 1 (abs (- ?x2 ?x1)) (abs (- ?x3 ?x2))))
      (and (= ?z1 ?z2 ?z3)  ;aligned in z direction
           (= 2 (abs (- ?x3 ?x1))) (= 1 (abs (- ?x2 ?x1)) (abs (- ?x3 ?x2)))))))


(define-types
  peg (compute (loop for i from 1  ;peg names: peg1, peg2, ...
                       to (/ (* *n* (1+ *n*)) 2)  ;peg for each position
                  collect (intern (format nil "PEG~D" i))))
  coord (compute (loop for i from 1 to *n*  ;x,y,z board coordinate
                        collect i))
  position (compute (loop for ?x from 1 to *n*  ;all board x,y,z positions
                       append (loop with max = (1+ (- *n* ?x))
                                 for ?y from 1 to max
                                 for ?z from max downto 1
                                   collect (list ?x ?y ?z))))
  span (compute (loop for i from 1  ;name of a triplet of positions
                        to (/ (* 3 (- *n* 2) (- *n* 1)) 2)  ;number of adjacent collinear triads
                   collect (intern (format nil "SPAN~D" i))))
  triad (compute (loop with positions = (gethash 'position *types*)  ;the triads of 3 positions
                       with triads
                       for ?pos1 in positions
                         do (loop for ?pos2 in positions
                                   do (loop for ?pos3 in positions
                                     when (adjacent&colinear ?pos1 ?pos2 ?pos3)
                                       do (push (list ?pos1 ?pos2 ?pos3) triads)))
                         finally (return (delete-duplicates (nreverse triads)
                                            :test (lambda (x y)
                                                     (alexandria:set-equal x y :test #'equal)))))))


(define-dynamic-relations
  (loc $peg position)  ;location of a peg
  (peg-count $integer))  ;pegs remaining on the board


(define-static-relations
  (triplet span $position1 $position2 $position3))  ;3 colinear adjacent positions
  

(define-action jump-incr
    1
  (?span span)
  (and (bind (triplet ?span $pos1 $pos2 $pos3))
       (bind (loc $peg1 $pos1))
       (bind (loc $peg2 $pos2))
       (not (bind (loc $peg3 $pos3)))
       (bind (peg-count $peg-count)))
  (($peg1 $pos1 $peg2 $pos2) fluent)
  (assert (not (loc $peg2 $pos2))
          (loc $peg1 $pos3)
          (not (loc $peg1 $pos1))
          (peg-count (1- $peg-count))))


(define-action jump-decr
    1
  (?span span)
  (and (bind (triplet ?span $pos1 $pos2 $pos3))
       (bind (loc $peg3 $pos3))
       (bind (loc $peg2 $pos2))
       (not (bind (loc $peg1 $pos1)))
       (bind (peg-count $peg-count)))
  (($peg3 $pos3 $peg2 $pos2) fluent)
  (assert (not (loc $peg2 $pos2))
          (loc $peg3 $pos1)
          (not (loc $peg1 $pos3))
          (peg-count (1- $peg-count))))


(define-init-action place-pegs
    0
  (dot-products ?peg peg ?position position)
  (not (eq ?peg 'peg1))  ;don't put peg in top position (hole)
  ()
  (assert (loc ?peg ?position)))


(define-init-action specify-triplets
    0
  (dot-products ?span span ?triad triad)
  (and (setq $pos1 (first ?triad))
       (setq $pos2 (second ?triad))
       (setq $pos3 (third ?triad)))
  ()
  (assert (triplet ?span $pos1 $pos2 $pos3)))


(define-init
  `(peg-count ,(1- (/ (* *n* (1+ *n*)) 2))))  ;all pegs except one

    
(define-goal  ;only one peg left
  (peg-count 1))
