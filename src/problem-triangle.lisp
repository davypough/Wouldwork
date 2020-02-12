;;;; Filename: problem-triangle.lisp


;;; Problem specification for triangle peg problem
;;; with peg-count.

;;; The peg board holes have coordinates measured 
;;; from the triangle's right (row) and top (col) sides,
;;; from 1 to *N*.


(in-package :ww)  ;required

(ww-set 'problem 'triangle)

(ww-set 'solution-type 'first)

(ww-set 'tree-or-graph 'tree)


(defparameter *N* 5)  ;the number of pegs on a side


(define-types
  peg (compute (loop for i from 1  ;peg1, peg2, ...
                     below (/ (* *N* (1+ *N*)) 2)
                   collect (intern (format nil "PEG~D" i))))
  row (compute (loop for i below *N*
                   collect i))
  col (compute (loop for i below *N*
                   collect i)))


(define-dynamic-relations
    (loc peg $row $col)      ;location of a peg
    (contents row col $peg)  ;peg contents at a location
    (peg-count $integer))    ;pegs remaining on the board


(define-action jump+row  ;jump in the +row direction
    1
  (?peg peg)
  (and (bind (loc ?peg $r $c))
       (<= $r (- *N* 3))
       (setq $r+1 (1+ $r))
       (bind (contents $r+1 $c $adj-peg))
       (setq $r+2 (+ $r 2))
       (not (bind (contents $r+2 $c $any-peg)))
       (bind (peg-count $peg-count)))
  (($r $c) fluent)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r+2 $c)
          (contents $r+2 $c ?peg)
          (not (loc $adj-peg $r+1 $c))
          (not (contents $r+1 $c $adj-peg))
          (peg-count (1- $peg-count))))


(define-action jump-row  ;jump in the -row direction
    1
  (?peg peg)
  (and (bind (loc ?peg $r $c))
       (>= $r (+ $c 2))
       (setq $r-1 (1- $r))
       (bind (contents $r-1 $c $adj-peg))
       (setq $r-2 (- $r 2))
       (not (bind (contents $r-2 $c $any-peg)))
       (bind (peg-count $peg-count)))
  (($r $c) fluent)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r-2 $c)
          (contents $r-2 $c ?peg)
          (not (loc $adj-peg $r-1 $c))
          (not (contents $r-1 $c $adj-peg))
          (peg-count (1- $peg-count))))


(define-action jump+col  ;jump in the +col direction
    1
  (?peg peg)
  (and (bind (loc ?peg $r $c))
       (>= $r (+ $c 2))
       (setq $c+1 (1+ $c))
       (bind (contents $r $c+1 $adj-peg))
       (setq $c+2 (+ $c 2))
       (not (bind (contents $r $c+2 $any-peg)))
       (bind (peg-count $peg-count)))
  (($r $c) fluent)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r $c+2)
          (contents $r $c+2 ?peg)
          (not (loc $adj-peg $r $c+1))
          (not (contents $r $c+1 $adj-peg))
          (peg-count (1- $peg-count))))


(define-action jump-col  ;jump in the -col direction
    1
  (?peg peg)
  (and (bind (loc ?peg $r $c))
       (>= $c 2)
       (setq $c-1 (1- $c))
       (bind (contents $r $c-1 $adj-peg))
       (setq $c-2 (- $c 2))
       (not (bind (contents $r $c-2 $any-peg)))
       (bind (peg-count $peg-count)))
  (($r $c) fluent)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r $c-2)
          (contents $r $c-2 ?peg)
          (not (loc $adj-peg $r $c-1))
          (not (contents $r $c-1 $adj-peg))
          (peg-count (1- $peg-count))))



(define-action jump+diag  ;jump in the +diagonal direction
    1
  (?peg peg)
  (and (bind (loc ?peg $r $c))
       (<= $r (- *N* 3))
       (setq $r+1 (1+ $r))
       (setq $c+1 (1+ $c))
       (bind (contents $r+1 $c+1 $adj-peg))
       (setq $r+2 (+ $r 2))
       (setq $c+2 (+ $c 2))
       (not (bind (contents $r+2 $c+2 $any-peg)))
       (bind (peg-count $peg-count)))
  (($r $c) fluent)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r+2 $c+2)
          (contents $r+2 $c+2 ?peg)
          (not (loc $adj-peg $r+1 $c+1))
          (not (contents $r+1 $c+1 $adj-peg))
          (peg-count (1- $peg-count))))


(define-action jump-diag  ;jump in the -diagonal direction
    1
  (?peg peg)
  (and (bind (loc ?peg $r $c))
       (>= $c 2)
       (setq $r-1 (1- $r))
       (setq $c-1 (1- $c))
       (bind (contents $r-1 $c-1 $adj-peg))
       (setq $r-2 (- $r 2))
       (setq $c-2 (- $c 2))
       (not (bind (contents $r-2 $c-2 $any-peg)))
       (bind (peg-count $peg-count)))
  (($r $c) fluent)
  (assert (not (contents $r $c ?peg))
          (loc ?peg $r-2 $c-2)
          (contents $r-2 $c-2 ?peg)
          (not (loc $adj-peg $r-1 $c-1))
          (not (contents $r-1 $c-1 $adj-peg))
          (peg-count (1- $peg-count))))


(progn (format t "~&Initializing database...~%")
  (loop with pegs = (gethash 'peg *types*)
    ;*db* is the name of the initial database
    ;update is the function that asserts a proposition
    ;into the database
    initially (update *db* `(peg-count ,(length pegs)))
    for row from 1 below *N*
      do (loop for col from 0 below *N*
            when (<= col row)
             do (let ((peg (pop pegs)))
                  (update *db* `(loc ,peg ,row ,col))
                  (update *db* `(contents ,row ,col ,peg))))))


(define-goal  ;only one peg left
  (peg-count 1))
