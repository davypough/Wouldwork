;;;; Filename: problem-2jugs.lisp

;;; Fluent problem specification for pouring between jugs
;;; to achieve 1gal given 2gal-jug & 5gal-jug. See user manual.


(in-package :ww)  ;required


(setq *depth-cutoff* 6)  ;set to expected # steps to goal


(define-types
    jug (jug1 jug2))


(define-dynamic-relations
    (contents jug $integer))


(define-static-relations
    (capacity jug $integer))


(define-action fill
    1
  (?jug jug ($amt $cap) fluent)
  (and (contents ?jug $amt)
       (capacity ?jug $cap)
       (< $amt $cap))
  (?jug jug $cap fluent)
  (assert (contents ?jug $cap)))


(define-action empty
    1
  (?jug jug $amt fluent)
  (and (contents ?jug $amt)
       (> $amt 0))
  (?jug jug)
  (assert (contents ?jug 0)))
        

(define-action pour  ;A into B
    1
  ((?jugA ?jugB) jug ($amtA $amtB $capB) fluent)
  (and (contents ?jugA $amtA)
       (> $amtA 0)
       (contents ?jugB $amtB)
       (capacity ?jugB $capB)
       (< $amtB $capB))
  ((?jugA ?jugB) jug ($amtA $amtB $capB) fluent)
  (assert (if (<= $amtA (- $capB $amtB))
            (and (contents ?jugA 0)
                 (contents ?jugB (+ $amtB $amtA)))
            (and (contents ?jugA (- (+ $amtA $amtB) $capB))
                 (contents ?jugB $capB)))))
                 

(define-init
    (contents jug1 0)
    (contents jug2 0)
    (capacity jug1 2)
    (capacity jug2 5))


(define-goal
    (or (contents jug1 1)
        (contents jug2 1)))
