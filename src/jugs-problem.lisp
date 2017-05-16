;;;; Filename: jugs-problem.lisp

;;; Fluent problem specification for pouring between jugs
;;; to achieve 1gal given 2gal-jug & 5gal-jug. See user manual.


(in-package :pl)  ;required


(setq *depth_cutoff* 6)  ;set to expected # steps to goal


(define-types
    jug (jug1 jug2))


(define-base-relations
    (contents jug !real)
    (capacity jug !real))


(define-monitored-relations
    contents capacity)


(define-action fill
    0
  (?jug jug ($amt $cap) fluent)
  (and (contents ?jug $amt)
       (capacity ?jug $cap)
       (< $amt $cap))
  (?jug jug $cap fluent)
  (contents ?jug $cap))


(define-action empty
    0
  (?jug jug $amt fluent)
  (and (contents ?jug $amt)
       (> $amt 0))
  (?jug jug)
  (contents ?jug 0))
        

(define-action pour  ;A into B
    0
  ((?jugA ?jugB) jug ($amtA $amtB $capB) fluent)
  (and (contents ?jugA $amtA)
       (> $amtA 0)
       (contents ?jugB $amtB)
       (capacity ?jugB $capB)
       (< $amtB $capB))
  ((?jugA ?jugB) jug ($amtA $amtB $capB) fluent)
  (if (<= $amtA (- $capB $amtB))
      (and (contents ?jugA 0)
           (contents ?jugB (+ $amtB $amtA)))
      (and (contents ?jugA (- (+ $amtA $amtB) $capB))
           (contents ?jugB $capB))))
                 

(define-init
    (contents jug1 0)
    (contents jug2 0)
    (capacity jug1 2)
    (capacity jug2 5))


(define-goal
    (or (contents jug1 1)
        (contents jug2 1)))
