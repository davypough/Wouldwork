;;;; Filename: 4jugs-problem.lisp

;;; Fluent problem specification for pouring between 4 jugs
;;; to achieve 10 gal.


(in-package :ww)  ;required


(setq *depth-cutoff* 10)  ;set to expected # steps to goal


(define-types
    jug (jug1 jug2 jug3 jug4))


(define-base-relations
    (contents jug !real)
    (capacity jug !real))


(define-monitored-relations
    contents capacity)
      

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
    (contents jug1 9)
    (contents jug2 0)
    (contents jug3 0)
    (contents jug4 4)
    (capacity jug1 9)
    (capacity jug2 4)
    (capacity jug3 12)
    (capacity jug4 9))


(define-goal
    (contents jug3 10))
