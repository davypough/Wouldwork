;;;; Filename: problem-4jugs.lisp

;;; Fluent problem specification for pouring between 4 jugs
;;; to achieve 10 gal.


(in-package :ww)  ;required


(setq *depth-cutoff* 6)  ;set to expected # steps to goal


(define-types
    jug (jug1 jug2 jug3 jug4))


(define-dynamic-relations
    (contents jug $real))


(define-static-relations
    (capacity jug $real))


(define-action pour  ;A into B
    1
  ((?jugA ?jugB) jug ($amtA $amtB $capB) fluent)
  (and (contents ?jugA $amtA)
       (> $amtA 0)
       (contents ?jugB $amtB) ;(ut::prt ?jugA $amtA ?jugB $amtB $capB) 
       (capacity ?jugB $capB) ;(print 4)
       (< $amtB $capB)) ;(print 5))
  ((?jugA ?jugB) jug ($amtA $amtB $capB) fluent)
  (assert (if (<= $amtA (- $capB $amtB))
            (and (contents ?jugA 0)
                 (contents ?jugB (+ $amtB $amtA)))
            (and (contents ?jugA (- (+ $amtA $amtB) $capB))
                 (contents ?jugB $capB)))))
                 

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