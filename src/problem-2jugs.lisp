;;;; Filename: problem-2jugs.lisp


;;; Fluent problem specification for pouring between jugs
;;; to achieve 1 gal given 2-gal jug & 5-gal jug.


(in-package :ww)  ;required

(ww-set 'problem '2jugs)

(ww-set 'depth-cutoff 6)  ;set to expected # steps to goal

(ww-set 'solution-type 'min-length)


(define-types
    jug (jug1 jug2))


(define-dynamic-relations
    (contents jug $integer))


(define-static-relations
    (capacity jug $integer))


(define-action fill
    2
  (?jug jug)
  (and (bind (contents ?jug $amt))
       (bind (capacity ?jug $cap))
       (< $amt $cap))
  (?jug jug $cap fluent)
  (assert (contents ?jug $cap)))


(define-action empty
    1
  (?jug jug)
  (and (bind (contents ?jug $amt))
       (> $amt 0))
  (?jug jug)
  (assert (contents ?jug 0)))


(define-action pour  ;A into B
    3
  ((?jugA ?jugB) jug)
  (and (bind (contents ?jugA $amtA))
       (> $amtA 0)
       (bind (contents ?jugB $amtB))
       (bind (capacity ?jugB $capB))
       (< $amtB $capB))
  (?jugA jug $amtA fluent ?jugB jug ($amtB $capB) fluent)
  (assert (if (<= $amtA (- $capB $amtB))
            (assert (contents ?jugA 0)
                    (contents ?jugB (+ $amtB $amtA)))
            (assert (contents ?jugA (- (+ $amtA $amtB) $capB))
                    (contents ?jugB $capB)))))


(define-init
    (contents jug1 0)
    (contents jug2 0)
    (capacity jug1 2)
    (capacity jug2 5))


(define-goal
    (or (contents jug1 1)
        (contents jug2 1)))
