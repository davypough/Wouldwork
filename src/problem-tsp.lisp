;;;; Filename: problem-tsp.lisp

;;; Problem specification for a simple traveling salesperson problem.


(in-package :ww)  ;required


(ww-set *problem* tsp)

(ww-set *solution-type* min-value)


(define-types
  city (a b c d e))


(define-dynamic-relations
  (path $list)
  (path-distance $fixnum))


(define-static-relations
  (distance city city $fixnum)
  (min-edges city $list))


(define-query get-best-relaxed-value? ()
  (do (setq $value 0)
      (bind (path $path))
      (setq $last-city (first $path))
      (setq $penultimate-city (second $path))
      (doall (?c city)
        (do (bind (min-edges ?c $min-edges))
            (if $penultimate-city  ;not in start state
              (if (or (eql ?c $last-city) (eql ?c $penultimate-city))
                (do (bind (distance $penultimate-city $last-city $dist))
                    (setq $value (+ $value
                                    (- (reduce #'+ $min-edges) (car (last $min-edges)))
                                    $dist)))
                (setq $value (+ $value (reduce #'+ $min-edges))))
              (setq $value (+ $value (reduce #'+ $min-edges))))))
      (ceiling (/ $value 2))))


(define-action move
    1
  (?city city)
  (and (bind (path $path))
       (setq $last-city (first $path))
       (or (and (not (member ?city $path))
                (bind (distance $last-city ?city $dist))
                (setq $new-path (cons ?city $path)))
           (and (= (length $path) 5)
                (bind (distance $last-city 'a $dist))
                (setq $new-path (cons 'a $path))))
       (bind (path-distance $path-distance))
       (setq $new-path-distance (+ $path-distance $dist))
       (setq $objective-value $new-path-distance))
  (($new-path $new-path-distance) fluent)
  (assert (path $new-path)
          (path-distance $new-path-distance)))


(define-init
  (path (a))  ;start at a & end at a
  (path-distance 0)
  (distance a b 3)
  (distance a c 1)
  (distance a d 5)
  (distance a e 8)
  (distance b c 6)
  (distance b d 7)
  (distance b e 9)
  (distance c d 4)
  (distance c e 2)
  (distance d e 3))


(define-init-action compute-min-city-edges
  0
  ()
  (always-true)
  ()
  (do (doall (?c city)
        (push (cons ?c nil) $city-edges))
      (doall (combinations (?c1 ?c2) city)
        (if (bind (distance ?c1 ?c2 $dist))
          (do (push $dist (cdr (assoc ?c1 $city-edges)))
              (push $dist (cdr (assoc ?c2 $city-edges))))))
      (setf $city-edges (mapcar (lambda (pair)
                                  (cons (car pair)
                                        (subseq (sort (cdr pair) #'<) 0 2)))
                                $city-edges))
      (doall (?c city)
        (do (setq $min-edges (cdr (assoc ?c $city-edges)))
            (assert (min-edges ?c $min-edges))))))

   
(define-goal
  (and (bind (path $path))
       (eql (first $path) 'a)))
