;;; Filename: problem-captjohn.lisp

;;; Brain Teaser logic problem, Capt John's Journey (part 1)


(in-package :ww)

(setq *tree-or-graph* 'tree)

;(setq *first-solution-sufficient* t)


(define-types
    captain (john)
    ship    (wasp)
    crew    (crew1 crew2)
    guard   (guard1 guard2)
    grass   (grass1 grass2 grass3)
    object  (either captain ship crew guard grass)
    row     (1 2 3)
    column  (1 2 3))


(define-dynamic-relations
    (loc object $row $column)
    (next-row $row)
    (next-col $column))


(define-derived-relations
    (already-placed* ?object)  (let ($r $c)
                                 (loc ?object $r $c))
      
    (in-same-row* ?object1 ?object2)  (let ($r1 $c1 $r2 $c2)
                                        (loc ?object1 $r1 $c1)
                                        (loc ?object2 $r2 $c2)
                                        (= $r1 $r2))

    (in-same-col* ?object1 ?object2)  (let ($r1 $c1 $r2 $c2)
                                        (loc ?object1 $r1 $c1)
                                        (loc ?object2 $r2 $c2)
                                        (= $c1 $c2))
  
    (in-col* ?object ?column)  (let ($r $c)
                                 (loc ?object $r $c)
                                 (= $c ?column))
  
    (vert-next-to* ?object1 ?object2)  (let ($r1 $c1 $r2 $c2)
                                         (loc ?object1 $r1 $c1)
                                         (loc ?object2 $r2 $c2)
                                         (and (= $c1 $c2)
                                              (or (= $r1 (1+ $r2))
                                                  (= $r1 (1- $r2)))))
 
    (diag-next-to* ?object1 ?object2)  (let ($r1 $c1 $r2 $c2)
                                         (loc ?object1 $r1 $c1)
                                         (loc ?object2 $r2 $c2)
                                         (or (and (= (1+ $r1) $r2)
                                                  (= (1+ $c1) $c2))
                                             (and (= (1+ $r1) $r2)
                                                  (= (1- $c1) $c2))
                                             (and (= (1- $r1) $r2)
                                                  (= (1+ $c1) $c2))
                                             (and (= (1- $r1) $r2)
                                                  (= (1- $c1) $c2))))
)

  
(define-action put
    1
  (?object object ($row $col) fluent)
  (and (not (already-placed* ?object))
       (next-row $row)
       (next-col $col))
  (?object object ($row $col) fluent)
  (assert (loc ?object $row $col)
          (if (= $col 3)
            (assert (next-col 1)
                    (next-row (1+ $row)))
            (assert (next-col (1+ $col))))))


(define-init
  (next-row 1)
  (next-col 1))


(define-goal
    (and (next-row 4)  ;if next-row /= 4, no need to check constraints
         (and (not (in-same-row* wasp john))
              (not (in-same-col* wasp john)))
         (forall (?guard guard)
           (and (not (in-same-row* john ?guard))
                (not (in-same-col* john ?guard))))
         (forall (?guard guard)
           (not (in-col* ?guard 3)))
         (forall (?guard guard)
           (exists (?grass grass)
             (vert-next-to* ?guard ?grass)))
         (exists ((?guard1 ?guard2) guard)
           (and (in-same-row* wasp ?guard1)
                (in-same-col* wasp ?guard2)))
         (exists (?grass grass)
           (forall (?crew crew)
             (diag-next-to* ?grass ?crew)))
         (exists (?grass grass)
           (loc ?grass 1 2))
         (exists ((?guard1 ?guard2) guard)
           (and (not (in-same-row* ?guard1 ?guard2))
                (not (in-same-col* ?guard1 ?guard2))))))
