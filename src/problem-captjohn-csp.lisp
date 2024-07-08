;;; Filename: problem-captjohn-csp.lisp

;;; Brain Teaser logic problem, 
;;; Capt John's Journey (part 1)


(in-package :ww)

(ww-set *problem* captjohn-csp)

(ww-set *problem-type* csp)

(ww-set *solution-type* first)

(ww-set *tree-or-graph* tree)


(define-types
    captain (john)
    ship    (wasp)
    crew    (crew1 crew2)
    guard   (guard1 guard2)
    grass   (grass1 grass2 grass3)
    object  (either captain ship crew guard grass))


(define-dynamic-relations
  (remaining object $list))  ;list of coords--eg, ((0 2) ...)


(define-query get-remaining (?obj)
  (do (bind (remaining ?obj $coords))
      $coords))


(define-query in-same-row (?coord1 ?coord2)
  (= (first ?coord1) (first ?coord2)))


(define-query in-same-col (?coord1 ?coord2)
  (= (second ?coord1) (second ?coord2)))
  

(define-query in-row (?coord ?row)
  (= (first ?coord) ?row))


(define-query in-col (?coord ?col)
  (= (second ?coord) ?col))


(define-query vert-next-to (?coord1 ?coord2)
  (and (= (second ?coord1) (second ?coord2))
       (or (= (first ?coord1) (1+ (first ?coord2)))
           (= (first ?coord1) (1- (first ?coord2))))))

 
(define-query diag-next-to (?coord1 ?coord2)
  (or (and (= (1+ (first ?coord1)) (first ?coord2))
           (= (1- (second ?coord1)) (second ?coord2)))
      (and (= (1+ (first ?coord1)) (first ?coord2))
           (= (1+ (second ?coord1)) (second ?coord2)))
      (and (= (1- (first ?coord1)) (first ?coord2))
           (= (1- (second ?coord1)) (second ?coord2)))
      (and (= (1- (first ?coord1)) (first ?coord2))
           (= (1+ (second ?coord1)) (second ?coord2)))))


(define-update update-new-assignments (&rest ?objects&?coords)
  ;update assignments only if new
  (ww-loop for (?object ?coord) on ?objects&?coords by #'cddr
           do (doall (?obj object)
                (do (bind (remaining ?obj $coords))
                    (if (eql ?obj ?object)
                      (if (not (alexandria:length= 1 $coords))
                        (remaining ?object (list ?coord)))  ;update new
                      (remaining ?obj (remove ?coord $coords :test #'equal)))))))


;Make one rule for each set of related constraints


(define-action put1-guard1-guard2
;neither guard is in the last column (of 0,1,2)
  1
  (?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2))
  (and (not (in-col ?guard1-coord 2))
       (not (in-col ?guard2-coord 2)))
  (?guard1-coord ?guard2-coord)
  (assert (update-new-assignments guard1 ?guard1-coord guard2 ?guard2-coord)))


(define-action put2-guard1-guard2
;the two guards are not in the same row or column
  1
  (?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2))
  (and (not (in-same-row ?guard1-coord ?guard2-coord))
       (not (in-same-col ?guard1-coord ?guard2-coord)))
  (?guard1-coord ?guard2-coord)
  (assert (update-new-assignments guard1 ?guard1-coord guard2 ?guard2-coord)))


(define-action put3-john-guard1-guard2
;john is not in the same row or col as either guard
  1
  (?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2)
   ?john-coord (get-remaining john))
  (and (not (in-same-row ?john-coord ?guard1-coord))
       (not (in-same-col ?john-coord ?guard1-coord))
       (not (in-same-row ?john-coord ?guard2-coord))
       (not (in-same-col ?john-coord ?guard2-coord)))
  (?john-coord ?guard1-coord ?guard2-coord)
  (assert (update-new-assignments guard1 ?guard1-coord guard2 ?guard2-coord
                                  john ?john-coord)))


(define-action put4-guard1-guard2-wasp
;wasp is in the same row as one guard and in the same col as the other guard
  1
  (?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2)
   ?wasp-coord (get-remaining wasp))
  (and (or (and (in-same-row ?wasp-coord ?guard1-coord)
                (in-same-col ?wasp-coord ?guard2-coord))
           (and (in-same-row ?wasp-coord ?guard2-coord)
                (in-same-col ?wasp-coord ?guard1-coord))))
  (?guard1-coord ?guard2-coord ?wasp-coord)
  (assert (update-new-assignments guard1 ?guard1-coord guard2 ?guard2-coord
                                  wasp ?wasp-coord)))


(define-action put5-john-wasp
;the wasp is not in the same row or col as john
  1
  (?john-coord (get-remaining john) ?wasp-coord (get-remaining wasp))
  (and (not (in-same-row ?john-coord ?wasp-coord))
       (not (in-same-col ?john-coord ?wasp-coord)))
  (?john-coord ?wasp-coord)
  (assert (update-new-assignments john ?john-coord wasp ?wasp-coord)))


(define-action put6-guard1-guard2-grass
;both guards are vertically next to grass
  1
  (?guard1-coord (get-remaining guard1) ?guard2-coord (get-remaining guard2)
   ?grass1-coord (get-remaining grass1) ?grass2-coord (get-remaining grass2)
   ?grass3-coord (get-remaining grass3))
  (and (or (vert-next-to ?guard1-coord ?grass1-coord)
           (vert-next-to ?guard1-coord ?grass2-coord)
           (vert-next-to ?guard1-coord ?grass3-coord))
       (or (vert-next-to ?guard2-coord ?grass1-coord)
           (vert-next-to ?guard2-coord ?grass2-coord)
           (vert-next-to ?guard2-coord ?grass3-coord)))
  (?guard1-coord ?guard2-coord ?grass1-coord ?grass2-coord ?grass3-coord)
  (assert (update-new-assignments guard1 ?guard1-coord guard2 ?guard2-coord
                                  grass1 ?grass1-coord grass2 ?grass2-coord grass3 ?grass3-coord)))


(define-action put7-grass-at-0-1
;one of the grass is in row 0 column 1
  1
  (?grass1-coord (get-remaining grass1) ?grass2-coord (get-remaining grass2)
   ?grass3-coord (get-remaining grass3))
  (or (and (in-row ?grass1-coord 0)
           (in-col ?grass1-coord 1))
      (and (in-row ?grass2-coord 0)
           (in-col ?grass2-coord 1))
      (and (in-row ?grass3-coord 0)
           (in-col ?grass3-coord 1)))
  (?grass1-coord ?grass2-coord ?grass3-coord)
  (assert (update-new-assignments grass1 ?grass1-coord grass2 ?grass2-coord
                                  grass3 ?grass3-coord)))


(define-action put8-crew1-crew2-grass
;one of the grass is diagonally next to both crew
  1
  (?crew1-coord (get-remaining crew1) ?crew2-coord (get-remaining crew2)
   ?grass1-coord (get-remaining grass1) ?grass2-coord (get-remaining grass2)
   ?grass3-coord (get-remaining grass3))
  (or (and (diag-next-to ?grass1-coord ?crew1-coord)
           (diag-next-to ?grass1-coord ?crew2-coord))
      (and (diag-next-to ?grass2-coord ?crew1-coord)
           (diag-next-to ?grass2-coord ?crew2-coord))
      (and (diag-next-to ?grass3-coord ?crew1-coord)
           (diag-next-to ?grass3-coord ?crew2-coord)))
  (?crew1-coord ?crew2-coord ?grass1-coord ?grass2-coord ?grass3-coord)
  (assert (update-new-assignments crew1 ?crew1-coord crew2 ?crew2-coord
                                  grass1 ?grass1-coord grass2 ?grass2-coord grass3 ?grass3-coord)))


(define-init
  (remaining john   ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining wasp   ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining crew1  ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining crew2  ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining guard1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining guard2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining grass1 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining grass2 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))
  (remaining grass3 ((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))))


(define-goal
  (forall (?obj object)
    (and (bind (remaining ?obj $coords))
         (alexandria:length= 1 $coords))))
