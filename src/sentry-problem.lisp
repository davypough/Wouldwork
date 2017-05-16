;;;; Filename: sentry-problem.lisp

;;; Problem specification for getting by an automated sentry by jamming it.
;;; See sentry-problem in user manual appendix.


(in-package :pl)  ;required

 
(setq *depth_cutoff* 16)


(define-types
  myself    (me)
  box       (box1)
  jammer    (jammer1)
  gun       (gun1)
  sentry    (sentry1)  
  switch    (switch1)
  red       ()
  green     ()
  area      (area1 area2 area3 area4 area5 area6 area7 area8)
  cargo     (either  jammer) ;box
  threat    (either gun sentry)
  target    (either threat))


(define-base-relations
  ;dynamic
  (holding myself cargo)
  (loc (either myself cargo threat) area)
  (red switch)
  (green switch)
  (jamming jammer target)
  (waiting)
  ;static
  (always-true)
  (adjacent area area)
  (los area target)  ;line-of-sight exists
  (visible area area)  ;area is wholly visible from another area
  (controls switch gun)
  (watches gun area))


(define-monitored-relations
  holding loc jamming)


(define-derived-relations
  (free> me)                 (not (exists (?c cargo) 
                                    (holding me ?c)))
  
  (passable> ?area1 ?area2)  (adjacent ?area1 ?area2)

  (safe> ?area)              (forall (?g gun)
                               (if (watches ?g ?area)
                                 (disabled> ?g)))

  (disabled> ?threat)        (or (exists (?j jammer)
                                   (jamming ?j ?threat))
                                 (exists (?s switch)
                                   (and (controls ?s ?threat)
                                        (green ?s)))))


(define-happening sentry1
  :events
  ((1 (not (loc sentry1 area6)) (loc sentry1 area7))
   (2 (not (loc sentry1 area7)) (loc sentry1 area6))
   (3 (not (loc sentry1 area6)) (loc sentry1 area5))
   (4 (not (loc sentry1 area5)) (loc sentry1 area6)))
  :repeat t
  :rebound
    (exists (?c cargo ?a area)
      (and (loc sentry1 ?a)
           (loc ?c ?a)))
  :interrupt
    (exists (?j jammer)
            (jamming ?j sentry1)))


(define-constraint
  ;Constraints only needed for happening events that can kill or delay an action.
  ;Global constraints included here. Return t if constraint violated.
  (exists (?s sentry ?a area)
    (and (loc me ?a)
         (loc ?s ?a)
         (not (disabled> ?s)))))


(define-action move
    1
  ((?area1 ?area2) area)
  (and (loc me ?area1)
       (passable> ?area1 ?area2)
       (safe> ?area2))
  ((?area1 ?area2) area)
  (and (not (loc me ?area1))
       (loc me ?area2)))


(define-action pickup
    1
  (?cargo cargo ?area area)
  (and (loc me ?area)
       (loc ?cargo ?area)
       (free> me))
  (?cargo cargo ?area area)
  (and (not (loc ?cargo ?area))
       (holding me ?cargo)
       (if (jammer ?cargo)
           (forall (?t target)
             (if (jamming ?cargo ?t)
                 (not (jamming ?cargo ?t)))))))


(define-action drop
    1
  (?cargo cargo ?area area)
  (and (loc me ?area)
       (holding me ?cargo))
  (?cargo cargo ?area area)
  (and (not (holding me ?cargo))
       (loc ?cargo ?area)))
       

(define-action jam
    1
  (?jammer jammer ?target target (?area1 ?area2) area)
  (and (holding me ?jammer)
       (loc me ?area1)
       (or (los ?area1 ?target)
           (and (loc ?target ?area2)
                (visible ?area1 ?area2))))
  (?target target ?jammer jammer ?area1 area)
  (and (not (holding me ?jammer))
       (loc ?jammer ?area1)
       (jamming ?jammer ?target)))


(define-action throw-switch
    1
  (?switch switch ?area area)
  (and (free> me)
       (loc me ?area)
       (loc ?switch ?area))
  (?switch switch)
  (if (red ?switch)
    (and (not (red ?switch))
         (green ?switch))
    (and (not (green ?switch))
         (red ?switch))))


(define-action wait
    0
  ()
  (always-true)
  ()
  (waiting))


(define-init
  ;dynamic
  (loc me area1)
  (loc jammer1 area1)
  (loc switch1 area3)
  (loc sentry1 area6)
  (loc box1 area4)
  (red switch1)
  ;static
  (watches gun1 area2)
  (controls switch1 gun1)
  (los area1 gun1)
  (los area2 gun1)
  (los area3 gun1)
  (los area4 gun1)
  (visible area5 area6)
  (visible area5 area7)
  (visible area5 area8)
  (visible area6 area7)
  (visible area6 area8)
  (visible area7 area8)
  (adjacent area1 area2)
  (adjacent area2 area3)
  (adjacent area2 area4)
  (adjacent area4 area5)
  (adjacent area5 area6)
  (adjacent area6 area7)
  (adjacent area7 area8))


(define-goal
  (loc me area8))