;;;; Filename: problem-smallspace.lisp

;;; Problem specification (in Talos Principle)
;;; for the small space problem in Road to Gehenna sigil dome.


(in-package :ww)  ;required

 
(setq *depth-cutoff* 9)

(setq *tree-or-graph* 'tree)

(setq *first-solution-sufficient* t)


(define-types
  myself      (me)
  gate        (gate1 gate2)
  barrier     ()
  jammer      ()
  connector   (connector1 connector2)
  box         ()
  fan         ()
  gears       ()
  ladder      ()
  rostrum     ()
  hue         (blue red)
  transmitter (transmitter1 transmitter2)
  receiver    (receiver1 receiver2)
  area        (area1 area2 area3 area4 area5 area6 area7 area8)
  cargo       (either connector)
  target      (either gate gears)
  divider     (either gate barrier)
  terminus    (either transmitter receiver connector)
  fixture     (either transmitter receiver)
  station     (either fixture gate)
  support     (either box rostrum))


(define-dynamic-relations
  (holding myself $cargo)
  (free myself)
  (loc (either myself cargo) $area)
  (on (either myself cargo) $support)
  (attached fan gears)
  (jamming jammer $target)
  (connecting terminus terminus)
  (active (either connector gate))
  (inactive (either connector gate))
  (color terminus $hue))


(define-static-relations
  (adjacent area area)  ;agent can always move unimpeded between areas
  (locale fixture area)
  (separates divider area area)
  (climbable> ladder area area)
  (height support $real)
  (controls receiver $gate)
  (controls2 receiver receiver $gate)  ;gate controlled by two receivers together
  (los0 area (either gate fixture))  ;clear los from an area to a gate/fixture
  (los1 area divider (either gate fixture))
  (los2 area divider divider (either gate fixture))
  (visible0 area area)  ;could see a potential mobile object in an area from a given area
  (visible1 area divider area)
  (visible2 area divider divider area))


(define-complementary-relations  
  (holding myself $cargo) -> (not (free myself))
  (active (either connector gate)) -> (not (inactive (either connector gate)))
  (inactive (either connector gate)) -> (not (active (either connector gate))))


(define-derived-relations
  ;predicates for simplifying preconditions and for testing the state of various objects

  (passable* ?area1 ?area2)  (or (adjacent ?area1 ?area2)
                                 (exists (?b (either barrier ladder))
                                   (and (separates ?b ?area1 ?area2)
                                        (free me)))  ;must drop cargo first
                                 (exists (?g gate)
                                   (and (separates ?g ?area1 ?area2)
                                        (inactive ?g))))

  (visible* ?area1 ?area2)  (or (visible0 ?area1 ?area2)
                                (visible-thru-1-divider* ?area1 ?area2)
                                (visible-thru-2-dividers* ?area1 ?area2))

  (visible-thru-1-divider* ?area1 ?area2)  (exists (?d divider)
                                             (and (visible1 ?area1 ?d ?area2)
                                                  (or (barrier ?d)
                                                      (and (gate ?d)
                                                           (inactive ?d)))))

  (visible-thru-2-dividers* ?area1 ?area2)  (exists ((?d1 ?d2) divider)
                                              (and (visible2 ?area1 ?d1 ?d2 ?area2)
                                                   (or (and (barrier ?d1)
                                                            (barrier ?d2))
                                                       (and (barrier ?d1)
                                                            (gate ?d2)
                                                            (inactive ?d2))
                                                       (and (barrier ?d2)
                                                            (gate ?d1)
                                                            (inactive ?d1))
                                                       (and (gate ?d1)
                                                            (inactive ?d1)
                                                            (gate ?d2)
                                                            (inactive ?d2)))))

  (los* ?area ?station)  (or (los0 ?area ?station)
                             (los-thru-1-divider* ?area ?station)
                             (los-thru-2-dividers* ?area ?station))

  (los-thru-1-divider* ?area ?station)  (exists (?d divider)
                                          (and (los1 ?area ?d ?station)
                                               (or (barrier ?d)
                                                   (and (gate ?d)
                                                        (inactive ?d)))))

  (los-thru-2-dividers* ?area ?station)  (exists ((?d1 ?d2) divider)
                                           (and (los2 ?area ?d1 ?d2 ?station)
                                                (or (and (barrier ?d1)
                                                         (barrier ?d2))
                                                    (and (barrier ?d1)
                                                         (gate ?d2)
                                                         (inactive ?d2))
                                                    (and (barrier ?d2)
                                                         (gate ?d1)
                                                         (inactive ?d1))
                                                    (and (gate ?d1)
                                                         (inactive ?d1)
                                                         (gate ?d2)
                                                         (inactive ?d2)))))

  (connectable* ?area ?terminus)  (or (los* ?area ?terminus)  ;from connector in area to terminus
                                      (and (connector ?terminus)
                                           (exists (?a area)
                                             (and (loc ?terminus ?a)
                                                  (visible* ?area ?a)))))
  
  (compatible-colors* ?hue1 ?hue2)  (or (eql ?hue1 ?hue2)
                                        (eql ?hue1 nil)
                                        (eql ?hue2 nil))

  (source* ?terminus)  (or (transmitter ?terminus)
                           (active ?terminus))
                                       
  (colorable* ?terminus)  (and (connector ?terminus)  ;same as inactive-connector*
                               (inactive ?terminus))



  ;derived relations for simplifying effects


  (disconnect-connector* ?connector)  (forall (?t terminus)
                                        (if (connecting ?connector ?t)
                                          (assert (not (connecting ?connector ?t))
                                                  (if (and (active ?t)
                                                           (not (exists (?s (either transmitter connector))
                                                                  (and (connecting ?t ?s)
                                                                       (source* ?s)))))
                                                      (assert (inactive ?t)
                                                              (bind (color ?t $hue))
                                                            (if (not (eql $hue nil))  ;(color ?t $hue1)
                                                              (not (color ?t $hue))))))))

  (activate-terminus1-given-terminus2* ?terminus1 ?terminus2)  (if (and (connector ?terminus1)
                                                                        (inactive ?terminus1)
                                                                        (source* ?terminus2))
                                                                 (active ?terminus1))
)


(define-precondition-function elevation! (?support)
  (let ($h $s)
    (height ?support $h)
    (bind (on ?support $s))
    (if (eql $s nil)
      (return-from elevation! $h)
      (return-from elevation! (+ $h (elevation! state $s))))))


(define-effect-function disengage-jammer! (?jammer ?target)
  (assert (not (jamming ?jammer ?target))
          (if (not (exists (?j jammer)
                     (and (different ?j ?jammer)
                          (jamming ?j ?target))))
            (assert (active ?target)))))


(define-action connect-to-2-terminus  ;using held connector
    1
  ($cargo fluent (?terminus1 ?terminus2) terminus ($area $hue1 $hue2) fluent)
  (and (holding me $cargo)
       (connector $cargo)
       (loc me $area)
       (connectable* $area ?terminus1)
       (connectable* $area ?terminus2)
       (bind (color ?terminus1 $hue1))
       (bind (color ?terminus2 $hue2))
       (compatible-colors* $hue1 $hue2))
  ($cargo fluent ?terminus1 terminus $hue1 fluent ?terminus2 terminus $hue2 fluent $area fluent)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (connecting $cargo ?terminus1)
          (connecting $cargo ?terminus2)
          (if (not (eql $hue1 nil))
            (assert (color $cargo $hue1)
                    (if (colorable* ?terminus2)
                      (color ?terminus2 $hue1))))
          (if (not (eql $hue2 nil))
            (assert (color $cargo $hue2)
                    (if (colorable* ?terminus1)
                      (color ?terminus1 $hue2))))
          (activate-terminus1-given-terminus2* ?terminus1 ?terminus2)
          (activate-terminus1-given-terminus2* ?terminus2 ?terminus1)
          (if (or (source* ?terminus1)
                  (source* ?terminus2))
            (active $cargo))))


(define-action connect-to-1-terminus  ;using held connector
    1
  ($cargo fluent ?terminus terminus ($area $hue) fluent)
  (and (holding me $cargo)
       (connector $cargo)
       (loc me $area)
       (connectable* $area ?terminus)
       (bind (color ?terminus $hue)))
  ($cargo fluent ?terminus terminus ($hue $area) fluent)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (connecting $cargo ?terminus)
          (if (not (eql $hue nil))
            (color $cargo $hue))
          (activate-terminus1-given-terminus2* $cargo ?terminus)))


(define-action jam
    1
  ($cargo fluent ?target target $area fluent)
  (and (holding me $cargo)
       (jammer $cargo)
       (loc me $area)
       (los* $area ?target))
  (?target target $cargo fluent $area fluent)
  (assert (not (holding me $cargo))
          (loc $cargo $area)
          (jamming $cargo ?target)
          (inactive ?target)))


(define-action pickup-jammer
    1
  (?jammer jammer ($area $target) fluent)
  (and (free me)
       (loc me $area)
       (loc ?jammer $area)
       (bind (jamming ?jammer $target)))
  (?jammer jammer ($area $target) fluent)
  (assert (holding me ?jammer)
          (not (loc ?jammer $area))
          (if (not (eql $target nil))
            (disengage-jammer! ?jammer $target))))


(define-action pickup-connector
    1
  (?connector connector ($area $hue) fluent)
  (and (free me)
       (loc me $area)
       (loc ?connector $area)
       (bind (color ?connector $hue)))
  (?connector connector ($area $hue) fluent)
  (assert (holding me ?connector)
          (not (loc ?connector $area))
          (if (not (eql $hue nil))
            (not (color ?connector $hue)))
          (if (active ?connector)
            (not (active ?connector)))
          (disconnect-connector* ?connector)))


(define-action put
    1
  (?support support ($cargo $elev $area) fluent)
  (and (holding me $cargo)
       (loc me $area)
       (loc ?support $area)
       (not (exists (?c cargo)  ;cleartop ?support
              (on ?c ?support)))
       (setq $elev (elevation! ?support))  ;recursive function, not derived predicate
       (<= $elev 1))
  ($cargo fluent ?support support ($elev $area) fluent)
  (assert (on $cargo ?support)
          (loc $cargo $area)
          (not (holding me $cargo))))


(define-action drop-cargo
    1
  ($cargo fluent $area fluent)
  (and (loc me $area)
       (holding me $cargo))
  ($cargo fluent $area fluent)
  (assert (not (holding me $cargo))
          (loc $cargo $area)))

#|  ;doubles time to find 1st solution
(define-action double-move
    2
  ($area1 fluent (?area2 ?area3) area)
  (and (loc me $area1)
       (different $area1 ?area2)
       (different $area1 ?area3)
       (passable* $area1 ?area2)
       (passable* ?area2 ?area3))
  ($area1 fluent (?area2 ?area3) area)
  (assert (not (loc me $area1))
          (loc me ?area3)))
|#

(define-action move
    1
  ($area1 fluent ?area2 area)
  (and (loc me $area1)
       (different $area1 ?area2)
       (passable* $area1 ?area2))
  ($area1 fluent ?area2 area)
  (assert (not (loc me $area1))
          (loc me ?area2)))


(define-init
  ;dynamic
  (loc me area1)
  (loc connector1 area4)
  (loc connector2 area7)
  (free me)
  (active gate1)
  (active gate2)
  
  ;static
  (adjacent area1 area2)
  (adjacent area2 area3)
  (adjacent area3 area4)
  (adjacent area4 area5)
  (adjacent area6 area7)
  (locale transmitter1 area4)
  (locale transmitter2 area7)
  (locale receiver1 area4)
  (locale receiver2 area8)
  (color transmitter1 blue)
  (color transmitter2 red)
  (color receiver1 blue)
  (color receiver2 red)
  (controls receiver1 gate1)
  (controls receiver2 gate2)
  (separates gate1 area4 area7)
  (separates gate2 area7 area8)

;los is from an area to a fixed station
  (los0 area2 transmitter1)
  (los0 area3 transmitter1)
  (los0 area5 transmitter1)
  (los0 area6 transmitter1)
  (los0 area8 transmitter1)
  (los1 area7 gate1 transmitter1)
  (los0 area6 transmitter2)
  (los1 area8 gate2 transmitter2)
  (los0 area3 receiver1)
  (los0 area5 receiver1)
  (los0 area5 receiver2)
  (los1 area7 gate2 receiver2)
  (los2 area3 gate1 gate2 receiver2)
  (los2 area4 gate1 gate2 receiver2)

;visibility is from an area to an area potentially containing a movable target or terminus
  (visible0 area1 area3)
  (visible0 area1 area4)
  (visible0 area1 area5)
  (visible1 area1 gate1 area7) 
  (visible0 area2 area4)
  (visible0 area2 area5)
  (visible0 area2 area6)
  (visible1 area2 gate1 area7) 
  (visible2 area2 gate1 gate2 area8)
  (visible0 area3 area5)
  (visible0 area3 area6)
  (visible1 area3 gate1 area7) 
  (visible2 area3 gate1 gate2 area8)
  (visible0 area4 area6)
  (visible0 area4 area8)
  (visible1 area4 gate1 area7) 
  (visible1 area4 gate1 area6) 
  (visible2 area4 gate1 gate2 area8)
  (visible0 area5 area6)
  (visible0 area5 area8)
  (visible1 area5 gate1 area7) 
  (visible1 area6 gate2 area8) 
)


;init-actions save listing systematic facts

(define-init-action init-los0  ;los exists to any station within its local area
    0
  (?station station (?area1 ?area2) area)
  (or (locale ?station ?area1)             ;for fixtures
      (separates ?station ?area1 ?area2))  ;for gates
  (?station station ?area1 area)
  (assert (los0 ?area1 ?station)))


(define-init-action init-visible0-locally  ;any object is visible from its own local area
    0
  (?area area)
  (always-true)
  (?area area)
  (assert (visible0 ?area ?area)))


(define-init-action init-visible0-via-adjacency  ;any object is visible from an adjacent area
    0
  ((?area1 ?area2) area)
  (adjacent ?area1 ?area2)
  ((?area1 ?area2) area)
  (assert (visible0 ?area1 ?area2)))


(define-init-action init-visible1-thru-divider
    0
  (?divider divider (?area1 ?area2) area)
  (separates ?divider ?area1 ?area2)
  (?divider divider (?area1 ?area2) area)
  (assert (visible1 ?area1 ?divider ?area2)))


(define-init-action inactive-connectors
    0
  (?connector connector)
  (always-true)
  (?connector connector)
  (assert (inactive ?connector)))


(define-goal  ;always put this last
  (inactive gate2)
)

