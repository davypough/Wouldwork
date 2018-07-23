;;;; Filename: problem-crater.lisp

;;; Problem specification (in Talos Principle)
;;; for the (second) Nexus-2 crater problem in Road to Gehenna.


(in-package :ww)  ;required

 
(setq *depth-cutoff* 9)

(setq *tree-or-graph* 'tree)

;(setq *first-solution-sufficient* t)


(define-types
  myself      (me)
  gate        (gate4 gate5 gate6)
  barrier     (barrier3 barrier4)
  jammer      (jammer1)
  connector   (connector2 connector3 connector4 connector5 connector6)
  box         (box1 box2 box3 box4 box5)
  fan         (fan1 fan2 fan3 fan4)
  gears       (gears2 gears3)
  ladder      (ladder3 ladder4)
  rostrum     (rostrum2)
  hue         (blue red)
  transmitter (transmitter2)
  receiver    (receiver7 receiver8 receiver9 receiver10 receiver11 receiver12
               receiver13 receiver14)
  area        (area1 area2 area8 area9 area10 area11 area12)
  divider     (either gate barrier)
  cargo       (either jammer connector box fan)
  target      (either gate gears)
  terminus    (either transmitter receiver connector)
  fixture     (either transmitter receiver gears ladder rostrum)
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
  (active (either connector gate gears))
  (inactive (either connector gate gears))
  (color terminus $hue))


(define-static-relations
  (adjacent area area)  ;agent can always move unimpeded between areas
  (locale fixture area)
  (separates divider area area)
  (climbable> ladder area area)
  (height support $real)
  (controls receiver (either $gate $gears))
  (controls2 receiver receiver $gate)  ;gate controlled by two receivers together
  (los0 area (either gate fixture))  ;clear los from an area to a gate/fixture
  (los1 area divider (either gate fixture))
  (los2 area divider divider (either gate fixture))
  (visible0 area area)  ;could see a potential mobile object in an area from a given area
  (visible1 area divider area)
  (visible2 area divider divider area))


(define-complementary-relations  
  (holding myself $cargo) -> (not (free myself))
  (active (either gears connector gate)) -> (not (inactive (either gate gears connector)))
  (inactive (either connector gears gate)) -> (not (active (either connector gate gears))))


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
  (free me)
  (loc jammer1 area1)
  (loc connector2 area2) (loc connector3 area8) (loc connector4 area10)
  (loc connector5 area11) (loc connector6 area11)
  (loc box1 area2) (loc box3 area1) (loc box4 area1) (loc box5 area1)
  (on box3 box4)
  (loc fan2 area12) (loc fan3 area2) (loc fan1 area1)
  (attached fan3 gears3)
  (on connector2 box1)
  (active gate4) (active gate5) (active gate6)
  
  ;static
  (adjacent area8 area9)
  (height box2 1) (height box3 1) (height box4 1) (height box5 1)
  (height rostrum2 0.5)
  (locale transmitter2 area8)
  (locale rostrum2 area10)
  (locale ladder3 area10) (locale ladder4 area10)
  (locale gears2 area10) (locale gears3 area2)
  (locale receiver7 area9) (locale receiver8 area10) (locale receiver9 area10) 
  (locale receiver10 area10) (locale receiver11 area11)  (locale receiver12 area11)
  (locale receiver13 area11) (locale receiver14 area2)
  (color transmitter2 red)
  (color receiver7 blue) (color receiver8 blue) (color receiver9 blue) 
  (color receiver10 red) (color receiver11 red) (color receiver12 blue) 
  (color receiver13 red) (color receiver14 red)
  (controls receiver7 gate4) (controls receiver8 gate4) (controls receiver9 gears2) 
  (controls receiver10 gate5) (controls receiver11 gate5) (controls receiver12 gate6) 
  (controls receiver13 gate6) (controls receiver14 gears3)
  (separates barrier3 area1 area8) (separates barrier4 area1 area11)
  (separates gate4 area9 area10) (separates gate5 area10 area11) (separates gate6 area11 area12)
  (climbable> ladder3 area10 area8) (climbable> ladder4 area10 area11)

;los is from an area to a fixed station
  (los0 area1 transmitter2) (los0 area2 transmitter2)
  (los0 area1 receiver7) (los0 area8 receiver7)
  (los0 area1 gate4) (los0 area8 gate4) (los1 area11 gate5 gate4)    
  (los1 area11 gate5 receiver8)
  (los1 area9 gate4 receiver9) (los1 area11 gate5 receiver9)
  (los1 area9 gate4 receiver10)
  (los0 area1 gate5) (los1 area9 gate4 gate5) (los1 area12 gate6 gate5)
  (los0 area1 receiver11) (los1 area12 gate6 receiver11)
  (los2 area9 gate4 gate5 receiver12) (los1 area10 gate5 receiver12) (los1 area12 gate6 receiver12)
  (los2 area9 gate4 gate5 receiver13) (los1 area10 gate5 receiver13) (los1 area12 gate6 receiver13)
  (los1 area10 gate5 gate6)
  (los0 area1 receiver14) (los1 area10 gate5 receiver14) (los0 area11 receiver14)

;visibility is from an area to an area potentially containing a movable target or terminus
  (visible0 area1 area9) (visible0 area8 area2)
  (visible1 area1 gate4 area10) 
  (visible1 area8 gate4 area10) 
  (visible2 area9 gate4 gate5 area11)
  (visible1 area10 gate5 area1) (visible2 area10 gate5 gate6 area12) 
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
    (and (free me)
         (loc me area10)
         (loc jammer1 area1)
         (jamming jammer1 gate4)
         (inactive gate4)
         (loc connector3 area8)
         (connecting connector3 transmitter2)
         (color connector3 red)
         (active connector3)
         (loc connector4 area10)
         (connecting connector4 connector3)
         (connecting connector4 receiver10)
         (color connector4 red)
         (active connector4))
)

