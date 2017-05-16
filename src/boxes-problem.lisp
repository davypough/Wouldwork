;;;; Filename: boxes-problem.lisp

;;; Problem specification for using boxes to move to an area through a sequence of gates
;;; controlled by pressure plates. See diagram in user manual.


(in-package :pl)  ;required


(setq *depth_cutoff* 10)


(define-types
  myself    (me)
  box       (box1 box2)
  gate      (gate1 gate2 gate3)
  plate     (plate1 plate2 plate3)
  area      (area1 area2 area3 area4)
  object    (either myself box plate))


(define-base-relations
  (holding myself box)
  (loc (either myself box plate) area)
  (on box plate)
  (controls plate gate)
  (separates gate area area))



(define-monitored-relations
  holding loc on)


(define-derived-relations
  (free> me)                    (not (exists (?b box) 
                                       (holding me ?b)))
  
  (cleartop> ?plate)            (not (exists (?b box)
                                       (on ?b ?plate)))

  (open> ?gate ?area1 ?area2)   (and (separates ?gate ?area1 ?area2)
                                     (exists (?p plate)
                                       (and (controls ?p ?gate)
                                            (exists (?b box)
                                              (on ?b ?p))))))


(define-action move
    1
  ((?area1 ?area2) area)
  (and (loc me ?area1)
       (exists (?g gate)
         (open> ?g ?area1 ?area2)))
  ((?area1 ?area2) area)
  (and (not (loc me ?area1))
       (loc me ?area2)))


(define-action pickup
    1
  (?box box ?area area)
  (and (loc me ?area)
       (loc ?box ?area)
       (free> me))
  (?box box ?plate plate ?area area)
  (if (on ?box ?plate)
      (and (not (on ?box ?plate))
           (not (loc ?box ?area))
           (holding me ?box))
      (if (not (exists (?p plate)
                 (on ?box ?p)))
          (and (not (loc ?box ?area))
               (holding me ?box)))))


(define-action drop
    1
  (?box box ?area area)
  (and (loc me ?area)
       (holding me ?box))
  (?box box ?plate plate ?area area)
  (and (loc ?box ?area)
       (not (holding me ?box))
       (if (loc ?plate ?area)
           (on ?box ?plate))))


(define-init
  ;dynamic
  (loc me area1)
  (loc box1 area1)    
  (loc box2 area2)
  ;static
  (loc plate1 area1)
  (loc plate2 area1)
  (loc plate3 area3)
  (controls plate1 gate1)
  (controls plate2 gate2)
  (controls plate3 gate3)
  (separates gate1 area1 area2)
  (separates gate2 area1 area3)
  (separates gate3 area3 area4))


(define-goal
  (loc me area4))
