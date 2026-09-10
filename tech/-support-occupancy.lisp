;;; Filename: -support-occupancy.lisp

;;; Support occupancy substrate: what rests on the top of a support, and whether a
;;; prospective occupant may join it there.  This file owns the support-occupant type
;;; composition and the (on $support-occupant $support) relation.
;;;
;;; A support holds at most one occupant per interaction layer, not at most one occupant.
;;; Ordinarily there is only one layer, so that reduces to the familiar single occupant.
;;; With a recorder running, the live and ghost copies of the world are superimposed --
;;; a live box and a ghost box may stand on the same plate, and START-RECORDER's fork
;;; creates exactly that whenever a live occupant rests on a support both layers share.
;;; ON is therefore NOT bijective: the occupant is the key and the support the value, so
;;; each occupant still rests on at most one support, but a support may carry several
;;; occupants and the reverse direction is a scan rather than an index lookup.
;;;
;;; The two questions a caller can ask are kept apart, because they have different answers
;;; once two layers exist:
;;;   SUPPORT-OCCUPIED  --  is anything at all resting here.  This is the physical
;;;                         question, asked by consequences that do not care whose object
;;;                         it is: a plate is depressed by a ghost's weight as surely as by
;;;                         a live one.
;;;   CLEARTOP          --  may this particular occupant take the top.  This is the
;;;                         manipulation question, asked by every action that mounts,
;;;                         places, lands, or lifts.  Contention is decided by
;;;                         -INTERACTION-POLICY's SUPPORT-OCCUPANCY-CONFLICT-P hook,
;;;                         which -RECORDER-CORE overrides.
;;;
;;; REQUIRES:
;;;   type     : support
;;; PROVIDES:
;;;   types    : support-occupant (either agent box jammer connector fan tray)
;;;              support (either pressure-plate toggle-plate box fan tray floor-blower
;;;              angled-blower).  Gears are not a support: only a fan can occupy them, via
;;;              -gears-fan's (mounted-on ...) attachment rather than (on ...).  A tray is
;;;              a support only while held; on the ground it is inert (see -vertical's
;;;              BASE, which gives a held tray its holder's top and a grounded one its own
;;;              location's level)
;;;   relation : (on $support-occupant $support)  --  the support an occupant rests on,
;;;              absent if it rests on the ground
;;;   nested   : -interaction-policy (neutral support-use-allowed and
;;;              support-occupancy-conflict-p hooks)
;;;   queries  : support-occupied, cleartop

(include-tech -plate-types)
(include-tech -interaction-policy)
(include-tech -physical-init-checks)

(in-package :ww)


(define-types
  support-occupant (either agent box jammer connector fan tray)
  support
    (either pressure-plate toggle-plate box fan tray floor-blower angled-blower))  ;fixed floor/angled blowers expose the same flush support surface as a mounted fan


(define-dynamic-relations
  (on support-occupant $support))  ;keyed by occupant: one support each, but a support may carry one occupant per layer


(define-query support-occupied (?support support)
  ;; Physical occupancy, layer-blind.  Read by consequences that respond to weight or
  ;; obstruction rather than to whose object it is.
  (exists (?occupant support-occupant)
    (on ?occupant ?support)))


(define-query cleartop (?support support ?occupant support-occupant)
  ;; True when ?support's top is available to ?occupant -- nothing already resting there
  ;; contends with it for the same physical space.  ?occupant is the object whose layer
  ;; decides contention: the one arriving, for a mount, a placement, or a landing; the one
  ;; being lifted, for a pickup, since it is same-layer weight on its own top that pins it.
  ;; An object never blocks itself.
  (not (exists (?other support-occupant)
         (and (different ?other ?occupant)
              (on ?other ?support)
              (support-occupancy-conflict-p ?occupant ?other)))))
