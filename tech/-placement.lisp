;;; Filename: -placement.lisp

;;; Placement substrate: where a carried object may be set down -- a plate, a floor-mounted
;;; fan, a fixed floor/angled blower, a clear box top, another agent's currently-held tray,
;;; or bare ground -- gated by CLEARTOP for the object being placed, and by the agent's
;;; placement reach, which bounds how far
;;; above the agent a resting place may be but never how far below it.  A support already
;;; carrying an occupant of the other recorder layer still counts as clear: the layers are
;;; superimposed, so a live box may be set down on a plate a ghost box already stands on.  A fan qualifies as
;;; a support only while mounted on gears: a loose fan is mere cargo, like a connector, and
;;; a wall-mounted fan has no has-location.  A fixed floor/angled blower exposes the same
;;; flush support surface through its fixed position.  A tray qualifies as a support only
;;; while held: grounded, it is inert, so only currently-held trays are ever offered.  Shared by every carried-object
;;; technology that must choose where a held object comes to rest: box, jammer, beam-relay,
;;; and a fan mounted on floor-gears.  Declared identically by each until now; this file owns it
;;; once.  Mounting a fan on gears is an attachment, not a support placement, so it is
;;; -gears-fan's own mount-fan action rather than a case here.
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -vertical (top, location-elevation); -support-elevation
;;;               (within-agent-placement-reach); -support-occupancy
;;;               (support, on, cleartop); -location (has-location);
;;;               -position (has-position); -holding (cargo, holding);
;;;               -interaction-policy (object-manipulation-allowed,
;;;               support-use-allowed)
;;; PROVIDES:
;;;   queries   : placement-choice-allowed -- shared policy gate used by both option
;;;               generation and the placement update
;;;               placement-options  --  legal plate/fan/fixed-blower/box/tray/ground
;;;               placements at a location, excluding a given object (?self) as a
;;;               candidate support; only a floor-mounted fan and only a currently-held
;;;               tray are ever offered
;;;               placement-elevation -- the resting base elevation produced by an option
;;;   update    : place-held-object! -- releases the object at the chosen destination.
;;;               Local tray releases settle riders through -support-settling before
;;;               propagation; remote releases retain relocation and ground unloading.
;;;   nested    : -support-motion via -configuration-transition, shared surface
;;;               enumeration and stack relocation; -support-settling, release selection.

(include-tech -configuration-transition)
(include-tech -support-settling)
(include-tech -vertical)
(include-tech -support-elevation)
(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -holding)
(include-tech -interaction-policy)

(in-package :ww)


(define-optional-types fan floor-blower angled-blower)


(define-query placement-choice-allowed (?agent agent ?object cargo ?place)
  ;; ?place is either a support object or the Lisp marker GROUND.
  (and (object-manipulation-allowed ?agent ?object)
       (or (eql ?place 'ground)
           (and (support ?place)
                (support-use-allowed ?object ?place)))))


(define-query placement-options (?agent agent ?location location ?self cargo)
  (do (assign $places nil)
      (assign $surfaces (physical-supports-at ?location t))
      (ww-loop for $surface in $surfaces
        do (if (and (not (support-dependent-p $surface ?self nil))
                    (cleartop $surface ?self)
                    (placement-choice-allowed ?agent ?self $surface)
                    (within-agent-placement-reach ?agent (top $surface)))
             (push $surface $places)))
      (if (and (placement-choice-allowed ?agent ?self 'ground)
               (within-agent-placement-reach ?agent (location-elevation ?location)))
        (push 'ground $places))
      $places))


(define-query placement-elevation (?location location ?place)
  ;; ?place is either a support object or the Lisp marker GROUND.  This is the base level
  ;; an object will have after PLACE-HELD-OBJECT!, before its own height is added.
  (if (eql ?place 'ground)
    (location-elevation ?location)
    (top ?place)))


(define-update place-held-object!
    (?agent agent ?object cargo ?location location ?place)
  ;; Snapshot before releasing HOLDING: BASE must still see the original support.
  ;; Remote releases retain legacy unloading; only local releases select catches.
  (if (placement-choice-allowed ?agent ?object ?place)
    (do (assign $riders nil)
        (assign $local nil)
        (if (tray ?object)
          (do (bind (has-location ?agent $holder-location))
              (assign $local (eql $holder-location ?location))
              (assign $riders (tray-release-riders ?object $local))))
        (not (holding ?agent ?object))
        (has-location ?object ?location)
        (if (tray ?object)
          (relocate-tray-and-riders! ?object ?location))
        (ww-loop for $entry in $riders
          do (assign $rider (first $entry))
             (not (on $rider ?object)))
        (if (not (eql ?place 'ground)) (on ?object ?place))
        (if $local (settle-released-riders! $riders ?location)))
    (inconsistent-state)))
