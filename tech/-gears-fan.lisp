;;; Filename: -gears-fan.lisp

;;; Gears/fan substrate: the shared machinery every blower technology programs against,
;;; modeled on -beam-substrate's peer-substrate pattern.  A removable blower consists of
;;; a carryable fan mounted on fixed gears.  A fixed combined unit is instead one
;;; floor-blower, wall-blower, or angled-blower object, unified by the blower type.  The
;;; three gears leaves remain mountable and are unified separately as gears.  Control
;;; follows gate.lisp's DNF convention via -controls, and the derived state is uniform:
;;; each gears/blower drive turns when uncontrolled or control-on; a removable fan blows
;;; iff mounted on a turning drive, and a fixed blower blows iff it turns.  What blowing
;;; *does* is mounting-specific, so this file's update-blower-status! derives turning/blowing
;;; only; each mounting behavior owns its own consequences update (-floor-blowing's
;;; update-floor-blowing-status! launches and drops, wall-blower's
;;; update-wall-blower-status! sweeps, angled-blower's update-angled-blower-status!
;;; launches along the arc), called after it.
;;;
;;; A floor- or angled-mounted fan is a floor object: mount-fan gives it a has-location,
;;; so it is steppable (step.lisp) and a placement target (-placement) at its gears'
;;; location.  A wall-mounted fan hangs on the wall with NO has-location: every
;;; has-location bind in step, placement, and the resting-fan pickup branch fails for it
;;; automatically, so nothing can stand or rest on it, and pickup-fan reaches it through
;;; its own wall-mounted branch instead (vertical reach to blower-elevation).
;;;
;;; REQUIRES:
;;;   types     : agent, location
;;;   nested    : -vertical (top, location-elevation); -propagation (derived-state driver);
;;;               -support-occupancy ((on ...), cleartop); -location ((has-location ...));
;;;               -position ((has-position ...)); -elevation ((has-elevation ...));
;;;               -controls (control-on; its controls relation accepts every drive type);
;;;               -placement (placement-options, place-held-object!; nests
;;;               -support-elevation and -holding); -reachability (reachable);
;;;               -pickup (pickup-clear); -recording-shadow-policy (neutral state-view
;;;               hooks, overridden by recorder's capability-specific shadows);
;;;               -recorder-fork-registry (mounted-on fact fork)
;;;   conditional relations:
;;;               jamming (jammer), guarded by an exists over jammer (gate.lisp's pattern):
;;;               a jamming jammer forces a drive stopped in update-blower-status!; jammer is
;;;               declared optional here, so a problem with no jammers need not declare it
;;;   substrate edits (companions to this file; these files splice before this file's
;;;   gears union is installed, so they reference the leaf types directly):
;;;               -controls          : (controls $list (either gate floor-gears wall-gears
;;;                                    angled-gears floor-blower wall-blower
;;;                                    angled-blower) $mode)
;;;               -position          : fixed-position-object (either pressure-plate
;;;                                    toggle-plate ladder floor-gears wall-gears
;;;                                    angled-gears floor-blower wall-blower angled-blower)
;;;               -elevation         : elevated-object includes wall-gears and wall-blower;
;;;                                    floor and angled drives are flush
;;;               -support-occupancy : support includes floor-blower and angled-blower;
;;;                                    mountable gears are NOT supports -- only their
;;;                                    attached fan can be occupied
;;;               -location          : mobile-object includes fan
;;;               -holding           : cargo includes fan
;;;               -support-elevation : a fan is a movable, zero-thickness support
;;;               step.lisp          : steppable-object includes fan, floor-blower, and
;;;                                    angled-blower; removable fans read mounted-on
;;;   driver    : the master propagate-consequences! must call update-blower-status!
;;;               after update-receiver-status! and update-plate-status!, and each
;;;               included mounting tech's consequences update after that
;;; PROVIDES:
;;;   types     : floor-gears, wall-gears, angled-gears, floor-blower, wall-blower,
;;;               angled-blower, fan -- declared optional here
;;;               gears (either floor-gears wall-gears angled-gears)
;;;               blower (either floor-blower wall-blower angled-blower)
;;;   relations : (aimed-at (either gears blower) $location) -- fixed stream destination
;;;               (mounted-on fan $gears)  --  the fan's attachment to its gears
;;;               (turning (either gears blower)) -- derived by update-blower-status!
;;;               (blowing (either fan floor-blower wall-blower angled-blower)) -- derived
;;;   init checks: gears-fan-init-check -- every drive whose mounting physics is installed
;;;               has distinct has-position source and aimed-at destination locations;
;;;               the fluent relation keys reject multiples
;;;               gears-fan-mounting-init-check -- each gears holds at most one fan,
;;;               and mounted fan holding/location/support state matches its mounting
;;;   query     : blower-drive -- mounted fan's gears, or a fixed blower itself
;;;               blower-present -- whether a drive has a removable or built-in fan
;;;               blower-elevation -- the working height of a mounted or fixed blower
;;;               declared has-elevation or 1 (matching transmitter/receiver anchors);
;;;               floor/angled drives use their floor elevation
;;;               blower-turning-for-object -- ordinary turning state except that recorder
;;;               ghosts use recording-side state for each recorder-supported blower drive
;;;               blower-active-for-object -- presence plus the correct turning view
;;;   updates   : update-blower-status! (state only)
;;;   shared    : -support-motion (nested through -placement) owns dependency checks,
;;;               relocate-stack!, landing-support, and land-on-support!. Blower
;;;               landings retain their support kinds, ordering, and height contract.
;;;   actions   : pickup-fan, put-fan, mount-fan

(include-tech -vertical)
(include-tech -propagation)
(include-tech -support-occupancy)
(include-tech -location)
(include-tech -position)
(include-tech -elevation)
(include-tech -controls)
(include-tech -placement)
(include-tech -reachability)
(include-tech -pickup)
(include-tech -recording-shadow-policy)
(include-tech -recorder-fork-registry)

(in-package :ww)


(define-optional-types
  floor-gears wall-gears angled-gears
  floor-blower wall-blower angled-blower fan jammer)


(define-types
  gears (either floor-gears wall-gears angled-gears)
  blower (either floor-blower wall-blower angled-blower))


(define-dynamic-relations
  (mounted-on fan $gears)  ;the fan's attachment to its gears; not an (on ...) support fact
  (turning (either floor-gears wall-gears angled-gears
                   floor-blower wall-blower angled-blower))
  (blowing (either fan floor-blower wall-blower angled-blower)))


;; MOUNTED-ON's contribution to the recorder's ghost fork, registered here because this
;; file owns the relation.  The gears are never themselves mapped.
(register-recorder-fork-clause 'mounted-on
  '(doall (?live fan)
     (if (bind (recording-copy> ?live $ghost))
       (if (bind (mounted-on ?live $gears))
         (mounted-on $ghost $gears)))))


(define-derived-relations
  turning
  blowing)


(define-static-relations
  (aimed-at (either floor-gears wall-gears angled-gears
                    floor-blower wall-blower angled-blower)
            $location))


(define-init-check gears-fan-init-check (literals)
  (check-init-blower-drive-endpoint-completeness literals))


(define-init-check gears-fan-mounting-init-check (literals)
  (check-init-mounted-fan-consistency literals))


(define-init-check-helper check-init-blower-drive-endpoint-completeness (literals)
  "Require every drive with installed mounting physics to name distinct endpoints."
  (dolist (drive (append (init-type-instances 'floor-gears)
                         (init-type-instances 'wall-gears)
                         (init-type-instances 'angled-gears)
                         (init-type-instances 'floor-blower)
                         (init-type-instances 'wall-blower)
                         (init-type-instances 'angled-blower)))
    (when (init-blower-drive-physics-installed-p drive)
      (let ((source-literal
              (init-blower-drive-relation-literal 'has-position drive literals))
            (destination-literal
              (init-blower-drive-relation-literal 'aimed-at drive literals)))
        (unless source-literal
          (fail-init-check
            nil
            "~%Blower drive ~S has no HAS-POSITION source.~%~
             Every installed floor, wall, or angled drive must name exactly one source location."
            drive))
        (unless destination-literal
          (fail-init-check
            nil
            "~%Blower drive ~S has no AIMED-AT destination.~%~
             Every installed floor, wall, or angled drive must name exactly one destination."
            drive))
        (when (eql (third (init-literal-proposition source-literal))
                   (third (init-literal-proposition destination-literal)))
          (fail-init-check
            destination-literal
            "~%Blower drive ~S has the same source and destination location: ~S.~%~
             A floor, wall, or angled stream must lead away from its source."
            drive
            (third (init-literal-proposition source-literal))))))))


(define-init-check-helper init-blower-drive-physics-installed-p (drive)
  (or (and (or (init-type-member-p drive 'floor-gears)
               (init-type-member-p drive 'floor-blower))
           (member 'update-floor-blowing-status! *update-names*))
      (and (or (init-type-member-p drive 'wall-gears)
               (init-type-member-p drive 'wall-blower))
           (member 'update-wall-blower-status! *update-names*))
      (and (or (init-type-member-p drive 'angled-gears)
               (init-type-member-p drive 'angled-blower))
           (member 'update-angled-blower-status! *update-names*))))


(define-init-check-helper init-blower-drive-relation-literal (relation drive literals)
  (find drive
        (positive-init-literals-with-relation relation literals)
        :test #'eql
        :key (lambda (literal)
               (second (init-literal-proposition literal)))))


(define-init-check-helper init-blower-drive-related-location (relation drive literals)
  (let ((literal (init-blower-drive-relation-literal relation drive literals)))
    (when literal
      (third (init-literal-proposition literal)))))


(define-init-check-helper check-init-mounted-fan-consistency (literals)
  "Require authored MOUNTED-ON facts to match the state produced by MOUNT-FAN."
  (let ((locations (init-literal-map 'has-location literals 1 2))
        (positions (init-literal-map 'has-position literals 1 2))
        (on-map (init-literal-map 'on literals 1 2))
        (held-objects (init-held-objects literals))
        (mounted-drives (make-hash-table :test #'eql)))
    (dolist (literal (positive-init-literals-with-relation 'mounted-on literals))
      (destructuring-bind (fan drive) (rest (init-literal-proposition literal))
        (let ((existing-fan (gethash drive mounted-drives))
              (source (gethash drive positions))
              (fan-location (gethash fan locations)))
          (when existing-fan
            (fail-init-check
              literal
              "~%DEFINE-INIT mounts multiple fans on one blower drive.~%~
               Drive:      ~S~%First fan:  ~S~%Second fan: ~S"
              drive existing-fan fan))
          (setf (gethash drive mounted-drives) fan)
          (when (gethash fan held-objects)
            (fail-init-check
              literal
              "~%DEFINE-INIT fan is both held and mounted.~%~
               Fan: ~S~%Drive: ~S"
              fan drive))
          (when (gethash fan on-map)
            (fail-init-check
              literal
              "~%DEFINE-INIT fan is both mounted and resting ON a support.~%~
               Fan: ~S~%Drive: ~S"
              fan drive))
          (unless source
            (fail-init-check
              literal
              "~%DEFINE-INIT mounts a fan on gears with no HAS-POSITION source.~%~
               Fan: ~S~%Drive: ~S"
              fan drive))
          (if (or (init-type-member-p drive 'wall-gears)
                  (init-type-member-p drive 'wall-blower))
            (when fan-location
              (fail-init-check
                literal
                "~%DEFINE-INIT wall-mounted fan must not have HAS-LOCATION.~%~
                 Fan: ~S at ~S~%Drive: ~S at ~S"
                fan fan-location drive source))
            (if (not fan-location)
              (fail-init-check
                literal
                "~%DEFINE-INIT floor/angled-mounted fan must have HAS-LOCATION.~%~
                 Fan: ~S~%Drive: ~S at ~S"
                fan drive source)
              (unless (eql fan-location source)
                (fail-init-check
                  literal
                  "~%DEFINE-INIT mounted fan location does not match its drive location.~%~
                   Fan: ~S at ~S~%Drive: ~S at ~S"
                  fan fan-location drive source)))))))))


(define-query blower-drive
    (?source (either fan floor-blower wall-blower angled-blower))
  ;; A removable fan delegates to its current mount; a fixed blower is its own drive.
  (if (fan ?source)
    (do (bind (mounted-on ?source $gears))
        $gears)
    ?source))


(define-query blower-present
    (?drive (either floor-gears wall-gears angled-gears
                    floor-blower wall-blower angled-blower))
  (or (floor-blower ?drive)
      (wall-blower ?drive)
      (angled-blower ?drive)
      (exists (?fan fan)
        (and (bind (mounted-on ?fan $gears))
             (eql $gears ?drive)))))


(define-query blower-elevation
    (?drive (either floor-gears wall-gears angled-gears
                    floor-blower wall-blower angled-blower))
  ;; Wall drives use their declared stream elevation, defaulting to 1.  Floor and angled
  ;; drives are flush and use the floor elevation of their fixed position.
  (if (or (wall-gears ?drive)
          (wall-blower ?drive))
    (if (bind (has-elevation ?drive $level))
      $level
      1)
    (do (bind (has-position ?drive $location))
        (location-elevation $location))))


(define-query blower-turning-for-object
    (?object
     ?drive (either floor-gears wall-gears angled-gears
                    floor-blower wall-blower angled-blower))
  (if (recording-shadow-object ?object)
    (recording-shadow-turning ?drive)
    (turning ?drive)))


(define-query blower-active-for-object
    (?object
     ?drive (either floor-gears wall-gears angled-gears
                    floor-blower wall-blower angled-blower))
  (and (blower-present ?drive)
       (blower-turning-for-object ?object ?drive)))


(define-update update-blower-status! ()
  ;; Pass 1: turning <=> control-on AND not jammed, with -controls' shared CONTROL-ON
  ;; supplying the DNF aggregate and a T uncontrolled default, so drives nothing controls
  ;; turn all the time.  A jamming jammer forces the drive stopped -- the polarity mirror of
  ;; gate's jam-forces-open: a jam always disables the barrier.  Pass 2 derives removable
  ;; and fixed blowing sources from their drives.  Pure state derivation only: what blowing does
  ;; (launching, sweeping, dropping) is
  ;; mounting-specific and owned by each mounting tech's own consequences update, which
  ;; the driver calls after this one.  Change detection is automatic, so an unchanged
  ;; re-assert is silent.
  (do (doall (?drive (either floor-gears wall-gears angled-gears
                              floor-blower wall-blower angled-blower))
        (if (and (control-on ?drive t)
                 (not (exists (?j jammer)
                        (jamming ?j ?drive))))
          (turning ?drive)
          (not (turning ?drive))))
      (doall (?f fan)
        (if (and (bind (mounted-on ?f $gears))
                 (turning $gears))
          (blowing ?f)
          (not (blowing ?f))))
      (doall (?fixed blower)
        (if (turning ?fixed)
          (blowing ?fixed)
          (not (blowing ?fixed))))))


(define-action pickup-fan
  ;; Pick up a clear fan within reach.  Two cases: a fan with a has-location (resting on
  ;; a support or ground, or mounted on floor or angled gears) goes through the ordinary
  ;; pickup-clear path; a wall-mounted fan has no has-location, so it is reached through
  ;; its gears' position and stream elevation instead, and nothing can rest on it.
  ;; Occupied fans (like occupied boxes) cannot be lifted, so an agent standing on a fan
  ;; can never carry it away.  Fixed combined blowers are a different type and cannot bind
  ;; this action.  Lifting a fan off turning gears is allowed -- the fan itself is never
  ;; blown -- and propagation clears its blowing status and applies mounting consequences.
  1
  (?agent agent ?fan fan)
  (and (bind (has-location ?agent $a-location))
       (or (and (bind (has-location ?fan $fan-location))
                (cleartop ?fan ?fan)
                (pickup-clear ?agent $a-location ?fan $fan-location))
           (and (bind (mounted-on ?fan $w-gears))
                 (wall-gears $w-gears)
                 (object-manipulation-allowed ?agent ?fan)
                 (not (bind (holding ?agent $any-held)))
                (bind (has-position $w-gears $fan-location))
                (reachable $fan-location $a-location)
                (within-agent-vertical-reach ?agent (blower-elevation $w-gears)))))
  (">" ?agent "picks up" ?fan "at" $fan-location "from" $a-location)
  (assert (holding ?agent ?fan)
          (if (bind (has-location ?fan $f-location))
            (not (has-location ?fan $f-location)))
          (if (bind (on ?fan $support))
            (not (on ?fan $support)))
          (if (bind (mounted-on ?fan $gears))
            (not (mounted-on ?fan $gears)))
          (finally (propagate-changes!))))


(define-action put-fan
  ;; Place a held fan on the ground or on a clear support at a reachable location
  ;; (including the agent's own): one successor per legal placement-options result.
  ;; Mounting on gears is the separate mount-fan action, since gears are not a support.
  1
  (?agent agent ?fan fan ?location location)
  (and (holding ?agent ?fan)
       (bind (has-location ?agent $a-location))
       (reachable ?location $a-location)
       (assign $places (placement-options ?agent ?location ?fan)))
  (">" ?agent "puts" ?fan "on" $place "at" ?location)
  (ww-loop for $placement-option in $places
           do (assert (assign $place $placement-option)
                      (place-held-object! ?agent ?fan ?location $placement-option)
                      (finally (propagate-changes!)))))


(define-action mount-fan
  ;; Attach a held fan to vacant gears at a reachable location, within vertical reach of
  ;; the gears' working height.  A floor or angled mount makes the fan a floor object at
  ;; the gears' location; a wall mount hangs it with no has-location.  Mounting on
  ;; already-turning gears is legal; the empty fan just starts blowing.
  1
  (?agent agent ?fan fan ?gears gears)
  (and (holding ?agent ?fan)
       (object-manipulation-allowed ?agent ?fan)
       (bind (has-location ?agent $a-location))
       (bind (has-position ?gears $g-location))
       (reachable $g-location $a-location)
       (not (exists (?f fan)
              (and (bind (mounted-on ?f $g))
                   (eql $g ?gears))))
       (within-agent-vertical-reach ?agent (blower-elevation ?gears)))
  (">" ?agent "mounts" ?fan "on" ?gears "at" $g-location)
  (assert (not (holding ?agent ?fan))
          (if (or (floor-gears ?gears)
                  (angled-gears ?gears))
            (has-location ?fan $g-location))
          (mounted-on ?fan ?gears)
          (finally (propagate-changes!))))
