;;; Filename: -reachability-coordinates.lisp

;;; Reachability coordinates substrate: derives the symmetric REACH-VIA manipulation edges
;;; from raw segment geometry, for a problem that would rather author 2D positions than
;;; hand-list which locations can be manipulated from which.  Nested under public
;;; reachability; entirely inert unless the problem actually asserts WALL-SEGMENT>,
;;; EDGE-SEGMENT>, or BOUNDARY-WALL -- a problem that hand-authors its reach edges directly
;;; is unaffected, and so is one that carries no coordinates at all.
;;;
;;; Reach is a line-of-sight question, not a walking one, and the derivation follows the
;;; LOS conventions rather than -walkability-coordinates' region-adjacency ones.  An arm
;;; travels in a straight line between two points: it is stopped by a wall, an edge, or the
;;; boundary silhouette; it passes through a window exactly as sight does, since a window is
;;; an opening rather than a partition; and it is stopped outright by a screen, which
;;; REACHABLE-CLEAR can never clear.  Every gate the line crosses becomes a barrier in the
;;; emitted fact's conjunctive list, so the derived edge carries the same open-every-gate
;;; condition an authored one would.
;;;
;;; Two bounds decide whether a pair reaches at all.  The horizontal one is
;;; *HORIZONTAL-REACH-LIMIT*, a static bound on how far apart two points may be for a reach
;;; edge to exist between them.  It is deliberately NOT the vertical limit, and the two are
;;; not the same quantity: -support-elevation's *VERTICAL-REACH-LIMIT* is an agent
;;; capability applied at action time, measured by WITHIN-AGENT-VERTICAL-REACH from wherever
;;; the actor currently stands -- including on top of a box it has just climbed -- while this
;;; one is a fact about the map that is fixed before the search begins.  Folding the level
;;; difference into the horizontal test here would freeze at authoring time a judgement the
;;; vertical tests are built to make from the actor's live elevation.
;;;
;;; The second bound is the ledge exception, and it is what makes reach over a raised slab
;;; work.  A solid blocks only when its TOP rises above the higher of the two endpoints'
;;; levels: an actor standing on a platform reaches down over the vertical face of the
;;; platform it is standing on, and an actor on the ground does not reach through that same
;;; face.  This is the same shape as -walkability-coordinates' supported-doorway rule, and
;;; for the same reason -- a planar footprint says nothing on its own about which side of it
;;; the arm passes.  A gate or screen is judged by span overlap instead: one sitting entirely
;;; above or below the reach line's own level range is simply not in the way.
;;;
;;; The derivation is additive.  A pair the problem has already authored, in REACH-VIA or in
;;; either direction of REACH-VIA>, is left exactly as authored and never restated; an
;;; authored fact may therefore reach further than the limit, or through geometry the
;;; derivation reads as blocked, and it will still stand.  REACH-VIA> itself is never
;;; derived: a one-way opening -- a letterbox slot, a recess whose mouth faces one side -- is
;;; not a consequence of planar geometry, which is symmetric, and remains authored.
;;;
;;; REACH-DISALLOWED> subtracts.  Geometry decides what is physically possible, and that is
;;; a superset of what a puzzle wants: two locations a single step apart on open floor
;;; genuinely are within arm's length of each other, and saying so can hand the search a
;;; shortcut the problem was designed to withhold.  The relation is authored directionally,
;;; like JAM-DISALLOWED>, and read symmetrically, because the fact it suppresses is itself
;;; symmetric.  A disallowal that suppresses nothing is an error rather than a no-op: the
;;; pair it named has drifted out of range, behind a solid, or into an authored REACH-VIA,
;;; and the guard it was placed to provide is silently gone.
;;;
;;; Self-contained; spliced by (include-tech -reachability-coordinates), nested from
;;; reachability.
;;;
;;; REQUIRES:
;;;   types     : location  --  declared by the problem; switch optional, declared by
;;;               nested -reachability alongside the REACH-TARGET union
;;;   nested    : -reachability (reach-target); -location-coordinates (LOCATION-COORDS>);
;;;               -apparatus-coordinates (APPARATUS-COORDS>); -segment-geometry (the named
;;;               segment relations and *BOUNDARY-WALL-HEIGHT*); -vertical (BASE, TOP,
;;;               LOCATION-ELEVATION)
;;; PROVIDES:
;;;   parameter : *horizontal-reach-limit*, default 5/2 -- the greatest horizontal
;;;               separation across which a derived reach edge may exist.  A problem may
;;;               override it with DEFPARAMETER after including its parent tech.
;;;   relations : (reach-via reach-target $list reach-target),
;;;               (reach-via> location $list reach-target),
;;;               (reach-disallowed> reach-target reach-target)
;;;   queries   : reachability-coordinates-target-points, -solid-records, -gate-records,
;;;               -screen-records, -authored, -emittable, -unused-disallowals
;;;   init      : derive-reach-from-segments

(include-tech -reachability)
(include-tech -location-coordinates)
(include-tech -apparatus-coordinates)
(include-tech -segment-geometry)
(include-tech -vertical)

(in-package :ww)


(defparameter *horizontal-reach-limit* 5/2
  "Greatest horizontal separation across which a derived REACH-VIA edge may exist.  Compared
   squared, so the value may be any exact rational without ever introducing a float.")


(define-static-relations
  (reach-via reach-target $list reach-target)  ;symmetric manipulation reach; barriers must be open
  (reach-via> location $list reach-target)  ;directional reach, reacher's location first
  (reach-disallowed> reach-target reach-target))  ;authored suppression of one derived pair


;;;; DERIVATION CORE ;;;;
;;;; Plain Lisp functions operating on point and segment records passed as arguments -- no
;;;; live database access, so no WW query wrapper is needed for these.  High-level first.


(defun reachability-coordinates-pair-spec (point-a point-b solids screens gates limit)
  "Return (:reach BARRIERS) when a manipulation reach runs between POINT-A and POINT-B, or
   NIL when none does.  Each point is a (name x y level) record from
   REACHABILITY-COORDINATES-TARGET-POINTS; SOLIDS, SCREENS and GATES are
   (name x1 y1 x2 y2 base top) records.  BARRIERS is the gate names the line crosses, sorted
   by name so the derived fact set is deterministic and diffable.
   A solid blocks only when it rises above the higher endpoint -- see the file header on the
   ledge exception -- while a screen or gate must additionally have its own vertical span
   meet the range the two endpoints bracket."
  (let ((x1 (second point-a))
        (y1 (third point-a))
        (x2 (second point-b))
        (y2 (third point-b))
        (high (max (fourth point-a) (fourth point-b)))
        (low (min (fourth point-a) (fourth point-b))))
    (when (and (reachability-coordinates-within-limit-p x1 y1 x2 y2 limit)
               (notany (lambda (solid)
                         (and (> (seventh solid) high)
                              (reachability-coordinates-crosses-p x1 y1 x2 y2 solid t)))
                       solids)
               (notany (lambda (screen)
                         (and (reachability-coordinates-span-meets-p screen low high)
                              (reachability-coordinates-crosses-p x1 y1 x2 y2 screen nil)))
                       screens))
      (list :reach
            (sort (loop for gate in gates
                        when (and (reachability-coordinates-span-meets-p gate low high)
                                  (reachability-coordinates-crosses-p x1 y1 x2 y2 gate nil))
                          collect (first gate))
                  #'string<
                  :key #'symbol-name)))))


(defun reachability-coordinates-within-limit-p (x1 y1 x2 y2 limit)
  "True when the horizontal separation between the two points is at most LIMIT.  Compared
   squared on purpose: a square root would turn every authored fraction into a float, and the
   derived fact set would then depend on rounding rather than on the authored coordinates."
  (<= (+ (* (- x2 x1) (- x2 x1))
         (* (- y2 y1) (- y2 y1)))
      (* limit limit)))


(defun reachability-coordinates-span-meets-p (record low high)
  "True when RECORD's own vertical span meets the level range the reach line brackets.  A
   door whose span sits entirely above the two endpoints, or entirely below them, stands in
   no relation to the arm passing between them and is neither a barrier nor a block."
  (and (<= (sixth record) high)
       (>= (seventh record) low)))


(defun reachability-coordinates-crosses-p (x1 y1 x2 y2 record endpoints-block)
  "True when the reach line from (X1 Y1) to (X2 Y2) meets RECORD's segment.  The reach line
   is always read strictly, so a target sitting exactly on a segment never counts as crossing
   it -- the walking derivation already rejects a location placed on or inside one, and every
   fixture is authored offset from the barrier it sits beside.  ENDPOINTS-BLOCK says whether
   RECORD's own endpoint counts, following the beam substrate's convention: a solid blocks at
   its corner, so a reach cannot thread the join between two of them, while a gate's corner
   belongs to the solid beside it and is left strict."
  (multiple-value-bind (along-reach along-segment)
      (reachability-coordinates-intersection-parameters
        x1 y1 x2 y2
        (second record) (third record) (fourth record) (fifth record))
    (and along-reach
         (< 0 along-reach 1)
         (if endpoints-block
           (<= 0 along-segment 1)
           (< 0 along-segment 1)))))


(defun reachability-coordinates-intersection-parameters (x1 y1 x2 y2 x3 y3 x4 y4)
  "The point where the infinite lines through (X1 Y1)-(X2 Y2) and (X3 Y3)-(X4 Y4) meet,
   returned as two parameters: the first measured along the first segment, the second along
   the second.  NIL when the two are parallel or collinear, the only case with no single
   meeting point.  Every value is an exact rational, so the caller's containment tests are
   exact too.
   A namespaced twin of -beam-los-coordinates' BEAM-COORDINATES-SEGMENT-INTERSECTION-
   PARAMETERS, duplicated for the reason -walkability-coordinates keeps its own boundary
   builder rather than borrowing that file's: reach must not drag the entire beam substrate
   -- transmitters, receivers, chroma, coupling -- in behind it to obtain eight lines of
   arithmetic."
  (let* ((dx1 (- x2 x1))
         (dy1 (- y2 y1))
         (dx2 (- x4 x3))
         (dy2 (- y4 y3))
         (offset-x (- x3 x1))
         (offset-y (- y3 y1))
         (denominator (- (* dx1 dy2) (* dy1 dx2))))
    (unless (zerop denominator)
      (values (/ (- (* offset-x dy2) (* offset-y dx2)) denominator)
              (/ (- (* offset-x dy1) (* offset-y dx1)) denominator)))))


(defun reachability-coordinates-boundary-records (points)
  "Convert an explicitly closed BOUNDARY-WALL point list into solid records, one per polygon
   edge.  Each is given the full *BOUNDARY-WALL-HEIGHT*, so the silhouette blocks reach at
   every level rather than becoming a ledge an actor on a raised slab could reach over."
  (loop for (point1 point2) on points
        while point2
        for edge-index from 1
        collect (list edge-index
                      (first point1) (second point1)
                      (first point2) (second point2)
                      0 *boundary-wall-height*)))


(defun reachability-coordinates-point (target points)
  "TARGET's own (name x y level) record from POINTS."
  (or (assoc target points)
      (error "No reach-target position is recorded for ~A." target)))


(defun report-unused-reach-disallowals (pairs)
  "Signal every REACH-DISALLOWED> pair the derivation never produces, together rather than
   one staging at a time.  A disallowal exists to withhold a reach the geometry would
   otherwise grant; one that withholds nothing is not harmless, because the pair it named has
   moved out of range, behind a solid, or into an authored REACH-VIA, and whatever it was
   placed to guard is now unguarded without anything saying so."
  (error "~%REACH-DISALLOWED> names ~D pair~:P the reach derivation does not produce:~%~%~
          ~{  ~S~%~}~%~
          Each pair above is already out of *HORIZONTAL-REACH-LIMIT*, blocked by a solid or ~
          a screen, or authored as REACH-VIA, so suppressing it changes nothing.  Remove the ~
          fact, or repair the coordinates it was written to guard."
         (length pairs) pairs))


;;;; QUERY FUNCTIONS ;;;;


(define-query reachability-coordinates-target-points ()
  ;; Every reach target's (name x y level), routed to its owning position relation exactly
  ;; as -beam-los-coordinates routes its own endpoints: LOCATION-COORDS> for a location and
  ;; APPARATUS-COORDS> for a switch.  The level comes from the vertical model rather than
  ;; from the coordinate's third slot directly, so LOCATION-ELEVATION's memo and its
  ;; overridable LOCATION-LEVEL seam are both honored.  A target with no position is an
  ;; authoring error in a problem carrying segment geometry, and is named here rather than
  ;; left to surface as a NIL inside the arithmetic downstream.
  (do (assign $points nil)
      (doall (?location location)
        (if (bind (location-coords> ?location $x $y))
          (do (assign $level (location-elevation ?location))
              (push (list ?location $x $y $level) $points))
          (error "No LOCATION-COORDS> is defined for location ~A." ?location)))
      (doall (?switch switch)
        (if (bind (apparatus-coords> ?switch $sx $sy))
          (do (assign $mounting (base ?switch))
              (push (list ?switch $sx $sy $mounting) $points))
          (error "No APPARATUS-COORDS> is defined for switch ~A." ?switch)))
      $points))


(define-query reachability-coordinates-solid-records ()
  ;; Walls and edges, each planar record extended with its own base and top, plus every
  ;; boundary polygon edge.  Windows are deliberately absent: a window is an opening in a
  ;; partition, and an arm passes through it exactly as sight does.
  (do (assign $records nil)
      (doall (?wall wall)
        (if (bind (wall-segment> ?wall $wx1 $wy1 $wx2 $wy2 $wz))
          (do (assign $wall-base (base ?wall))
              (assign $wall-top (top ?wall))
              (push (list ?wall $wx1 $wy1 $wx2 $wy2 $wall-base $wall-top) $records))))
      (doall (?edge edge)
        (if (bind (edge-segment> ?edge $ex1 $ey1 $ex2 $ey2 $ez))
          (do (assign $edge-base (base ?edge))
              (assign $edge-top (top ?edge))
              (push (list ?edge $ex1 $ey1 $ex2 $ey2 $edge-base $edge-top) $records))))
      (if (bind (boundary-wall $boundary-points))
        (assign $records
                (append (reachability-coordinates-boundary-records $boundary-points)
                        $records)))
      $records))


(define-query reachability-coordinates-gate-records ()
  ;; Gates in the same (name x1 y1 x2 y2 base top) shape the solids use, so one span test
  ;; and one crossing test serve both.  A crossed gate becomes a barrier rather than a block.
  (do (assign $records nil)
      (doall (?gate gate)
        (if (bind (gate-segment> ?gate $gx1 $gy1 $gx2 $gy2 $gz))
          (do (assign $gate-base (base ?gate))
              (assign $gate-top (top ?gate))
              (push (list ?gate $gx1 $gy1 $gx2 $gy2 $gate-base $gate-top) $records))))
      $records))


(define-query reachability-coordinates-screen-records ()
  ;; Screens block reach outright rather than conditioning it.  REACHABLE-CLEAR admits only
  ;; an open gate, so emitting a screen as a barrier would produce a permanently dead edge;
  ;; producing no edge at all says the same thing without the row.
  (do (assign $records nil)
      (doall (?screen screen)
        (if (bind (screen-segment> ?screen $cx1 $cy1 $cx2 $cy2 $cz))
          (do (assign $screen-base (base ?screen))
              (assign $screen-top (top ?screen))
              (push (list ?screen $cx1 $cy1 $cx2 $cy2 $screen-base $screen-top) $records))))
      $records))


(define-query reachability-coordinates-authored (?a reach-target ?b reach-target)
  ;; True when the problem itself has already stated how this pair reaches.  REACH-VIA
  ;; carries no ">" suffix, so WW mirrors it and a single bind sees either order; REACH-VIA>
  ;; is directional and is asked each way, guarded by its first argument's location type.  An
  ;; authored fact always wins: this derivation adds to a problem's own reach facts and never
  ;; restates, refines, or contradicts one.
  (or (bind (reach-via ?a $authored-symmetric ?b))
      (and (location ?a)
           (bind (reach-via> ?a $authored-forward ?b)))
      (and (location ?b)
           (bind (reach-via> ?b $authored-backward ?a)))))


(define-query reachability-coordinates-emittable (?a reach-target ?b reach-target)
  ;; Whether a pair the geometry admits should actually be asserted: not already authored,
  ;; and not withheld by REACH-DISALLOWED>.  Both tests live here, rather than as separate
  ;; branches at the emission site, so the init-action's update body needs no else-branch and
  ;; carries no state between iterations.
  ;; REACH-DISALLOWED> is authored directionally and read symmetrically, because the fact it
  ;; suppresses is itself symmetric: a one-way suppression of a two-way fact would have no
  ;; meaning.  A genuinely one-way opening is expressed by disallowing the pair here and
  ;; authoring REACH-VIA> for the direction that survives.
  (and (not (reachability-coordinates-authored ?a ?b))
       (not (reach-disallowed> ?a ?b))
       (not (reach-disallowed> ?b ?a))))


(define-query reachability-coordinates-unused-disallowals (?points ?solids ?screens ?gates)
  ;; Every authored REACH-DISALLOWED> pair that suppresses nothing, because the derivation
  ;; would not have produced it anyway.  Recomputing each pair's spec here rather than
  ;; accumulating suppressions during emission keeps the init-action's update body free of
  ;; cross-iteration state, and costs one extra spec per authored disallowal.  A pair named
  ;; in both orders is reported twice, which is itself worth seeing.
  (do (assign $unused nil)
      (doall (?a reach-target)
        (doall (?b reach-target)
          (if (and (reach-disallowed> ?a ?b)
                   (not (and (reachability-coordinates-pair-spec
                               (reachability-coordinates-point ?a ?points)
                               (reachability-coordinates-point ?b ?points)
                               ?solids ?screens ?gates *horizontal-reach-limit*)
                             (not (reachability-coordinates-authored ?a ?b)))))
            (push (list ?a ?b) $unused))))
      $unused))


;;;; INITIALIZATION ;;;;


(define-init-action derive-reach-from-segments
  ;; Derives the symmetric REACH-VIA manipulation edges from the problem's raw segment
  ;; geometry -- see the file header for the line-of-sight reading, the ledge exception, and
  ;; why REACH-VIA> is not derived.  Runs only when the problem has asserted WALL-SEGMENT>,
  ;; EDGE-SEGMENT>, or BOUNDARY-WALL, the same trigger -WALKABILITY-COORDINATES and
  ;; -BEAM-LOS-COORDINATES use -- inert otherwise, so a problem that hand-authors its own
  ;; reach edges, or carries no coordinates at all, is unaffected.
  ;;
  ;; Location pairs are visited once each, in type order, because REACH-VIA has no ">"
  ;; suffix and WW mirrors it; location/switch pairs need no such guard, the two types being
  ;; disjoint.  Switch/switch pairs are never considered: REACHABLE's second argument is
  ;; always the actor's own location, so a reach between two fixtures could never be read.
  ;;
  ;; The unused-disallowal check runs last, after every emission, so it sees the authored
  ;; REACH-VIA facts and the derived ones alike.  Ends with its own
  ;; CONVERT-DATABASES-TO-INTEGERS for the same reason DERIVE-WALKING-FROM-SEGMENTS does --
  ;; so the facts asserted here are visible to later BIND calls.
  0
  ()
  (or (exists (?wall wall)
        (bind (wall-segment> ?wall $x1 $y1 $x2 $y2 $z)))
      (exists (?edge edge)
        (bind (edge-segment> ?edge $x1 $y1 $x2 $y2 $z)))
      (bind (boundary-wall $trigger-boundary)))
  ()
  (assert
    (do (assign $points (reachability-coordinates-target-points))
        (assign $solids (reachability-coordinates-solid-records))
        (assign $screens (reachability-coordinates-screen-records))
        (assign $gates (reachability-coordinates-gate-records))
        (doall (?source location)
          (doall (?target location)
            (if (member ?target
                        (rest (member ?source (gethash 'location *types*))))
              (do (assign $spec (reachability-coordinates-pair-spec
                                  (reachability-coordinates-point ?source $points)
                                  (reachability-coordinates-point ?target $points)
                                  $solids $screens $gates *horizontal-reach-limit*))
                  (if $spec
                    (if (reachability-coordinates-emittable ?source ?target)
                      (reach-via ?source (second $spec) ?target)))))))
        (doall (?location location)
          (doall (?switch switch)
            (do (assign $switch-spec (reachability-coordinates-pair-spec
                                       (reachability-coordinates-point ?location $points)
                                       (reachability-coordinates-point ?switch $points)
                                       $solids $screens $gates *horizontal-reach-limit*))
                (if $switch-spec
                  (if (reachability-coordinates-emittable ?location ?switch)
                    (reach-via ?location (second $switch-spec) ?switch))))))
        (assign $unused (reachability-coordinates-unused-disallowals
                          $points $solids $screens $gates))
        (if $unused
          (report-unused-reach-disallowals $unused))
        (convert-databases-to-integers))))
