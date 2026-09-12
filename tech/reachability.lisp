;;; Filename: reachability.lisp

;;; Reachability background capability: whether a location or fixed switch is within
;;; manipulation reach of a location.  Two endpoints are in reach iff identical, or an
;;; edge joins them with every barrier gate open.
;;;
;;; The reach edges themselves may be hand-authored or derived.  -reachability-coordinates
;;; owns both the relations and the coordinate derivation that fills them, the way
;;; -traversal owns TRAVERSE-VIA for walkability: a problem carrying raw segment geometry
;;; gets its REACH-VIA edges computed from that geometry and subtracts the ones its design
;;; withholds with REACH-DISALLOWED>, while a problem carrying no geometry authors every
;;; edge itself and the derivation stays inert.  This file owns what reach *means* once the
;;; edges exist -- the query, its barrier predicate, and the barrier-type validation.
;;;
;;; REQUIRES:
;;;   types     : location
;;;   nested    : -reachability (identity-default reachable query overridden here,
;;;               reach-target);
;;;               -reachability-coordinates (reach-via, reach-via>, reach-disallowed>, and
;;;               the coordinate derivation that fills them);
;;;               -gate (gate optional type, (open gate) relation) -- shared with gate,
;;;               walkability (via -passability), visibility, beam-direct, and
;;;               beam-crossing, which all nest -gate instead of hand-declaring it
;;; PROVIDES:
;;;   queries   : reachable (overrides -reachability), reachable-clear
;;;   init      : reachability-init-check

(include-tech -reachability)
(include-tech -reachability-coordinates)
(include-tech -gate)

(in-package :ww)


(define-init-check reachability-init-check (literals)
  (:consumes gate)
  (check-init-list-relation-items-have-types
    literals 'reach-via '(gate))
  (check-init-list-relation-items-have-types
    literals 'reach-via> '(gate)))


(define-query reachable (?target reach-target ?reacher location)
  ;; Within reach iff the same location, a symmetric reach edge joins them, or a directional
  ;; edge runs from ?REACHER to ?TARGET -- every barrier open
  ;; in either case.  Callers pass the target first and the actor's own location second (see
  ;; PICKUP-CLEAR and the PUT-* actions), so a REACH-VIA> row reads reacher-then-target and
  ;; models an opening that admits an arm one way only: a letterbox slot, an overhang, or a
  ;; recess whose mouth faces one side.  Reserve it for asymmetry the actor cannot defeat by
  ;; standing higher.  A pure height difference belongs in symmetric REACH-VIA instead, since
  ;; the vertical tests already measure that difference from wherever the actor currently
  ;; stands -- encoding it here as well would freeze the one-way verdict at ground level and
  ;; wrongly deny an actor who has climbed onto a box.  Reach itself stays agent-independent:
  ;; WITHIN-AGENT-VERTICAL-REACH bounds lifting in both directions, and
  ;; WITHIN-AGENT-PLACEMENT-REACH bounds only how far above the actor a resting place may be.
  (or (eql ?target ?reacher)
      (and (bind (reach-via ?target $barriers ?reacher))
           (ww-loop for $b in $barriers
                    always (reachable-clear $b)))
      (and (bind (reach-via> ?reacher $directed-barriers ?target))
           (ww-loop for $b in $directed-barriers
                    always (reachable-clear $b)))))


(define-query reachable-clear (?barrier gate)
  ;; A reach barrier clears only as an open gate; a closed gate or any non-gate barrier blocks.
  (and (gate ?barrier)
       (open ?barrier)))
