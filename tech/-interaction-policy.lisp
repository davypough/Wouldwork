;;; Filename: -interaction-policy.lisp

;;; Interaction-policy hooks.  Shared actions call these neutral interfaces without knowing
;;; whether a recorder is present.  -recorder-core overrides them after nesting this file,
;;; so include-tech deduplication preserves the overrides regardless of public include order.
;;;
;;; Two distinct questions are asked about a support, and they have different answers.
;;; SUPPORT-USE-ALLOWED is policy: may this occupant use this support at all.
;;; SUPPORT-OCCUPANCY-CONFLICT-P is capacity: given that two occupants both want the
;;; support's top, do they collide.  Ordinary physics says every pair collides, which is
;;; the default here; only the recorder's superimposed live/ghost layers make a pair
;;; compatible.  This mirrors CONNECTOR-LOCATION-CONFLICT-P, which asks the same capacity
;;; question about a location for connectors.
;;;
;;; PROVIDES:
;;;   queries : object-manipulation-allowed  -- actor may pick up, carry, place, or mount object
;;;             support-use-allowed          -- occupant may rest or stand on support
;;;             support-occupancy-conflict-p -- two occupants contend for one support top
;;;             connector-pairing-allowed    -- actor may pair connector to terminus
;;;             connector-location-conflict-p -- another lit connector blocks this placement

(in-package :ww)


(define-query object-manipulation-allowed (?actor ?object)
  (do ?actor ?object t))


(define-query support-use-allowed (?occupant ?support)
  (do ?occupant ?support t))


(define-query support-occupancy-conflict-p (?occupant ?other)
  (do ?occupant ?other t))


(define-query connector-pairing-allowed (?actor ?connector ?terminus)
  (do ?actor ?connector ?terminus t))


(define-query connector-location-conflict-p (?connector ?other)
  (do ?connector ?other t))
