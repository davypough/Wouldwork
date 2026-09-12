;;; Filename: -support-settling.lisp
;;; Event-driven, same-location tray-release settling. No global gravity sweep.
;;; Candidate geometry is the final tray placement with displaced roots detached.
;;; All landings are selected before any is applied; no iteration-order tie breaking.

(include-tech -support-motion)

(in-package :ww)

(define-query tray-release-riders (?tray tray ?capture-height)
  (do (assign $riders nil)
      (doall (?rider support-occupant)
        (if (on ?rider ?tray)
          (do (assign $level nil)
              (if ?capture-height (assign $level (base ?rider)))
              (push (list ?rider $level) $riders))))
      $riders))

(define-query settling-support (?rider support-occupant ?location location ?ceiling ?excluded)
  (do (assign $best nil)
      (assign $height (location-elevation ?location))
      (if (> $height ?ceiling)
        (error "Settling floor ~S exceeds previous base ~S for ~S"
               $height ?ceiling ?rider))
      (assign $surfaces (physical-supports-at ?location t))
      (ww-loop for $surface in $surfaces
        do (if (and (not (member $surface ?excluded))
                    (support-use-allowed ?rider $surface)
                    (not (support-dependent-p $surface ?rider nil))
                    (cleartop $surface ?rider))
             (do (assign $level (top $surface))
                 (if (and (<= $level ?ceiling) (>= $level $height))
                   (do (if (> $level $height)
                         (assign $best nil))
                       (assign $height $level)
                       (push $surface $best))))))
      (if (rest $best)
        (error "Ambiguous settling for ~S at ~S, height ~S: ~S"
               ?rider ?location $height $best))
      (first $best)))

(define-query released-rider-group (?riders)
  (do (assign $excluded nil)
      (ww-loop for $entry in ?riders
        do (assign $group (support-group (first $entry)))
           (assign $excluded (append $group $excluded)))
      $excluded))

(define-query released-rider-landings (?riders ?location location)
  (do (assign $excluded (released-rider-group ?riders))
      (assign $landings nil)
      (ww-loop for $entry in ?riders
        do (assign $target
             (settling-support (first $entry) ?location (second $entry) $excluded))
           (ww-loop for $prior in $landings
             do (if (and $target (eql $target (second $prior))
                         (support-occupancy-conflict-p (first $entry) (first $prior)))
                  (error "Contending settling riders ~S and ~S on ~S"
                         (first $entry) (first $prior) $target)))
           (push (list (first $entry) $target) $landings))
      $landings))

(define-update settle-released-riders! (?riders ?location location)
  (do (assign $landings (released-rider-landings ?riders ?location))
      (ww-loop for $landing in $landings
        do (assign $rider (first $landing))
           (assign $support (second $landing))
           (if $support (on $rider $support)))))
