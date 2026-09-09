;;; Filename: -switch.lisp

;;; Switch substrate.  A switch is fixed wall-mounted apparatus with persistent binary
;;; state.  Absence of SWITCHED-ON means off.

(include-tech -apparatus-coordinates)

(in-package :ww)


(define-optional-types switch)


(define-dynamic-relations
  (switched-on switch))


(define-init-check switch-init-check (literals)
  (let ((coordinate-literals
          (positive-init-literals-with-relation 'apparatus-coords> literals)))
    (dolist (switch (init-type-instances 'switch))
      (let ((coordinate-count
              (count switch coordinate-literals
                     :key (lambda (literal)
                            (second (init-literal-proposition literal))))))
        (unless (= coordinate-count 1)
          (fail-init-check nil
            "~%Switch must have exactly one APPARATUS-COORDS> functional point.~%~
             Switch: ~S~%~
             Coordinate facts: ~D"
            switch coordinate-count))))))
