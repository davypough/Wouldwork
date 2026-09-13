;;; Gate-only factorial diagnostic. Explicitly load after ww-table-read-probe.
;;; No production function replacement, table rebinding, or search.
(in-package :ww)

(defvar *gate-table-probe-results* nil)

(defun gate-probe-rewrite (form names)
  "Rewrite only executable references; preserve quoted data."
  (cond ((eq form '*static-idb*) 'probe-static)
        ((eq form '*constant-integers*) 'probe-codes)
        ((equal form '(or *worker-static-read-view* *static-idb*)) 'probe-static)
        ((and (consp form) (eq (first form) 'worker-object-code))
         `(gethash ,(second form) probe-codes))
        ((atom form) form)
        ((eq (first form) 'quote) form)
        (t
         (let ((replacement (assoc (first form) names)))
           (if replacement
               (append (list (cdr replacement))
                       (mapcar (lambda (part) (gate-probe-rewrite part names)) (rest form))
                       '(probe-static probe-codes))
               (mapcar (lambda (part) (gate-probe-rewrite part names)) form))))))

(defun gate-probe-audit (form)
  "Reject additional WW calls rather than silently leaving unredirected paths."
  (when (consp form)
    (unless (eq (first form) 'quote)
      (let ((head (first form)))
        (when (and (symbolp head) (eq (symbol-package head) (find-package :ww))
                   (fboundp head)
                   (not (member head '(update-gate-status! control-on energized
                                       problem-state.idb add-int-prop-key del-int-prop-key
                                       worker-object-code))))
          (error "Unaudited gate dependency: ~S" head)))
      (mapc #'gate-probe-audit form))))

(defun gate-probe-compile (names)
  (dolist (entry (reverse names))
    (let ((form (subst-int-code (symbol-value (car entry)))))
      (gate-probe-audit form)
      (assert (eq (first form) 'lambda))
      (compile (cdr entry)
               `(lambda (,@(second form) probe-static probe-codes)
                  (declare (optimize (speed 3) (safety 1) (debug 1)))
                  ,@(mapcar (lambda (part) (gate-probe-rewrite part names))
                            (if (stringp (third form)) (cdddr form) (cddr form))))))))

(defun gate-probe-call (function state static codes original-p)
  (let ((*detect-propagated-changes* t) (*propagated-state-changed* nil)
        (*idb-hash-acc* nil) (*fixed-idb-hash-acc* nil) (*symmetry-idb-acc* nil))
    (if original-p (funcall function state) (funcall function state static codes))
    *propagated-state-changed*))

(defun gate-probe-verify (function states static codes)
  ;; Verify changing and settled branches, outside timing, against original code.
  (dolist (seed states)
    (let ((expected (copy-problem-state seed)) (actual (copy-problem-state seed)))
      (dotimes (pass 2)
        (let ((expected-change (gate-probe-call #'update-gate-status! expected nil nil t))
              (actual-change (gate-probe-call function actual static codes nil)))
          (assert (eql expected-change actual-change))
          (assert (equalp (problem-state.idb expected) (problem-state.idb actual))))))))

(defun gate-probe-kernel (context keys passes)
  (declare (ignore keys) (optimize (speed 3) (safety 1) (debug 1)))
  (destructuring-bind (function static codes states) context
    (let ((*detect-propagated-changes* t) (*propagated-state-changed* nil)
          (*idb-hash-acc* nil) (*fixed-idb-hash-acc* nil) (*symmetry-idb-acc* nil))
      (dotimes (pass passes)
        (dolist (state states) (funcall function state static codes))))
    (* passes (length states))))

(defun gate-probe-case (function states passes workers private-static private-codes)
  (let* ((static (table-read-probe-copy *static-idb*
                                       (sb-ext:hash-table-synchronized-p *static-idb*)))
         (codes (table-read-probe-copy *constant-integers*
                                      (sb-ext:hash-table-synchronized-p *constant-integers*)))
         (contexts
           (loop repeat workers collect
             (list function
                   (if private-static
                       (table-read-probe-copy static (sb-ext:hash-table-synchronized-p static)) static)
                   (if private-codes
                       (table-read-probe-copy codes (sb-ext:hash-table-synchronized-p codes)) codes)
                   (mapcar #'copy-problem-state states)))))
    (dolist (context contexts)
      (gate-probe-verify function states (second context) (third context)))
    (let ((result (table-read-probe-case contexts (coerce states 'vector) passes
                                        :kernel #'gate-probe-kernel)))
      (dolist (context contexts)
        (assert (equalp (second context) *static-idb*))
        (assert (equalp (third context) *constant-integers*))
        (loop for seed in states for final in (fourth context)
              do (let ((expected (copy-problem-state seed)))
                   (gate-probe-call #'update-gate-status! expected nil nil t)
                   (assert (equalp (problem-state.idb expected)
                                   (problem-state.idb final))))))
      (append (list :private-static private-static :private-codes private-codes
                    :verification :passed :unit :gate-update-calls)
              result))))

(defun gate-probe-seeds ()
  "Start and optional retained solution copies, plus inverted OPEN facts."
  (let* ((best (first (sort (copy-list *solution-paths*) #'< :key #'solution.depth)))
         (base (cons (copy-problem-state *start-state*)
                     (when best (list (copy-problem-state (solution.goal best))))))
         (altered (mapcar #'copy-problem-state base))
         (open-code (gethash 'open *constant-integers*)))
    (when best (assert (= (solution.depth best) 33)))
    (dolist (state altered)
      (dolist (gate '(gate1 gate2 gate3 gate4 gate5 gate6 gate7 gate8 gate9))
        (let* ((key (+ open-code (* 1000 (gethash gate *constant-integers*))))
               (db (problem-state.idb state)))
          (if (nth-value 1 (gethash key db))
              (remhash key db) (setf (gethash key db) t)))))
    (append base altered)))

(defun run-gate-table-probe (&key (target-seconds 1d0) (rounds 3))
  "One-worker reference plus four two-worker sharing combinations. No solve.
Timing repeats settled updates on start/optional endpoint-derived states, not search."
  (assert (eq *problem-name* 'claustro-topo))
  (assert (and (not *print-updates*) (null *happening-names*)
               (plusp target-seconds) (integerp rounds) (plusp rounds)))
  (let ((names (mapcar (lambda (name) (cons name (gensym (symbol-name name))))
                       '(update-gate-status! control-on energized)))
        (states (gate-probe-seeds))
        (cases '((1 nil nil) (2 nil nil) (2 t nil) (2 nil t) (2 t t))))
    (unwind-protect
        (progn
          (gate-probe-compile names)
          (let* ((function (symbol-function (cdar names)))
                 (passes
                   (loop for n = 2048 then (* 2 n)
                         for row = (gate-probe-case function states n 1 nil nil)
                         do (format t "~&GATE CALIBRATION ~S~%" row) (finish-output)
                         when (>= (getf row :wall-seconds) target-seconds) return n)))
            (format t "~&GATE PROBE ~S~%"
                    (list :lisp (lisp-implementation-version) :passes passes
                          :states (length states)
                          :seed-source (if (= (length states) 4)
                                           :start-and-solution :start-only)
                          :workload :settled-gate-updates
                          :static-sync (sb-ext:hash-table-synchronized-p *static-idb*)
                          :codes-sync (sb-ext:hash-table-synchronized-p *constant-integers*)
                          :hash-folding nil :clock internal-time-units-per-second
                          :policy '(speed 3 safety 1 debug 1)))
            (setf *gate-table-probe-results* nil)
            (dotimes (round rounds)
              (let* ((offset (mod round (length cases)))
                     (order (append (nthcdr offset cases) (subseq cases 0 offset))))
                (dolist (entry (if (oddp round) (reverse order) order))
                  (let ((row (append (list :round (1+ round))
                                     (apply #'gate-probe-case function states passes entry))))
                    (push row *gate-table-probe-results*)
                    (format t "~&GATE RESULT ~S~%" row) (finish-output)))))
            (setf *gate-table-probe-results* (nreverse *gate-table-probe-results*))))
      (dolist (entry names) (fmakunbound (cdr entry)))))
  (values))
