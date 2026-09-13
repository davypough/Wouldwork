;;; Explicitly loaded REPL diagnostics; no search on load.
(in-package :ww)

(defvar *parallel-phase-row* nil)
(defvar *parallel-phase-results* nil)
(defvar *parallel-phase-probe-active* nil)

(defparameter *parallel-update-specs*
  '((update-plate-status! :plate-calls :plate-samples :plate-ticks)
    (update-receiver-status! :receiver-calls :receiver-samples :receiver-ticks)
    (update-gate-status! :gate-calls :gate-samples :gate-ticks)
    (update-blower-status! :blower-calls :blower-samples :blower-ticks)
    (enforce-threat-safety! :threat-calls :threat-samples :threat-ticks)))

(defparameter *parallel-detail-specs*
  '((get-new-states :new-calls :new-samples :new-ticks)
    (copy-idb :copy-calls :copy-samples :copy-ticks)
    (process-followups :followup-calls :followup-samples :followup-ticks)
    (propagate-changes! :propagate-calls :propagate-samples :propagate-ticks)
    (propagate-consequences! :pass-calls :pass-samples :pass-ticks)))

(defun parallel-phase-wrapper (original calls-key samples-key time-key interval)
  "Sample wall time once per INTERVAL calls, with worker-local counters."
  (lambda (&rest args)
    (if (null *parallel-phase-row*)
        (apply original args)
        (let ((count (incf (getf *parallel-phase-row* calls-key))))
          (if (not (zerop (mod count interval)))
              (apply original args)
              (let ((start (get-internal-real-time)))
                (unwind-protect (apply original args)
                  (incf (getf *parallel-phase-row* samples-key))
                  (incf (getf *parallel-phase-row* time-key)
                        (- (get-internal-real-time) start)))))))))

(defun parallel-phase-worker-wrapper (original rows)
  (lambda (worker-id queue)
    (let* ((*parallel-phase-row*
             (list :worker worker-id :expand-calls 0 :expand-samples 0
                   :expand-ticks 0 :successor-calls 0 :successor-samples 0
                   :successor-ticks 0 :queue-calls 0 :queue-samples 0
                   :queue-ticks 0 :worker-ticks 0
                   :new-calls 0 :new-samples 0 :new-ticks 0
                   :copy-calls 0 :copy-samples 0 :copy-ticks 0
                   :followup-calls 0 :followup-samples 0 :followup-ticks 0
                   :propagate-calls 0 :propagate-samples 0 :propagate-ticks 0
                   :pass-calls 0 :pass-samples 0 :pass-ticks 0
                   :plate-calls 0 :plate-samples 0 :plate-ticks 0
                   :receiver-calls 0 :receiver-samples 0 :receiver-ticks 0
                   :gate-calls 0 :gate-samples 0 :gate-ticks 0
                   :blower-calls 0 :blower-samples 0 :blower-ticks 0
                   :threat-calls 0 :threat-samples 0 :threat-ticks 0))
           (start (get-internal-real-time)))
      (setf (aref rows worker-id) *parallel-phase-row*)
      (unwind-protect (funcall original worker-id queue)
        (setf (getf *parallel-phase-row* :worker-ticks)
              (- (get-internal-real-time) start))))))

(defun parallel-phase-summary (row)
  "Keep raw sample totals: they are not whole-phase elapsed times."
  (append row
          (list :clock-ticks-per-second internal-time-units-per-second
                :worker-seconds (/ (getf row :worker-ticks)
                                   (float internal-time-units-per-second))
                :queue-seconds (/ (getf row :queue-ticks)
                                  (float internal-time-units-per-second)))))

(defun run-parallel-phase-probe (label &key detail updates)
  "Run one instrumented baseline and restore function definitions afterward.
Workers must be idle before invocation. Expansion and successor handling are
sampled every 1024 calls. Queue calls are all timed, including terminal waits."
  (assert (and (> *threads* 0) (not *parallel-phase-probe-active*)))
  (let* ((*parallel-phase-probe-active* t)
         (rows (make-array *threads* :initial-element nil))
         (specs (append (when detail *parallel-detail-specs*)
                        (when updates *parallel-update-specs*)))
         (names (append '(parallel-worker expand worker-process-successors-phase1
                          tq-pop-blocking)
                        (mapcar #'first specs)))
         (originals (mapcar (lambda (name) (cons name (symbol-function name))) names)))
    (unwind-protect
        (progn
          (setf (symbol-function 'parallel-worker)
                (parallel-phase-worker-wrapper (cdr (assoc 'parallel-worker originals)) rows)
                (symbol-function 'expand)
                (parallel-phase-wrapper (cdr (assoc 'expand originals))
                                        :expand-calls :expand-samples :expand-ticks 1024)
                (symbol-function 'worker-process-successors-phase1)
                (parallel-phase-wrapper
                 (cdr (assoc 'worker-process-successors-phase1 originals))
                 :successor-calls :successor-samples :successor-ticks 1024)
                (symbol-function 'tq-pop-blocking)
                (parallel-phase-wrapper (cdr (assoc 'tq-pop-blocking originals))
                                        :queue-calls :queue-samples :queue-ticks 1))
          (when specs
            (dolist (spec specs)
              (destructuring-bind (name calls samples ticks) spec
                (setf (symbol-function name)
                      (parallel-phase-wrapper (cdr (assoc name originals))
                                              calls samples ticks 1024)))))
          (format t "~&Phase probe detail: ~S; updates: ~S; sample interval: 1024~%"
                  detail updates)
          (run-parallel-baseline label))
      (dolist (entry originals)
        (setf (symbol-function (car entry)) (cdr entry))))
    (setf *parallel-phase-results* (map 'list #'parallel-phase-summary rows))
    (format t "~&PHASE PROBE BEGIN~%~S~%PHASE PROBE END~%" *parallel-phase-results*)
    *parallel-phase-results*))
