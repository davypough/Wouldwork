;;; REPL-only diagnostics. Load explicitly; no search runs on load.
(in-package :ww)

(defvar *parallel-baseline-results* nil
  "Newest baseline first. Survives reloading this helper.")

(defun parallel-baseline-settings ()
  "Record effective controls without changing the staged problem."
  (loop for name in '(*problem-name* *algorithm* *tree-or-graph*
                     *solution-type* *depth-cutoff* *threads* *worker-read-snapshots* *debug*
                     *randomize-search* *symmetry-pruning* *branch*
                     *progress-reporting-interval* *split-depth-max*
                     *tasks-per-thread* *min-tasks* *num-closed-shards*
                     *bound-refresh-interval* *enable-work-donation*
                     *donation-check-interval* *donation-threshold*
                     *donation-fraction*)
        collect (list name (symbol-value name))))

(defun parallel-baseline-best-solution ()
  (first (sort (copy-list *solution-paths*) #'< :key #'solution.depth)))

(defun parallel-baseline-workers ()
  (when (> *threads* 0)
    (loop for stats across *worker-stats-vector*
          for worker from 0
          collect (list :worker worker
                        :states (ws-states-processed stats)
                        :cycles (ws-program-cycles stats)
                        :duplicates (ws-repeated-states stats)
                        :donations (ws-donation-events stats)
                        :nodes-donated (ws-nodes-donated stats)))))

(defun parallel-baseline-record (label settings wall cpu allocated gc-time)
  "Capture measurements before replay can affect any search counters."
  (let ((best (parallel-baseline-best-solution)))
    (list :label label :settings settings
          :lisp (lisp-implementation-version) :machine (machine-type)
          :outcome (search-outcome-status *last-search-outcome*)
          :reason (search-outcome-reason *last-search-outcome*)
          :wall-seconds wall :process-cpu-seconds cpu
          :allocated-bytes allocated :gc-seconds gc-time
          :states *total-states-processed* :cycles *program-cycles*
          :states-per-second (/ *total-states-processed* wall)
          :duplicates *repeated-states*
          :symmetry-pruned (if (use-canonical-symmetry-p)
                              *symmetric-duplicates-pruned*
                              *symmetry-pruning-count*)
          :best-depth (when best (solution.depth best))
          :best-path (when best (copy-tree (solution.path best)))
          :replay-valid nil
          :workers (parallel-baseline-workers))))

(defun run-parallel-baseline (label)
  "Run the staged Claustro baseline once, then replay its best solution.
Timing includes WW-SOLVE initialization and reporting, excludes staging and replay.
Use WW-SET to configure threads before calling; this helper never rebinds them."
  (assert (and (eq *problem-name* 'claustro-topo)
               (eq *algorithm* 'depth-first) (eq *tree-or-graph* 'graph)
               (eq *solution-type* 'min-length) (= *depth-cutoff* 34)
               *symmetry-pruning* (not *randomize-search*)
               (zerop *debug*)))
  (let ((settings (parallel-baseline-settings)))
    (format t "~&BASELINE START ~S~%~S~%" label settings)
    (finish-output)
    (let ((wall (get-internal-real-time))
          (cpu (get-internal-run-time))
          (allocated (sb-ext:get-bytes-consed))
          (gc-time sb-ext:*gc-run-time*))
      (ww-solve)
      (let* ((elapsed (/ (- (get-internal-real-time) wall)
                         (float internal-time-units-per-second)))
             (result (parallel-baseline-record
                      label settings elapsed
                      (/ (- (get-internal-run-time) cpu)
                         (float internal-time-units-per-second))
                      (- (sb-ext:get-bytes-consed) allocated)
                      (/ (- sb-ext:*gc-run-time* gc-time)
                         (float internal-time-units-per-second)))))
        (push result *parallel-baseline-results*)
        (when (getf result :best-depth)
          (setf (getf result :replay-valid)
                (not (null (%validate-solution (getf result :best-path) nil)))))
        (format t "~&BASELINE RESULT BEGIN~%~S~%BASELINE RESULT END~%" result)
        (unless (and (eq (getf result :outcome) :exhausted-with-solutions)
                     (eql (getf result :best-depth) 33)
                     (getf result :replay-valid))
          (warn "Baseline did not meet expected completion/depth/replay checks."))
        result))))
