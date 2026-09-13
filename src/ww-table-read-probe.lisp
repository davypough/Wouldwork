;;; Isolated SBCL read benchmark. No planner mutation or search on load/run.
(in-package :ww)

(defvar *table-read-probe-results* nil)

(defun table-read-probe-copy (source synchronized)
  (let ((copy (make-hash-table :test (hash-table-test source)
                               :size (hash-table-size source)
                               :rehash-size (hash-table-rehash-size source)
                               :rehash-threshold (hash-table-rehash-threshold source)
                               :synchronized synchronized)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) source)
    copy))

(defun table-read-probe-loop (table keys passes)
  (declare (optimize (speed 3) (safety 1) (debug 1))
           (type hash-table table) (type simple-vector keys)
           (type fixnum passes))
  (let ((hits 0))
    (declare (type fixnum hits))
    (dotimes (pass passes)
      (dotimes (index (length keys))
        (when (nth-value 1 (gethash (aref keys index) table))
          (incf hits))))
    hits))

(defun table-read-probe-worker (table keys passes ready start kernel)
  ;; Warm each worker before announcing readiness. Return errors to the caller.
  (let ((failure nil))
    (handler-case (funcall kernel table keys 32)
      (error (condition) (setf failure condition)))
    (sb-thread:signal-semaphore ready)
    (sb-thread:wait-on-semaphore start)
    (if failure
        failure
        (handler-case
            (let* ((begin (get-internal-real-time))
                   (hits (funcall kernel table keys passes))
                   (end (get-internal-real-time)))
              (list :hits hits :start begin :end end))
          (error (condition) condition)))))

(defun table-read-probe-case (tables keys total-passes &key (kernel #'table-read-probe-loop))
  "Time release-to-completion; thread creation and warmup are excluded."
  (let ((ready (sb-thread:make-semaphore :count 0))
        (start (sb-thread:make-semaphore :count 0))
        (threads nil) (rows nil) (wall nil) (cpu nil) (end nil) (cpu-end nil))
    (unwind-protect
        (progn
          (dolist (table tables)
            (let ((local-table table))
              (push (sb-thread:make-thread
                     (lambda ()
                       (table-read-probe-worker local-table keys
                                                (/ total-passes (length tables))
                                                ready start kernel)))
                    threads)))
          (dotimes (i (length tables)) (sb-thread:wait-on-semaphore ready))
          (setf cpu (get-internal-run-time) wall (get-internal-real-time))
          (sb-thread:signal-semaphore start (length tables))
          (setf rows (mapcar #'sb-thread:join-thread (reverse threads))
                end (get-internal-real-time) cpu-end (get-internal-run-time)))
      ;; Also releases waiting workers after interruption/partial creation.
      (sb-thread:signal-semaphore start (length tables))
      (dolist (thread threads) (sb-thread:join-thread thread :default nil)))
    (dolist (row rows) (when (typep row 'error) (error row)))
    (let ((hits (loop for row in rows sum (getf row :hits))))
      (assert (= hits (* (length keys) total-passes)))
      (list :workers (length tables) :lookups hits
            :wall-seconds (/ (- end wall) (float internal-time-units-per-second 1d0))
            :cpu-seconds (/ (- cpu-end cpu) (float internal-time-units-per-second 1d0))
            :release-tick wall :worker-rows rows))))

(defun table-read-probe-calibrate (table keys seconds)
  "Double even pass counts until the fastest one-worker control meets target."
  (loop for passes = 2048 then (* 2 passes)
        for row = (table-read-probe-case (list table) keys passes)
        do (format t "~&Calibration: ~S~%" row) (finish-output)
        when (>= (getf row :wall-seconds) seconds) return passes))

(defun run-static-table-read-probe (&key (target-seconds 1d0) (rounds 3)
                                        (context "Affinity/core placement unrecorded"))
  "Fixed total work across six cases, rotated/reversed order, private snapshots.
Call only while the planner is idle. No production synchronization changes."
  (assert (and (plusp target-seconds) (integerp rounds) (plusp rounds)))
  (let* ((keys (coerce (loop for key being the hash-keys of *static-idb*
                            collect key) 'simple-vector))
         (locked (table-read-probe-copy *static-idb* t))
         (locked2 (table-read-probe-copy *static-idb* t))
         (unlocked (table-read-probe-copy *static-idb* nil))
         (unlocked2 (table-read-probe-copy *static-idb* nil))
         (cases (list (list :sync-one locked) (list :sync-shared locked locked)
                      (list :sync-separate locked locked2)
                      (list :plain-one unlocked) (list :plain-shared unlocked unlocked)
                      (list :plain-separate unlocked unlocked2))))
    (assert (plusp (length keys)))
    (setf *table-read-probe-results* nil)
    (format t "~&TABLE READ PROBE V2 ~S~%"
            (list :lisp (lisp-implementation-version) :machine (machine-type)
                  :context context :clock internal-time-units-per-second
                  :policy '(speed 3 safety 1 debug 1) :keys (length keys)
                  :test (hash-table-test unlocked) :size (hash-table-size unlocked)
                  :key-order :snapshot-order :target-seconds target-seconds))
    (let ((passes (table-read-probe-calibrate unlocked keys target-seconds)))
      (assert (< (* passes (length keys)) most-positive-fixnum))
      (dotimes (round rounds)
        (let* ((offset (mod (* 2 round) (length cases)))
               (order (append (nthcdr offset cases) (subseq cases 0 offset))))
          (dolist (entry (if (oddp round) (reverse order) order))
            (let ((row (append (list :round (1+ round) :case (first entry)
                                     :total-passes passes)
                               (table-read-probe-case (rest entry) keys passes))))
              (push row *table-read-probe-results*)
              (format t "~&~S~%" row) (finish-output))))))
    (setf *table-read-probe-results* (nreverse *table-read-probe-results*))
    (values)))

(defun inspect-static-table-read-path ()
  "SBCL 2.6.8 diagnostic: inspect getters and observe private-copy lookup cache.
No benchmark, solve, implementation replacement, or planner-table mutation."
  (format t "~&TABLE READ PATH: SBCL ~A~%" (lisp-implementation-version))
  (dolist (name '(*static-idb* *constant-integers* *integer-constants*
                  *relations* *static-relations* *fluent-relation-indices*))
    (let ((table (symbol-value name)))
      (format t "~&~S~%"
              (list :table name :count (hash-table-count table)
                    :synchronized (sb-ext:hash-table-synchronized-p table)
                    :getter (sb-impl::hash-table-gethash-impl table)))))
  (dolist (synchronized '(nil t))
    (let* ((table (table-read-probe-copy *static-idb* synchronized))
           (keys (loop for key being the hash-keys of table
                       repeat 8 collect key))
           (getter (sb-impl::hash-table-gethash-impl table)))
      (format t "~&PRIVATE COPY: ~S~%"
              (list :synchronized synchronized :getter getter))
      (dolist (key (append keys keys))
        (let ((before (sb-impl::hash-table-cache table)))
          (multiple-value-bind (value present) (gethash key table)
            (declare (ignore value))
            (assert present)
            (format t "~&~S~%"
                    (list :key key :cache-before before
                          :cache-after (sb-impl::hash-table-cache table))))))
      (disassemble getter)))
  (dolist (name '(control-on energized update-gate-status!
                  update-receiver-status! direct-beam-reaches-receiver))
    (format t "~&GENERATED ~S~%" name)
    (pprint (subst-int-code (symbol-value name))))
  (values))
