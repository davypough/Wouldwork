;;; Bounded worker read experiment; no snapshots survive a worker group.
(in-package :ww)

(defstruct worker-read-view static codes memo-symbols memo-tables)
(defstruct worker-read-context sources copies object-index generation views)

(defparameter *worker-read-memo-symbols*
  '(*vertical-type-cache* *location-elevation-cache*
    *mobility-route-keys* *traversal-canonical-families*))

(declaim (inline worker-object-code))
(defun worker-object-code (object)
  (if *worker-code-read-view*
      (or (gethash object *worker-code-read-view*)
          (error 'worker-read-snapshot-error
                 :format-control "Worker read snapshot has no object code for ~S."
                 :format-arguments (list object)))
      (gethash object *constant-integers*)))

(defun validate-worker-read-snapshot-mode ()
  (when *worker-read-snapshots*
    (unless (and (eq *problem-name* 'claustro-topo)
                 (eq *algorithm* 'depth-first)
                 (eq *tree-or-graph* 'graph)
                 (eq *problem-type* 'planning)
                 (eq *solution-type* 'min-length)
                 (plusp *threads*) (null *happening-names*)
                 (null *randomize-search*) (zerop *debug*) (null *probe*))
      (error "Worker read snapshots require Claustro-Topo parallel DFS graph/min-length, no happenings, randomization or debug/probe."))
    ;; Arbitrary callbacks could invalidate the source-backed worker audit.
    (dolist (name '(*global-invariants* *solution-validators*
                    *search-prefix-validators* *search-successor-pruners*
                    *candidate-state-screeners* *min-steps-remaining-contributors*))
      (when (symbol-value name)
        (error "Worker read snapshot audit does not admit callbacks in ~S." name)))
    (dolist (name '(heuristic? prune-state? bounding-function? min-steps-remaining?
                    state-feasible?))
      (when (fboundp name)
        (error "Worker read snapshot audit does not admit ~S." name)))
    (validate-worker-read-registries))
  t)

(defun validate-worker-read-registries ()
  (assert (equal (symbol-value '*mobility-providers*) '(traversal-segments)))
  (assert (alexandria:set-equal
            (symbol-value '*configuration-transition-providers*)
            '(jump-configuration-transitions ladder-configuration-transitions)))
  (assert (alexandria:set-equal
            (mapcar #'second (symbol-value '*traversal-modes*))
            '(walking-segment-for-clause stairs-segment-for-clause
              jump-segment-for-clause ladder-segment-for-clause)))
  (assert (equal (symbol-value '*traversal-cache-parameters*) '(*vertical-reach-limit*))))

(declaim (inline reject-worker-static-write))
(defun reject-worker-static-write (table)
  (when (and *worker-read-phase*
             (or (eq table *static-idb*) (eq table *static-db*)
                 (eq table *worker-static-read-view*)))
    (reject-worker-read-write 'static-fact-update)))

(defun worker-read-empty-table (table)
  (make-hash-table :test (hash-table-test table)
                   :size (hash-table-size table)
                   :rehash-size (hash-table-rehash-size table)
                   :rehash-threshold (hash-table-rehash-threshold table)
                   :synchronized (sb-ext:hash-table-synchronized-p table)))

(defun worker-read-copy-value (value &optional ancestors)
  "Own finite cons trees and strings. Atoms retain identity; other types fail.
   No general vectors/structures or circular graphs are admitted by this pilot."
  (cond ((or (symbolp value) (numberp value) (characterp value)) value)
        ((stringp value) (copy-seq value))
        ((consp value)
         (when (member value ancestors :test #'eq)
           (error "Circular worker read payload is unsupported."))
         (let ((path (cons value ancestors)))
           (cons (worker-read-copy-value (car value) path)
                 (worker-read-copy-value (cdr value) path))))
        (t (error "Unsupported worker read payload type ~S." (type-of value)))))

(defun worker-read-copy-key (key table)
  (when (and (or (consp key) (stringp key))
             (not (eq (hash-table-test table) 'equal)))
    (error "Structured worker read key requires EQUAL, got ~S."
           (hash-table-test table)))
  ;; EQUAL compares strings/cons trees by content. Ordinary vectors use identity
  ;; under EQUAL and are deliberately rejected by the payload copier.
  (worker-read-copy-value key))

(defun worker-read-copy-table (table)
  (let ((copy (worker-read-empty-table table)))
    (maphash (lambda (key value)
               (setf (gethash (worker-read-copy-key key table) copy)
                     (worker-read-copy-value value)))
             table)
    copy))

(defun make-current-worker-read-view ()
  (make-worker-read-view
    :static (worker-read-copy-table *static-idb*)
    :codes (worker-read-copy-table *constant-integers*)
    :memo-symbols (copy-list *worker-read-memo-symbols*)
    :memo-tables (mapcar (lambda (name)
                          (worker-read-empty-table (symbol-value name)))
                        *worker-read-memo-symbols*)))

(defun call-with-worker-read-view (view function)
  (if (null view)
      (funcall function)
      (let ((*worker-static-read-view* (worker-read-view-static view))
            (*worker-code-read-view* (worker-read-view-codes view)))
        (progv (worker-read-view-memo-symbols view)
               (worker-read-view-memo-tables view)
          (funcall function)))))

(defun worker-read-source-tables ()
  (list *static-idb* *constant-integers* *integer-constants*))

(defun begin-worker-read-phase (count &optional (view-maker #'make-current-worker-read-view))
  "Coordinator only, AFTER root task generation and BEFORE thread creation."
  (when *worker-read-snapshots*
    (validate-worker-read-snapshot-mode)
    (bt:with-lock-held (*integer-lock*)
      (reject-worker-read-write 'begin-worker-read-phase)
      (setf *worker-read-phase* t)
      (let ((published nil))
        (unwind-protect
            (prog1
                (make-worker-read-context
                  :sources (worker-read-source-tables)
                  :copies (mapcar #'worker-read-copy-table (worker-read-source-tables))
                  :object-index *last-object-index*
                  :generation *goal-chain-stage-generation*
                  :views (loop repeat count collect (funcall view-maker)))
              (setf published t))
          (unless published (setf *worker-read-phase* nil)))))))

(defun verify-worker-read-context (context)
  (assert (every #'eq (worker-read-context-sources context)
                     (worker-read-source-tables)))
  (assert (= (worker-read-context-object-index context) *last-object-index*))
  (assert (= (worker-read-context-generation context) *goal-chain-stage-generation*))
  (assert (every #'equalp (worker-read-context-copies context)
                         (worker-read-source-tables)))
  (dolist (view (worker-read-context-views context))
    (assert (equalp (worker-read-view-static view)
                    (first (worker-read-context-copies context))))
    (assert (equalp (worker-read-view-codes view)
                    (second (worker-read-context-copies context)))))
  t)

(defun end-worker-read-phase (context)
  "Called only after ALL created workers have joined, including failed workers."
  (when context
    (bt:with-lock-held (*integer-lock*)
      (unwind-protect (verify-worker-read-context context)
        (setf (worker-read-context-views context) nil
              *worker-read-phase* nil)))))
