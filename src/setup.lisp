;;; Filename: setup.lisp

;;; Setup for planning program.

(in-package :ww)


(defmacro define-global-fixnum (var-name val-form &optional doc-string)
  `(progn (declaim (fixnum ,var-name))
     ,(if (> *num-parallel-threads* 0)
       #+sbcl (when (not (boundp var-name))
                `(sb-ext:defglobal ,var-name ,val-form ,doc-string))
       #+allegro `(defparameter ,var-name ,val-form ,doc-string)
        `(defparameter ,var-name ,val-form ,doc-string))))


(defstruct (problem-state (:print-function print-problem-state) (:copier nil))
  ;A planning state including the current propositional database
  (name nil :type symbol)  ;last action executed
  (instantiations nil :type list)  ;from last action effect
  (happenings nil :type list)  ;(object (next-index next-time next-direction)) pairs
  (time 0.0 :type real)
  (value 0.0 :type real)
  (heuristic 0.0 :type real)
  (idb (make-hash-table) :type hash-table))  ;integer hash table of propositions


(defun list-database (idb)
  (let ((propositions (iter (for (key values) in-hashtable idb)
                            (if (eq values t)
                                (collecting (convert-to-proposition key))  ;non-fluent prop
                                (collecting (convert-to-fluent-proposition key values))))))
    (sort propositions #'string< :key (lambda (prop) (format nil "~A" prop)))))


(defun print-problem-state (state stream depth)
  (declare (problem-state state) (ignore depth))
  (format stream "<~A ~A ~A ~A ~A ~A~%~A>"
      (problem-state-name state)
      (problem-state-instantiations state)
      (problem-state-happenings state)
      (problem-state-time state)
      (problem-state-value state)
      (problem-state-heuristic state)
      (list-database (problem-state-idb state))))


(defun copy-problem-state (state)
  (make-problem-state
   :name (problem-state-name state)
   :instantiations (copy-list (problem-state-instantiations state))
   :happenings (copy-tree (problem-state-happenings state))
   :time (problem-state-time state)
   :value (problem-state-value state)
   :heuristic (problem-state-heuristic state)
   :idb (alexandria:copy-hash-table (problem-state-idb state))))


(defstruct action
  (name nil :type symbol)
  (duration 0.0 :type real)
  (precondition-params nil :type list)
  (precondition-variables nil :type list)
  (precondition-types nil :type list)
  (dynamic nil :type (or nil t))  ;a dynamic rule requires recomputation of params on each execution
  (precondition-instantiations nil :type (or list symbol))
  (precondition-lambda nil :type list)
  (iprecondition-lambda nil :type list)
  ;(precondition-lits nil :type list)  ;used for backward search (not implemented)
  (iprecondition #'identity :type function)
  (effect-variables nil :type list)
  (effect-types nil :type list)
  (effect-adds nil :type list)  ;nonnegative literals only for backward search
  (effect-lambda nil :type list)
  (ieffect-lambda nil :type list)
  (ieffect #'identity :type function))


(defstruct update  ;db updates resulting from a successful action instantiation
  (changes nil :type list)
  (value 0.0 :type real)
  (instantiations nil :type list)
  (followups nil :type list))  ;next & finally followup function calls


(defstruct solution  ;the record of a solution
  (depth 0 :type fixnum)
  (time 0.0 :type real)
  (value 0.0 :type real)
  (path nil :type list)
  (goal (make-problem-state) :type problem-state))


(defparameter *start-state* (make-problem-state)
  "Start search from this state.")
(declaim (problem-state *start-state*))


(defmacro when-debug>= (n &rest expressions)
  "Inserts debugging expressions when *debug* >= n."
  `(when (>= *debug* ,n)
     ,@expressions))


(defun break-here ()
  (break))


(defun setup ()
  (format t "Setting up...~%")
  (setf *query-names* (nreverse *query-names*))
  (setf *update-names* (nreverse *update-names*))
  (setf *init-actions* (nreverse *init-actions*))
;  (dolist (action *actions*)  ;future backchaining
;    (install-precondition-lits action)
;    (install-effect-adds action))
  (with-slots (name instantiations happenings time value heuristic) *start-state*
    (let ((first-event-time (loop for object in *happenings* 
                              minimize (car (aref (get object :events) 0)))))
      (setf happenings (loop for object in *happenings* ;property list of happening objects
                             collect (list object 
                                          (list 0 first-event-time +1))))  ;next (index time direction)
      (setf time 0.0)
      (setf value 0.0)
      (setf heuristic 0.0)
      (setf instantiations nil)
      (setf name nil)))
  (do-init-action-updates *start-state*)  ;updates db & static-db, but not start-state idb yet
  (when (> (length *happenings*) 1)
    (setq *happenings* (sort *happenings* #'< :key (lambda (object)
                                                     (first (aref (get object :events) 0))))))
  (format t "Converting propositions to integers...~%")
  (do-integer-conversion)  ;allows integer hashtable db lookups, adds start-state idb
  (convert-ilambdas-to-fns) ;and compile
  (if *actions*
    (progn (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
           (setf *min-action-duration* (reduce #'min *actions* :key #'action-duration)))
    (format t "~%NOTE: There are no defined actions.~%"))
  (iter (for action in *actions*)
        (setf (action-precondition-instantiations action)  ;previous setting, nil or restriction
              (or (type-instantiations (action-precondition-types action)
                                       (action-precondition-instantiations action)
                                       *start-state*)
                  '(nil))))
   (when (fboundp 'heuristic?)
    (setf (problem-state-heuristic *start-state*) (funcall 'heuristic? *start-state*)))
  (display-parameter-settings))


(defun display-parameter-settings ()
  (format t "~%Current parameter settings:")
  (ut::prt (ww-get 'problem) (ww-get 'tree-or-graph) (ww-get 'solution-type)
           (ww-get 'depth-cutoff) (ww-get 'progress-reporting-interval) 
           *num-parallel-threads* *debug*)
  (terpri))
  
  
(defun delete-actions (&rest names)
  "Deletes named actions from *actions* at run-time."
  (setf *actions* (delete-if (lambda (name)
                               (member name names))
                             *actions*
                             :key #'action-name)))


(defun get-state-codes ()  ;user calls this after finding backwards *solutions*
  (format t "~%Working ...~%")
  (clrhash *state-codes*)
  (iter (for soln in *solutions*)
        (for path = (solution-path soln))
        (for db-props = (list-database (problem-state-idb (solution-goal soln))))
        (setf (gethash (funcall 'encode-state db-props) *state-codes*) path))
  *state-codes*)


(defun backward-path-exists (state)
  "Use in forward search goal to check existence of backward path."
  (gethash (funcall 'encode-state (list-database (problem-state-idb state))) *state-codes*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Type Specifiers ;;;;;;;;;;;;;;;;;;;;;;


(defun list-of-?variables (lst)
  (and (listp lst)
       (every #'?varp lst)))

(defun list-of-variables (lst)
  (and (listp lst)
       (every (lambda (arg)
                (or (varp arg)
                    (symbolp arg)
                    (realp arg)))
              lst)))

(defun type-description (descrip)
  (or (nth-value 1 (gethash descrip *types*))
      (eq descrip 'fluent)
      (and ($varp descrip)
           (or (nth-value 1 (gethash (extract-type descrip) *types*))
               (symbolp (extract-type descrip))))
      (and (consp descrip)
           (eql (car descrip) 'either)
           (or (every (lambda (typ)
                        (gethash (extract-type typ) *types*))
                      (cdr descrip))))))

(defun list-of-parameter-types (lst)
  (every (lambda (typ)
           (or (null typ)
               (ut::hash-table-present typ *types*)
               (eq typ 'fluent)
               (and (listp typ)
                    (list-of-parameter-types typ))))
         lst))

(defun relation (rel)
  (and (listp rel)
       (iter (for rel-item in (cdr rel))
             (for rel-type in (gethash (car rel) *relations*))
             (unless (or (eq rel-item rel-type)
                         (and (listp rel-type)
                              (member rel-item (cdr rel-type)))  ;either type
                         (subsetp (gethash rel-item *types*) (gethash rel-type *types*)))
                         ;(alexandria:set-equal rel-item rel-type))
               (leave nil))
             (finally (return t)))))

(defun negative-relation (neg-rel)
  (and (listp neg-rel)
       (eql (first neg-rel) 'not)
       (relation (second neg-rel))))

(defun key-list (lst)
  (and (consp lst)
       (loop for (keyword *) on lst by #'cddr
           when (not (member keyword '(:events :repeat :rebound :interrupt)))
           do (return nil)
             finally (return t))))

(defun proposition (prop)
  (and (listp prop)
       (or (gethash (car prop) *relations*)
           (gethash (car prop) *static-relations*))
       (or (null (cdr prop))
           (every (lambda (const typ)
                    (or (member const (gethash (extract-type typ) *types*))
                        (and (listp typ)
                             (eql (car typ) 'either)
                             (member const
                               (reduce #'union 
                                       (mapcar (lambda (typ)
                                                 (gethash (extract-type typ) *types*))
                                               (cdr typ)))))
                        (typep const (extract-type typ))))
              (cdr prop) (or (gethash (car prop) *relations*)
                             (gethash (car prop) *static-relations*))))))

(defun atomic-formula (form)
  (and (listp form)
       (or (gethash (car form) *relations*)
           (gethash (car form) *static-relations*))
       (every (lambda (arg)
                (or (varp arg)
                    (symbolp arg)
                    (realp arg)
                    (and (listp arg)
                         (fboundp (car arg)))))
              (cdr form))))

(defun function-formula (form)
  (and (listp form)
       (member (car form) (append *query-names* *update-names*))
       (every (lambda (arg)
                (or (varp arg)
                    (symbolp arg)
                    (realp arg)))
              (cdr form))))
