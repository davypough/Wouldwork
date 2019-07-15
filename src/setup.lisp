;;; Filename: setup.lisp

;;; Setup for planning program.

(in-package :ww)


(defstruct (problem-state (:print-function print-problem-state) (:copier nil))
  ;A planning state including the current propositional database
  (name nil :type symbol)  ;last action executed
  (instantiations nil :type list)  ;from last action effect
  (happenings nil :type list)  ;(object (next-index next-time next-direction)) pairs
  (time 0.0 :type real)
  (value 0.0 :type real)
  (idb (make-hash-table) :type hash-table))  ;integer hash table of propositions


(defun list-database (idb)
  (let ((propositions (iter (for (key values) in-hashtable idb)
                            (if (eq values t)
                                (collecting (convert-to-proposition key))  ;non-fluent prop
                                (collecting (convert-to-fluent-proposition key values))))))
    (sort propositions #'string< :key (lambda (prop) (format nil "~A" prop)))))


(defun print-problem-state (state stream depth)
  (declare (problem-state state) (ignore depth))
  (format stream "<~A ~A ~A ~A ~A~%~A>"
      (problem-state-name state)
      (problem-state-instantiations state)
      (problem-state-happenings state)
      (problem-state-time state)
      (problem-state-value state)
      (list-database (problem-state-idb state))))


(defun copy-problem-state (state)
  (make-problem-state
   :name (problem-state-name state)
   :instantiations (copy-list (problem-state-instantiations state))
   :happenings (copy-tree (problem-state-happenings state))
   :time (problem-state-time state)
   :value (problem-state-value state)
   :idb (alexandria:copy-hash-table (problem-state-idb state))))


(defstruct action
  (name nil :type symbol)
  (duration 0 :type real)
  (precondition-variables nil :type list)
  (precondition-types nil :type list)
  (precondition-instantiations nil :type list)
  (precondition-lambda nil :type list)
  (iprecondition-lambda nil :type list)
  (precondition-lits nil :type list)  ;used for backward search
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


(setq *print-right-margin* 140)
;Allows non-wrap printing of *search-tree* for deep trees.

;Global settings for wouldwork planner
(let ((settings (make-hash-table :test #'eq)))
    (setf (gethash 'debug settings) 0)
  ;Estimated maximum number of unique states to be explored during search.
  ;If this number is exceeded, hash table resizing may slow graph search.
    (setf (gethash 'max-states settings) 10000)
  ;Name of the current problem, assigned in problem.lisp by user.
    (setf (gethash 'problem settings) 'unspecified)
  ;Specify whether to search for 'first, 'min-length, 'min-time, or 'every solution.
    (setf (gethash 'solution-type settings) 'first)
  ;Print progress during search after each multiple n of states examined.
    (setf (gethash 'progress-reporting-interval settings) 100000)
  ;The max possible number of steps to consider toward any goal.
  ;Negative or 0 means no cutoff, value set in problem.lisp.
    (setf (gethash 'depth-cutoff settings) 10)
  ;Whether there are repeated states (graph) or not (tree); try both
    (setf (gethash 'tree-or-graph settings) 'graph)
  ;whether to invoke parallel processing
    (setf (gethash 'parallel settings) nil)
  ;for parallel processing (excluding main consumer)
    (setf (gethash 'num-threads settings) 3)
  (defun ww-get (var)  ;accessors for ww settings
    (gethash var settings))
  (defun ww-set (var value)
    (setf (gethash var settings) value)))


;To display debugging info, (ww-set 'debug <n>) where <n> is 0-5.
  ;0 - no debugging
  ;1 - display full search tree
  ;3 - display basic node (ww-set 'solution-type
  ;4 - display full node (ww-set 'solution-type
  ;5 - display full node (ww-set 'solution-type + break after each expansion cycle

(ww-set 'start-state (make-problem-state))

(ww-set 'types (make-hash-table :test #'eq))

(ww-set 'relations (make-hash-table :test #'eq))  ;dynamic relations

(ww-set 'static-relations (make-hash-table :test #'eq))

(ww-set 'connectives '(and or not))

(ww-set 'symmetrics (make-hash-table :test #'eq))  ;symmetric relations

(ww-set 'complements (make-hash-table :test #'eq))

(ww-set 'fluent-relation-indices (make-hash-table :test #'eq))  ;list of fluent argument indices for a relation

(ww-set 'db (make-hash-table :test #'equal))  ;initial database of dynamic relations

(ww-set 'idb (make-hash-table))  ;initial integer database of dynamic propositions

(ww-set 'constant-integers (make-hash-table))  ;integer codes for the problem's object constants

(ww-set 'integer-constants (make-hash-table))  ;translating codes back to constants for printout

(ww-set 'min-action-duration 0.0)  ;the least action duration among all actions

;;; The following parameters will not go into settings

(defparameter *query-names* nil)  ;list of all user-defined query functions

(defparameter *update-names* nil)  ;list of all user-defined update functions

(defparameter *actions* nil)  ;list of all potential actions

(defparameter *init-actions* nil)  ;list of all initialization actions

(defparameter *happenings* nil)  ;the list of objects having exogenous events

(defparameter *static-db* (make-hash-table :test #'equal))  ;initial database of static propositions

(defparameter *static-idb* (make-hash-table))  ;initial integer database of static propositions

(defparameter *goal* nil)  ;Holds the goal test function--value is a lambda expression, symbol-function is the function

(defparameter *constraint* nil)  ;any constraint test function

(defparameter *last-object-index* 0)  ;last index of object constants seen so far in propositions


(defun setup ()
  (format t "Setting up...~%")
  (setf *query-names* (nreverse *query-names*))
  (setf *update-names* (nreverse *update-names*))
  (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
  (ww-set 'min-action-duration (reduce #'min *actions* :key #'action-duration))
  (setf *init-actions* (nreverse *init-actions*))
;  (dolist (action *actions*)  ;future backchaining
;    (install-precondition-lits action)
;    (install-effect-adds action))
  (with-slots (name instantiations happenings time value) (ww-get 'start-state)
    (let ((first-event-time (loop for object in *happenings* 
                              minimize (car (aref (get object :events) 0)))))
      (setf happenings (loop for object in *happenings* ;property list of happening objects
                             collect (list object 
                                          (list 0 first-event-time +1))))  ;next (index time direction)
      (setf time 0.0)
      (setf value 0.0)
      (setf instantiations nil)
      (setf name nil)))
  (do-init-action-updates (ww-get 'start-state))  ;updates db & static-db, but not start-state idb yet
  (when (> (length *happenings*) 1)
    (setq *happenings* (sort *happenings* #'< :key (lambda (object)
                                                     (first (aref (get object :events) 0))))))
  (format t "Converting propositions to integers...~%")
  (do-integer-conversion)  ;allows integer hashtable db lookups, adds start-state idb
  (convert-ilambdas-to-fns) ;and compile
  (display-parameter-settings))


(defun display-parameter-settings ()
  (format t "~%Current parameter settings:")
  (ut::prt (ww-get 'problem) (ww-get 'tree-or-graph) (ww-get 'solution-type) (ww-get 'depth-cutoff)
           (ww-get 'debug) (ww-get 'parallel))
  (when (ww-get 'parallel)
    (ut::prt (ww-get 'num-threads)))
  (terpri))


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
  (or (nth-value 1 (gethash descrip (ww-get 'types)))
      (eql descrip 'fluent)
      (and ($varp descrip)
           (or (nth-value 1 (gethash (extract-type descrip) (ww-get 'types)))
               (symbolp (extract-type descrip))))
      (and (consp descrip)
           (eql (car descrip) 'either)
           (or (every (lambda (typ)
                        (gethash (extract-type typ) (ww-get 'types)))
                      (cdr descrip))))))

(defun list-of-parameter-types (lst)
  (every (lambda (typ)
           (or (null typ)
               (ut::hash-table-present typ (ww-get 'types))
               (eq typ 'fluent)
               (and (listp typ)
                    (list-of-parameter-types typ))))
         lst))

(defun relation (rel)
  (and (listp rel)
       (iter (for rel-item in (cdr rel))
             (for rel-type in (gethash (car rel) (ww-get 'relations)))
             (unless (or (eq rel-item rel-type)
                         (and (listp rel-type)
                              (member rel-item (cdr rel-type)))  ;either type
                         (subsetp (gethash rel-item (ww-get 'types)) (gethash rel-type (ww-get 'types))))
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
       (or (gethash (car prop) (ww-get 'relations))
           (gethash (car prop) (ww-get 'static-relations)))
       (or (null (cdr prop))
           (every (lambda (const typ)
                    (or (member const (gethash (extract-type typ) (ww-get 'types)))
                        (and (listp typ)
                             (eql (car typ) 'either)
                             (member const
                               (reduce #'union 
                                       (mapcar (lambda (typ)
                                                 (gethash (extract-type typ) (ww-get 'types)))
                                               (cdr typ)))))
                        (typep const (extract-type typ))))
              (cdr prop) (or (gethash (car prop) (ww-get 'relations))
                             (gethash (car prop) (ww-get 'static-relations)))))))

(defun atomic-formula (form)
  (and (listp form)
       (or (gethash (car form) (ww-get 'relations))
           (gethash (car form) (ww-get 'static-relations)))
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
