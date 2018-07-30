;;; Filename: setup.lisp

;;; Setup for planning program.

(in-package :ww)


(defstruct (problem-state (:print-function print-problem-state) (:copier nil))
  ;A planning state including the current propositional database
  (name nil :type symbol)  ;last action executed
  (instantiations nil :type list)  ;from last action effect
  (happenings nil :type list)  ;(object (next-index next-time next-direction)) pairs
  (time 0 :type real)
  (idb (make-hash-table) :type hash-table))  ;integer hash table of propositions


(defun list-database (idb)
  (let ((propositions (iter (for (key value) in-hashtable idb)
                            (when (eq value t)
                              (collecting (convert-to-proposition key))))))
    (sort propositions #'string< :key (lambda (prop) (format nil "~A" prop)))))


(defun print-problem-state (state stream depth)
  (declare (problem-state state) (ignore depth))
  (format stream "<~A ~A ~A ~A~%~A>~%"
      (problem-state-name state)
      (problem-state-instantiations state)
      (problem-state-happenings state)
      (problem-state-time state)
      (list-database (problem-state-idb state))))


(defun copy-problem-state (state)
  (make-problem-state
   :name (problem-state-name state)
   :instantiations (copy-list (problem-state-instantiations state))
   :happenings (copy-tree (problem-state-happenings state))
   :time (problem-state-time state)
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
  (effect-instantiations nil :type list)
  (effect-adds nil :type list)  ;nonnegative literals only for backward search
  (effect-lambda nil :type list)
  (ieffect-lambda nil :type list)
  (ieffect #'identity :type function))


(defparameter *debug* 0)
  ;Set this parameter to a number (n) to display debugging info.
  ;0 - no debugging
  ;1 - display full search tree
  ;2 - display basic node info
  ;3 - display full node info
  ;4 - display full node info + break after each expansion cycle

(setq *print-right-margin* 140)
  ;Allows non-wrap printing of *search-tree* for deep trees.

(defparameter *max-states* 1000000)
  ;Estimated maximum number of unique states to be explored during search.
  ;If this number is exceeded, hash table resizing will slow search significantly.

(defparameter *first-solution-sufficient* nil)
  ;Specify whether only one solution, the first found, is required, ending search.

(defparameter *progress-reporting-interval* 100000)
  ;Print progress during search after each multiple n of states examined.

(defparameter *depth-cutoff* 0)
  ;The max possible number of steps to consider toward any goal.
  ;Negative or 0 means no cutoff, value set in problem.lisp.

(defparameter *start-state* (make-problem-state))

(defparameter *tree-or-graph* 'graph)
  ;Whether there are repeated states (graph) or not (tree); try both

(defparameter *search-tree* nil)
  ;The dfs summary representation of the search

(defparameter *solutions* nil)
  ;The resulting list of solutions found.


(defparameter *types* (make-hash-table :test #'eq))

(defparameter *relations* (make-hash-table :test #'eq))  ;dynamic relations

(defparameter *static-relations* (make-hash-table :test #'eq))

(defparameter *connectives* '(and or not))

(defparameter *derived* (make-hash-table :test #'eq))

(defparameter *function-names* nil)  ;list of all user-defined functions

(defparameter *symmetrics* (make-hash-table :test #'eq))  ;symmetric relations

(defparameter *actions* nil)  ;list of all potential actions

(defparameter *init-actions* nil)  ;list of all initialization actions

(defparameter *complements* (make-hash-table :test #'eq))

(defparameter *current-precondition-fluents* nil)  ;list of fluents appearing in a precondition

(defparameter *current-effect-fluents* nil)  ;list of fluents appearing in an effect

(defparameter *fluent-relation-indices* (make-hash-table :test #'eq))  ;list of fluent argument indices for a relation

(defparameter *happenings* nil)  ;the list of objects having exogenous events

(defparameter *db* (make-hash-table :test #'equal))  ;initial database of dynamic relations

(defparameter *static-db* (make-hash-table :test #'equal))  ;the database of static relations

(defparameter *static-idb* (make-hash-table))  ;the integer database of static relations

(defparameter *goal* nil)
  ;Holds the goal test function--value is a lambda expression, symbol-function is the function

(defparameter *constraint* nil)  ;any constraint test function

(defparameter *constant-integers* (make-hash-table))  ;integer codes for the problem's object constants

(defparameter *integer-constants* (make-hash-table))  ;translating codes back to constants for printout

(defparameter *last-object-index* 0)  ;last index of object constants seen so far in propositions


(defun setup ()
  (format t "Setting up...~%")
  (setf *actions* (nreverse *actions*))  ;prioritize actions to problem spec
  (dolist (action *actions*)
    (install-precondition-lits action)
    (install-effect-adds action))
  (setf *init-actions* (nreverse *init-actions*))
  (with-slots (name instantiations happenings time) *start-state*
    (let ((first-event-time (loop for object in *happenings* 
                              minimize (car (aref (get object :events) 0)))))
            (setf happenings (loop for object in *happenings* ;property list of happening objects
                           collect (list object 
                                        (list 0 first-event-time +1))))  ;next (index time direction)
            (setf time 0)
            (setf instantiations nil)
            (setf name nil)))
  (when (> (length *happenings*) 1)
    (setq *happenings* (sort *happenings* #'< :key (lambda (object)
                                                     (first (aref (get object :events) 0))))))
  (do-init-action-updates *start-state*)  ;updates *db* & *static-db* but not *start-state*
  (do-integer-conversion)  ;allows integer hashtable db lookups
  (convert-lambdas-to-fns))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Type Specifiers ;;;;;;;;;;;;;;;;;;


(defun list-of-variables (lst)
  (and (listp lst)
       (every #'varp lst)))

(defun type-description (descrip)
  (declare (hash-table *types*))
  (or (nth-value 1 (gethash descrip *types*))
      (eql descrip 'fluent)
      (and ($varp descrip)
           (or (nth-value 1 (gethash (extract-type descrip) *types*))
               (symbolp (extract-type descrip))))
      (and (consp descrip)
           (eql (car descrip) 'either)
           (or (every (lambda (typ)
                        (gethash (extract-type typ) *types*))
                      (cdr descrip))))))

(defun list-of-parameter-types (lst)
  (declare (hash-table *types*))
  (and (listp lst)
       (every (lambda (typ)
                (and (symbolp typ)
                     (or (eql typ 'fluent)
                         (nth-value 1 (gethash typ *types*)))))
              lst)))

(defun relation (rel)
  (declare (hash-table *relations*))
  (and (listp rel)
       (iter (for rel-item in (cdr rel))
             (for rel-type in (gethash (car rel) *relations*))
             (unless (or (eq rel-item rel-type)
                         (and (listp rel-type)
                              (member rel-item (cdr rel-type)))  ;either type
                         (subsetp (gethash rel-item *types*) (gethash rel-type *types*)))
                         ;(alexandria:set-equal rel-item rel-type))
               (return nil))
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
  (declare (hash-table *types* *relations* *static-relations*))
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
       (member (car form) *function-names*)
       (every (lambda (arg)
                (or (varp arg)
                    (symbolp arg)
                    (realp arg)))
              (cdr form))))
