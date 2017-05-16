;;; Filename: support.lisp

;;; Support functions for planning.


(in-package :pl)


(defun either (&rest types)
  (loop for type in types
      append (gethash type *types*)))


(defun distinct_instantiations (types)
  (declare (list types) (special *types*))
  (if types
    (let* ((objects (map 'list #'(lambda (type)
                                   (if (listp type) ;"either" multi-type
                                     (loop for typ in (cdr type)
                                           append (gethash typ *types*))
                                     (gethash type *types*)))
                               types))
           (all_instantiations (apply #'alexandria:map-product #'list objects))
           ;next get list of all possible unique variable instantiations
           (distinct_instantiations (delete-if-not #'alexandria:setp 
                                                   all_instantiations)))
      distinct_instantiations)
    (list nil)))


(defun achieve_goal (db)
  (funcall (symbol-function '*goal*) db))


(defun symmetric_type_indexes (types)
  ;Returns the set of type indexes for the multi-types of a symmetric relation.
  (loop for remaining on types
      for i from 0
      collect (loop for type in (cdr remaining)
                  for j from (1+ i)
                  when (eq (car remaining) type)
                  collect j into indices
                  finally (when indices
                            (push i indices))
                          (return indices)) into index_sets
      finally (return (first (ut::delete-subsets (remove nil index_sets))))))


(defun convert_lambda_lists_to_fns ()
  (declare (special *actions* *constraint* *goal*))
  (format t "Converting lambda lists to functions...")
  (dolist (action *actions*)
    (with-slots (precondition precondition_lambda effect effect_lambda) action
      (setf precondition (coerce precondition_lambda 'function))
      (setf effect (coerce effect_lambda 'function))))
  (when *constraint*
    (setf (symbol-function '*constraint*)
      (coerce *constraint* 'function)))
  (setf (symbol-function '*goal*)
    (coerce *goal* 'function)))


(defun compile_lambda_fns ()
  (declare (special *actions* *constraint* *function_names* *goal*))
  (dolist (action *actions*)
    (with-slots (name precondition effect) action
      (format t "~&Compiling ~A precondition..." name)
      (setf precondition (compile nil precondition))
      (format t "~&Compiling ~A effect..." name)
      (setf effect (compile nil effect))))
  (when *constraint*
    (format t "~&Compiling constraint...")
    (setf (symbol-function '*constraint*)
      (compile nil (symbol-function '*constraint*))))
  (when *function_names*
   (format t "~&Compiling functions...")
    (loop for fn-name in *function_names* do
          (setf (symbol-function fn-name)
            (compile nil (symbol-function fn-name)))))
  (format t "~&Compiling goal...")
  (setf (symbol-function '*goal*)
    (compile nil (symbol-function '*goal*))))


(defun get_prop_key&values (proposition)
  (declare (special *relations* *types*))
  (loop for prop_item in proposition
      for rel_item in (cons (car proposition)
                            (gethash (car proposition) *relations*))
        if (or (eq prop_item rel_item)   ;it's the leading predicate
               (member prop_item (gethash rel_item *types*)))  ;it's a constant
           collect prop_item into key
        else if (varp #\! rel_item)
                if (let ((value_type (extract_type rel_item)))
                     (or (typep prop_item value_type)  ;it's a lisp type, eg real
                         (member prop_item (gethash value_type *types*))))  ;it's a problem type
                   collect prop_item into values
                else do (error "In get_prop_key&values: ~a ~a" prop_item rel_item)
        finally (return (list key values))))


(defun add_prop (proposition db)
  ;Adds a literal proposition to the database.
  (let ((key (remove-if #'realp proposition))
        (fluents (remove-if-not #'realp proposition)))
    (if (null fluents)
        (setf (gethash key db) t)
      (setf (gethash key db) fluents))))


(defun del_prop (proposition db)
  (let ((key (remove-if #'numberp proposition))
        (fluents (remove-if-not #'numberp proposition)))
    (if (null fluents)
        (remhash proposition db)
      (remhash key db))))
  

(defun add_proposition (proposition db)
  ;Adds all the symmetries of a proposition to the database.
  (declare (hash-table db *symmetrics*) (special *symmetrics*))
  (let ((symmetric_indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric_indexes)
      (add_prop proposition db)
      (let ((symmetric_instances (loop for index in symmetric_indexes
                                     collect (nth index (cdr proposition)))))
        (alexandria:map-permutations
          #'(lambda (perm)
              (loop for instance in perm
                    for index in symmetric_indexes
                    do ;(ut::prt instance index)
                       (setf (nth (1+ index) proposition) instance)
                    finally (add_prop (copy-list proposition) db)))
          symmetric_instances)))))


(defun delete_proposition (proposition db)
  (declare (hash-table db *symmetrics*) (special *symmetrics*))
  (let ((symmetric_indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric_indexes)
      (del_prop proposition db)  ;not symmetric proposition
      (let ((symmetric_instances (loop for index in symmetric_indexes
                                       collect (nth index (cdr proposition)))))
        (alexandria:map-permutations
          #'(lambda (perm)
              (loop for instance in perm
                    for index in symmetric_indexes
                    do (setf (nth (1+ index) proposition) instance)
                    finally (del_prop proposition db)))
         symmetric_instances)))))


(defun apply_effect (einsts fluent_values action state)
  ;Returns list of new db updates with einsts.
    (let* ((update (apply (action-effect action)
                          `(,state ,@einsts ,@fluent_values)))
           (new_einsts (map 'list #'(lambda (sym)
                                      (ut::walk-tree-until (lambda (x)
                                                             (eq x sym))
                                                           update))
                             einsts)))
      (list update new_einsts)))


(defun order_update (db_update)
  ;NOTs first so addhash db not removed by later remhash
  (list (order_propositions (first db_update))
        (second db_update)))


(defun order_propositions (props)
  ;NOTs first so addhash db not removed by later remhash
  (sort props #'(lambda (x y) 
                  (declare (ignore y))
                  (and (listp x) (eq (car x) 'not)))))


(defun revise (db literals)
  ;Use for simple list of literals.
  (loop for literal in literals
      do (if (eq (car literal) 'not)
             (delete_proposition (cadr literal) db)
           (add_proposition literal db))
      finally (return db)))


(defun complement_literals (literals)
  (order_propositions
   (map 'list #'(lambda (lit)
                  (if (eq (car lit) 'not)
                      (cadr lit)
                    (list 'not lit)))
     literals)))


(defun assign_fluents (fluents fluent_accessor db)
  ;Assigns fluents to their current values.
  (loop for fluent in fluents
      for value in ;(or 
                   (gethash fluent_accessor db)
                   ;(make-list (length fluents) :initial-element 0))
      do (cond ((symbolp fluent) (set fluent value))
               ((realp fluent) (when (not (= fluent value)) (return nil)))
               (t (error "Given fluent not a $var or real number: ~A" fluent)))
      finally (return t)))


(defun match (proposition propositions)
  (member proposition propositions :test #'equal))
            
            
(defun setup ()
  (format t "Initializing...~%")
  (when (> (length *happenings*) 0)
    (setq *happenings* (sort *happenings* #'< :key #'car)))
  (convert_lambda_lists_to_fns)
  (compile_lambda_fns))


(defun expand_into_plist (parameters)
  ;Return alternating plist of variable/type from input parameter list.
  (loop for (vars type) on parameters by #'cddr
      if (listp vars)
      append (ut::intersperse type vars) into plist
      else append (list vars type) into plist
      finally (if (and (every #'symbolp plist) (evenp (length plist)))
                  (return plist)
                (error "Action parameter list is ill-formed: ~A" parameters))))


(defun fluentp (item)
  (or (numberp item) (varp #\$ item)))


(defun varp (lead_char var)
  (and (symbolp var)
       (char= (elt (symbol-name var) 0) lead_char)))


(defun get_vars (lead_char form)
  ;Returns all vars in form starting with lead_char.
    (remove-if-not #'(lambda (item)
                       (and (symbolp item)
                            (char= (elt (symbol-name item) 0) lead_char)))
                   form))


(defun extract_type (value_symbol)
  (intern (subseq (symbol-name value_symbol) 1)))


(defun get_fluents (form)
  (remove-if-not #'fluentp form))


(defun cull_fluents (form)
  (remove-if #'fluentp form))


(defun form_has_fluents (form)
  (find-if #'fluentp form))

