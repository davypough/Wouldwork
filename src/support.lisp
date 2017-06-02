;;; Filename: support.lisp

;;; Support functions for planning.


(in-package :ww)


(defun either (&rest types)
  (loop for type in types
      append (gethash type *types*)))


(defun distinct-instantiations (types)
  (declare (list types) (special *types*))
  (if types
    (let* ((objects (map 'list #'(lambda (type)
                                   (if (listp type) ;"either" multi-type
                                     (loop for typ in (cdr type)
                                           append (gethash typ *types*))
                                     (gethash type *types*)))
                               types))
           (all-instantiations (apply #'alexandria:map-product #'list objects))
           ;next get list of all possible unique variable instantiations
           (distinct-instantiations (delete-if-not #'alexandria:setp 
                                                   all-instantiations)))
      distinct-instantiations)
    (list nil)))


(defun achieve-goal (db)
  (funcall (symbol-function '*goal*) db))


(defun symmetric-type-indexes (types)
  ;Returns the set of type indexes for the multi-types of a symmetric relation.
  (loop for remaining on types
      for i from 0
      collect (loop for type in (cdr remaining)
                  for j from (1+ i)
                  when (eq (car remaining) type)
                  collect j into indices
                  finally (when indices
                            (push i indices))
                          (return indices)) into index-sets
      finally (return (first (ut::delete-subsets (remove nil index-sets))))))


(defun convert-lambda-lists-to-fns ()
  (declare (special *actions* *constraint* *goal*))
  (format t "Converting lambda lists to functions...")
  (dolist (action *actions*)
    (with-slots (precondition precondition-lambda effect effect-lambda) action
      (setf precondition (coerce precondition-lambda 'function))
      (setf effect (coerce effect-lambda 'function))))
  (when *constraint*
    (setf (symbol-function '*constraint*)
      (coerce *constraint* 'function)))
  (setf (symbol-function '*goal*)
    (coerce *goal* 'function)))


(defun compile-lambda-fns ()
  (declare (special *actions* *constraint* *function-names* *goal*))
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
  (when *function-names*
   (format t "~&Compiling functions...")
    (loop for fn-name in *function-names* do
          (setf (symbol-function fn-name)
            (compile nil (symbol-function fn-name)))))
  (format t "~&Compiling goal...")
  (setf (symbol-function '*goal*)
    (compile nil (symbol-function '*goal*))))


(defun get-prop-key&values (proposition)
  (declare (special *relations* *types*))
  (loop for prop-item in proposition
      for rel-item in (cons (car proposition)
                            (gethash (car proposition) *relations*))
        if (or (eq prop-item rel-item)   ;it's the leading predicate
               (member prop-item (gethash rel-item *types*)))  ;it's a constant
           collect prop-item into key
        else if (varp #\! rel-item)
                if (let ((value-type (extract-type rel-item)))
                     (or (typep prop-item value-type)  ;it's a lisp type, eg real
                         (member prop-item (gethash value-type *types*))))  ;it's a problem type
                   collect prop-item into values
                else do (error "In get-prop-key&values: ~a ~a" prop-item rel-item)
        finally (return (list key values))))


(defun add-prop (proposition db)
  ;Adds a literal proposition to the database.
  (let ((key (cull-fluents proposition))
        (fluents (get-fluents proposition)))
    (if (null fluents)
        (setf (gethash key db) t)
      (setf (gethash key db) fluents))))


(defun del-prop (proposition db)
  (let ((key (cull-fluents proposition))
        (fluents (get-fluents proposition)))
    (if (null fluents)
        (remhash proposition db)
      (remhash key db))))
  

(defun add-proposition (proposition db)
  ;Adds all the symmetries of a proposition to the database.
  (declare (hash-table db *symmetrics*) (special *symmetrics*))
  (let ((symmetric-indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric-indexes)
      (add-prop proposition db)
      (let ((symmetric-instances (loop for index in symmetric-indexes
                                     collect (nth index (cdr proposition)))))
        (alexandria:map-permutations
          #'(lambda (perm)
              (loop for instance in perm
                    for index in symmetric-indexes
                    do ;(ut::prt instance index)
                       (setf (nth (1+ index) proposition) instance)
                    finally (add-prop (copy-list proposition) db)))
          symmetric-instances)))))


(defun delete-proposition (proposition db)
  (declare (hash-table db *symmetrics*) (special *symmetrics*))
  (let ((symmetric-indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric-indexes)
      (del-prop proposition db)  ;not symmetric proposition
      (let ((symmetric-instances (loop for index in symmetric-indexes
                                       collect (nth index (cdr proposition)))))
        (alexandria:map-permutations
          #'(lambda (perm)
              (loop for instance in perm
                    for index in symmetric-indexes
                    do (setf (nth (1+ index) proposition) instance)
                    finally (del-prop proposition db)))
         symmetric-instances)))))


(defun apply-effect (einsts fluent-values action state)
  ;Returns list of new db updates with einsts.
    (let* ((update (apply (action-effect action)
                          `(,state ,@einsts ,@fluent-values)))
           (new-einsts (map 'list #'(lambda (sym)
                                      (ut::walk-tree-until (lambda (x)
                                                             (eq x sym))
                                                           update))
                             einsts)))
      (list update new-einsts)))


(defun order-update (db-update)
  ;NOTs first so addhash db not removed by later remhash
  (list (order-propositions (first db-update))
        (second db-update)))


(defun order-propositions (props)
  ;NOTs first so addhash db not removed by later remhash
  (sort props #'(lambda (x y) 
                  (declare (ignore y))
                  (and (listp x) (eq (car x) 'not)))))


(defun revise (db literals)
  ;Use for simple list of literals.
  (loop for literal in literals
      do (if (eq (car literal) 'not)
             (delete-proposition (cadr literal) db)
           (add-proposition literal db))
      finally (return db)))


(defun complement-literals (literals)
  (order-propositions
   (map 'list #'(lambda (lit)
                  (if (eq (car lit) 'not)
                      (cadr lit)
                    (list 'not lit)))
     literals)))


(defun assign-fluents (fluents fluent-accessor db)
  ;Assigns fluents to their current values.
  (loop for fluent in fluents
      for value in ;(or 
                   (gethash fluent-accessor db)
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
  (convert-lambda-lists-to-fns)
  (compile-lambda-fns))


(defun expand-into-plist (parameters)
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


(defun varp (lead-char var)
  (and (symbolp var)
       (char= (elt (symbol-name var) 0) lead-char)))


(defun get-vars (lead-char form)
  ;Returns all vars in form starting with lead-char.
    (remove-if-not #'(lambda (item)
                       (and (symbolp item)
                            (char= (elt (symbol-name item) 0) lead-char)))
                   form))


(defun extract-type (value-symbol)
  (intern (subseq (symbol-name value-symbol) 1)))


(defun get-fluents (form)
  (loop with rel = (car form)
      with rel-args = (gethash rel *relations*)
      for arg in (cdr form)
      for rel-arg in rel-args
      when (or (varp #\$ arg) (varp #\! rel-arg))
        collect arg))


(defun cull-fluents (form)
  (loop with rel = (car form)
      with rel-args = (gethash rel *relations*)
      for arg in (cdr form)
      for rel-arg in rel-args
      when (not (or (varp #\$ arg) (varp #\! rel-arg)))
      collect arg into nonfluents
        finally (return (cons rel nonfluents))))


(defun form-has-fluents (form)
  (loop with rel = (car form)
      with rel-args = (gethash rel *relations*)
      for arg in (cdr form)
      for rel-arg in rel-args
      when (or (varp #\$ arg) (varp #\! rel-arg))
        do (return t)))

