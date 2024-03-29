;;; Filename: support.lisp

;;; Support functions for planning.


(in-package :ww)


(defun delete-actions (&rest names)
  "Deletes named actions from *actions* at run-time."
  (setf *actions* (delete-if (lambda (name)
                               (member name names))
                             *actions*
                             :key #'action.name)))


(defun get-state-codes ()
  "User calls this after finding backwards *solutions*."
  (format t "~%Working ...~%")
  (clrhash *state-codes*)
  (iter (for soln in *solutions*)
        (for path = (solution.path soln))
        (for db-props = (list-database (problem-state.idb (solution.goal soln))))
        (setf (gethash (funcall 'encode-state db-props) *state-codes*) path))
  *state-codes*)


(defun backward-path-exists (state)
  "Use in forward search goal to check existence of backward path."
  (gethash (funcall 'encode-state (list-database (problem-state.idb state))) *state-codes*))


(defmacro when-debug>= (n &rest expressions)
  "Inserts debugging expressions when *debug* >= n, otherwise NIL"
  `(when (>= *debug* ,n)
     ,@expressions))


(defun add-prop (proposition db)
  "Effectively adds a literal proposition to the database."
  (declare (hash-table db))
  (let ((int-db (eql (hash-table-test db) 'eql)))
    (if (get-prop-fluent-indices proposition)
      ;do if proposition has fluents
      (if int-db
        (setf (gethash (convert-to-integer (get-fluentless-prop proposition)) db) 
              (get-prop-fluents proposition))
        (setf (gethash (get-fluentless-prop proposition) db)
              (get-prop-fluents proposition)))
      ;do for non-fluent propositions
      (if int-db
        (setf (gethash (convert-to-integer proposition) db) t)  
        (setf (gethash proposition db) t)))
    ;do if proposition has a complement
    (when (gethash (car proposition) *complements*)
      (if int-db
        (remhash (convert-to-integer (get-complement-prop proposition)) db)
        (remhash (get-complement-prop proposition) db)))))
  

(defun del-prop (proposition db)
  "Effectively removes a literal proposition from the database."
  (let ((int-db (eql (hash-table-test db) 'eql)))
    (if (get-prop-fluent-indices proposition)
      ;do if proposition has fluents
      (if int-db
        (remhash (convert-to-integer (get-fluentless-prop proposition)) db)
        (remhash (get-fluentless-prop proposition) db)))
      ;do for non-fluent propositions
      (if int-db
        (remhash (convert-to-integer proposition) db)
        (remhash proposition db))
    ;do if proposition has a complement
    (when (gethash (car proposition) *complements*)
      (if int-db
        (setf (gethash (convert-to-integer (get-complement-prop proposition)) db) t)
        (setf (gethash (get-complement-prop proposition) db) t)))))
  

(defun add-proposition (proposition db)  
  "Adds a proposition and all its symmetries to the database."
  (declare (hash-table db))
  (let ((symmetric-indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric-indexes)
      (add-prop proposition db)
      (let ((symmetric-variables
               (loop for indexes in symmetric-indexes
                     collect (loop for index in indexes
                                 collect (nth index (cdr proposition)))))
            (props (list (copy-list proposition))))
        (loop for vars in symmetric-variables
              for idxs in symmetric-indexes do
              (setf props (generate-new-propositions vars props idxs)))
        (loop for prop in props do
              (add-prop prop db))))))

              
(defun delete-proposition (proposition db)
  (declare (hash-table db))
  (let ((symmetric-indexes (gethash (car proposition) *symmetrics*)))
    (if (null symmetric-indexes)
      (del-prop proposition db)
      (let ((symmetric-variables
               (loop for indexes in symmetric-indexes
                     collect (loop for index in indexes
                                 collect (nth index (cdr proposition)))))
            (props (list (copy-list proposition))))
        (loop for vars in symmetric-variables
              for idxs in symmetric-indexes do
              (setf props (generate-new-propositions vars props idxs)))
        (loop for prop in props do
              (del-prop prop db))))))


(defun generate-new-propositions (vars propositions idxs)
  "Collects new propositions for each prop in propositions."
  (loop for prop in propositions
        append (generate-proposition-permutations vars prop idxs)))


(defun generate-proposition-permutations (vars proposition idxs)
  "Returns list of propositions generated from given proposition replacing
   items at indices with vars, respectively."
  (let (propositions)
    (alexandria:map-permutations
      (lambda (perm)
        (push (ut::subst-items-at-ascending-indexes perm (mapcar #'1+ idxs) proposition) propositions))
      vars)
    propositions))


(defun revise (db literals)
  "Updates a database given a simple list of literals."
  (declare (hash-table db))
  (loop for literal in literals
      do (update db literal)
      finally (return db)))


(defun update (db literal)
  "Single add or delete from db."
  (declare (hash-table db))
  (if (eql (car literal) 'not)
    (delete-proposition (second literal) db)
    (add-proposition literal db))
  db)


;(defun commit1 (db literal)
;  (when (>= *debug* 4)
;    (format t "~&    COMMIT => ~A" literal))
;  (update db literal))


(defun expand-into-plist (parameters)
  "Return alternating plist of variable/type from input parameter list."
  (loop for (vars type) on parameters by #'cddr
      if (listp vars)
      append (ut::intersperse type vars) into plist
      else append (list vars type) into plist
        finally (return plist)))


(defun different (sym1 sym2)
  (if (and (symbolp sym1) (symbolp sym2))
    (not (eql sym1 sym2))
    (error "Arguments must be symbols: ~A ~A" sym1 sym2)))


(defun get-fluentless-prop (proposition)
  "Derives the fluentless proposition counterpart from a full proposition."
  (let ((indices (get-prop-fluent-indices proposition)))
    (ut::remove-at-indexes indices proposition)))


(defun get-complement-prop (proposition)
  "Derives the complement proposition counterpart from a given proposition."
  (let* ((predicate (car proposition))
         (joint-patterns (gethash predicate *complements*))
         (prop-pattern (first joint-patterns))
         (comp-pattern (copy-tree (second (second joint-patterns)))))
    (loop for const in (cdr proposition)
            for pat in (cdr prop-pattern)
            when (member pat comp-pattern :test #'equal)
              do (nsubst const pat comp-pattern :test #'equal)
            finally (return comp-pattern))))


(defun consolidate-types (types)
  (loop for type in types
      if (and (listp type)
              (eql (car type) 'either))
        collect (let* ((combo-instances (remove-duplicates 
                                          (loop for typ in (cdr type)
                                                append (gethash typ *types*))))
                       (sorted-types (sort (cdr type) #'string< :key #'symbol-name))
                       (combo-type (intern (ut::interleave+ sorted-types))))
                  (setf (gethash combo-type *types*) combo-instances)
                  combo-type)
      else collect type))


(defun get-set-instances (symbol-types product-instances)
  "Culls out duplicate instances of the same type from product-instances."
  (iter (for product-instance in product-instances)
        (unless (duplicate-product-instance symbol-types product-instance)
          (collect product-instance))))


(defun duplicate-product-instance (symbol-types product-instance)
  "Tests whether a product-instance contains duplicate type assignments of constants."
  (iter (for common-values in (get-common-values symbol-types product-instance))
        (unless (alexandria:setp common-values)  ;duplicate values
          (return t))
        (finally (return nil))))


(defun get-common-values (symbol-types product-instance)
  "Returns a set of product-instance values for equivalent types in symbol-types."
  (iter (with types-set = (remove-duplicates symbol-types))
        (for type in types-set)
        (collect (iter (for sym-type in symbol-types)
                       (for value in product-instance)
                       (for i from 0)
                       (when (eql sym-type type)
                         (collect value))))))


(defun dissect-parameters (parameter-list)
  "Returns a list of primitive parameter variables and types."
  (let ((restriction (case (car parameter-list)
                       (products 'products)
                       (combinations 'combinations)
                       (dot-products 'dot-products))))
    (destructuring-bind (variables types)
        (ut::segregate-plist (expand-into-plist (if restriction
                                                  (cdr parameter-list)
                                                  parameter-list)))
      (list variables (consolidate-types types) restriction))))


(defun transform-types (user-types)
  "Followup processing of user types returned from dissect-parameters.
   Returns types for passing to a call to type-instantiations."
  (iter (for user-type in user-types)
    (for entry = (gethash user-type *types*))
    (collect (etypecase user-type
               (symbol (if ($varp user-type)
                         user-type
                         entry))
               (list user-type)))))


(defun instantiate-types (type-lists restriction)
  "Returns list of possible variable instantiations, given a list of type-lists.
   May restrict symbol types to products, combinations or dot-products."
  (when type-lists
    (if (eql restriction 'dot-products)
      (apply #'mapcar #'list type-lists)
      (let ((product-instances (apply #'alexandria:map-product 'list type-lists)))
        (if (eql restriction 'products)
          product-instances
         (let ((set-instances (delete-if-not #'alexandria:setp product-instances)))    ;(ut::prt type-lists product-instances set-instances)
           (if (eql restriction 'combinations)
             (or (delete-duplicates set-instances :test #'alexandria:set-equal)
                 (make-list (length type-lists) :initial-element nil))
             (if (null restriction)
               (or set-instances
                   (make-list (length type-lists) :initial-element nil))
               (error "~2%Unknown restriction label in the action rule: ~A~%" restriction)))))))))


;(defun duplicate-db-entry-test (predicate object db)
;  (loop with partial-keys
;      for key being the hash-keys of db
;      do (let ((partial-key (list (first key) (second key))))
;           (if (and (eql (first key) predicate)
;                    (eql (second key) object)
;                    (member partial-key partial-keys :test #'equal))
;            (return-from duplicate-db-entry-test t)
;            (push partial-key partial-keys)))))
