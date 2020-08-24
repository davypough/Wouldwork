;;; Filename: support.lisp

;;; Support functions for planning.


(in-package :ww)


(defun either (&rest types)
  (loop for type in types
      append (gethash type *types*)))


(defun achieve-goal (db)
  (funcall (symbol-function '*goal*) db))


(defun symmetric-type-indexes (types)
  ;Returns the set of type indexes for the multi-types of a symmetric relation.
  (let ((dups (remove-duplicates types)))
    (loop for dup in dups
       collect (loop for type in types
                     for i from 0
                     when (eql type dup)
                     collect i) into indices
      finally (return (remove-if (lambda (elt) (= (length elt) 1))
                                 indices)))))


(defun convert-ilambdas-to-fns ()
  (format t "~&Converting lambda expressions to functions...~%")
  (iter (for fname in (append *query-names* *update-names*))
        (format t "~&~A...~%" fname)
        (when (fboundp `,fname)
          (fmakunbound `,fname))
        (eval `(defun ,fname ,@(cdr (eval fname))))
        (compile `,fname))
  (when *constraint*
    (format t "~&~A...~%" '*constraint*)
    (setf (symbol-function '*constraint*)
      (compile nil *constraint*)))
  (iter (for object in *happenings*)
        (format t "~&~A...~%" object)
        (setf (get object :interrupt-fn) (compile nil (eval object))))
  (format t "~&~A...~%" '*goal*)
  (setf (symbol-function '*goal*)
    (compile nil *goal*))
  (dolist (action *actions*)
    (format t "~&~A...~%" (action-name action))
    (with-slots (iprecondition-lambda iprecondition ieffect-lambda ieffect) action
      (setf iprecondition (compile nil iprecondition-lambda))
      (setf ieffect (compile nil ieffect-lambda)))))


(defun add-prop (proposition db)
  ;Effectively adds a literal proposition to the database.
  (declare (hash-table db))
  (let ((int-db (eq (hash-table-test db) 'eql)))
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
  ;Effectively removes a literal proposition from the database.
  (let ((int-db (eq (hash-table-test db) 'eql)))
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
  ;Adds a proposition and all its symmetries to the database.
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
  ;Collects new propositions for each prop in propositions.
  (loop for prop in propositions
        append (generate-proposition-permutations vars prop idxs)))


(defun generate-proposition-permutations (vars proposition idxs)
  ;Returns list of propositions generated from given proposition replacing
  ;items at indices with vars, respectively.
  (let (propositions)
    (alexandria:map-permutations
      (lambda (perm)
        (push (ut::subst-items-at-ascending-indexes perm (mapcar #'1+ idxs) proposition) propositions))
      vars)
    propositions))


(defun order-propositions (db-update)
  ;NOTs first so addhash db not removed by later remhash
  (ut::sortf (update-changes db-update) #'(lambda (x y) 
                                            (declare (ignore y))
                                            (and (listp x) (eq (car x) 'not))))
  db-update)


(defun revise (db literals)
  ;Use for simple list of literals.
  (declare (hash-table db))
  (loop for literal in literals
      do (update db literal)
      finally (return db)))


(defun update (db literal)
  ;Single add or delete from db.
  (declare (hash-table db))
  (if (eq (car literal) 'not)
    (delete-proposition (second literal) db)
    (add-proposition literal db)))

#|
(defun update2 (literal)
  (if (eq (car literal) 'not)
    (if (gethash (cadr literal) *relations*)
      (delete-proposition (second literal) *db*)
      (delete-proposition (second literal) *static-db*))
    (if (gethash (car literal) *relations*)
      (add-proposition literal *db*)
      (add-proposition literal *static-db*))))
|#

(defun commit1 (db literal)
  (when-debug>= 4
    (format t "~&    COMMIT => ~A" literal))
  (update db literal))


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


(defun sort-either-types (relation)
  ;Alphabetically sorts the 'either' types in a relation.
  (mapcar (lambda (item)  ;cannonically orders 'either' types
            (if (symbolp item)
              item
              (cons 'either (sort (cdr item) #'string< 
                                  :key #'symbol-name))))
          relation))
            
            
(defun expand-into-plist (parameters)
  ;Return alternating plist of variable/type from input parameter list.
  (loop for (vars type) on parameters by #'cddr
      if (listp vars)
      append (ut::intersperse type vars) into plist
      else append (list vars type) into plist
        finally (return plist)))


(defun fluentp (item)
  (or (numberp item)
      ($varp item)
      (and (listp item) (fboundp (car item)))))


(defun $varp (item)
  (and (symbolp item)
       (char= (elt (symbol-name item) 0) #\$)))


(defun ?varp (item)
  (and (symbolp item)
       (char= (elt (symbol-name item) 0) #\?)))


(defun varp (sym)
  (or (?varp sym)
      ($varp sym)))


(defun final-charp (final-char var)
  (and (symbolp var)
       (let ((str (symbol-name var)))
         (char= (elt str (1- (length str))) final-char))))


(defun get-vars (lead-char form)
  ;Returns all vars in form starting with lead-char.
    (remove-if-not #'(lambda (item)
                       (and (symbolp item)
                            (char= (elt (symbol-name item) 0) lead-char)))
                   form))


(defun different (sym1 sym2)
  (if (and (symbolp sym1) (symbolp sym2))
    (not (eql sym1 sym2))
    (error "Arguments must be symbols: ~A ~A" sym1 sym2)))


(defun extract-type (value-symbol)
  (if ($varp value-symbol)
      (intern (subseq (symbol-name value-symbol) 1))
    value-symbol))


(defun get-prop-fluent-indices (proposition)
  (gethash (car proposition) *fluent-relation-indices*))


(defun get-prop-fluents (proposition)
  ;Returns the fluent values in an arbitrary proposition.
  (let ((indices (get-prop-fluent-indices proposition)))
    (when indices
      (mapcar (lambda (index)
                (let ((item (nth index proposition)))
                  (if (and (symbolp item) (boundp item))
                    (symbol-value item)
                    item)))
        indices))))


(defun get-fluentless-prop (proposition)
  ;Derives the fluentless proposition counterpart from a full proposition.
  (let ((indices (get-prop-fluent-indices proposition)))
    (ut::remove-at-indexes indices proposition)))


(defun get-complement-prop (proposition)
  ;Derives the complement proposition counterpart from a given proposition.
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
              (eq (car type) 'either))
        collect (let* ((combo-instances (remove-duplicates 
                                          (loop for typ in (cdr type)
                                                append (gethash typ *types*))))
                       (sorted-types (sort (cdr type) #'string< :key #'symbol-name))
                       (combo-type (intern (ut::interleave+ sorted-types))))
                  (setf (gethash combo-type *types*) combo-instances)
                  combo-type)
      else collect type))


(defun type-instantiations (symbol-types restriction state)
  ;Returns lists of possible variable instantiations for a list of (only) type symbols.
  ;May restrict symbol types to combinations or dot-products.
  (when symbol-types
    (let* ((nonfluent-types (remove 'fluent symbol-types))
           (instances (mapcar (lambda (item)
                                (let ((name (car (gethash item *types*))))
                                  (if (member name *query-names*)
                                    (funcall name state)
                                    (gethash item *types*))))
                              nonfluent-types)))
      (when (member nil instances)
        (return-from type-instantiations nil))
      (if (eq restriction 'dot-products)
        (apply #'mapcar #'list instances)
        (let* ((product-instances (apply #'alexandria:map-product 'list instances))
               (set-instances (get-set-instances nonfluent-types product-instances)))
          (if (eq restriction 'combinations)
            (or (delete-duplicates set-instances :test #'alexandria:set-equal)
                (make-list (length nonfluent-types) :initial-element nil))
            (or set-instances
                (make-list (length nonfluent-types) :initial-element nil))))))))


(defun get-set-instances (symbol-types product-instances)
  ;Culls out duplicate instances of the same type from product-instances.
  (iter (for product-instance in product-instances)
        (unless (duplicate-product-instance symbol-types product-instance)
          (collect product-instance))))


(defun duplicate-product-instance (symbol-types product-instance)
  ;Tests whether a product-instance contains duplicate type assignments of constants.
  (iter (for common-values in (get-common-values symbol-types product-instance))
        (when (not (alexandria:setp common-values))  ;duplicate values
          (return t))
        (finally (return nil))))


(defun get-common-values (symbol-types product-instance)
  ;Returns a set of product-instance values for equivalent types in symbol-types.
  (iter (with types-set = (remove-duplicates symbol-types))
        (for type in types-set)
        (collect (iter (for sym-type in symbol-types)
                       (for value in product-instance)
                       (for i from 0)
                       (when (eq sym-type type)
                         (collect value))))))


(defun dissect-parameters (parameter-list)
  ;Returns a list of primitive parameter variables and types.
  (let ((restriction (case (car parameter-list)
                       (combinations 'combinations)
                       (dot-products 'dot-products))))
    (destructuring-bind (variables types)
        (ut::segregate-plist (expand-into-plist (if restriction
                                                  (cdr parameter-list)
                                                  parameter-list)))
      (list variables (consolidate-types types) restriction))))


(defun duplicate-db-entry-test (predicate object db)
  (loop with partial-keys
      for key being the hash-keys of db
      do (let ((partial-key (list (first key) (second key))))
           (if (and (eql (first key) predicate)
                    (eql (second key) object)
                    (member partial-key partial-keys :test #'equal))
            (return-from duplicate-db-entry-test t)
            (push partial-key partial-keys)))))


(defun get-all-vars (fn tree)
  "Selects one each of variables in the tree satisfying fn."
  (remove-duplicates (remove-if-not fn (alexandria:flatten tree))))


(defun get-bound-?vars (tree)
  "Searches tree for an atom or cons satisfying the predicate, and returns
   list of all such items. To include tree itself, pass in (list tree)."
  (iter (for item in tree)
        (when (consp item)
          (if (member (car item) '(exists exist forsome forall forevery doall))
            (nconcing (delete-if-not #'?varp (alexandria:flatten (second item))))
            (nconcing (get-bound-?vars item))))))


(defun fix-if-ignore (symbols lambda-expr)
  "Ignores variable symbols that are not in the lambda-body."
  (let ((ignores (ut::list-difference
                    symbols (get-all-vars (lambda (x)
                                            (member x symbols))
                                          (cddr lambda-expr)))))
    (when ignores
      (push `(declare (ignorable ,@ignores)) (cddr lambda-expr)))))
