;;; Filename: installer.lisp

;;; Installs a user domain file.


(in-package :ww)


(defun either (&rest types)
  (loop for type in types
      append (gethash type *types*)))


(defmacro define-types (&rest types&constants)
  `(install-types ',types&constants))


(defun install-types (types&constants)
  (format t "~%Installing object types...~%")
  (iter (for (type constants) on types&constants by #'cddr)
    (check-type type symbol)
    (check-type constants list)
    (when (and (consp constants) (eql (car constants) 'either))
      (setf constants (apply #'either (cdr constants))))
    (when (eql (car constants) 'compute)
      (setf constants (eval (second constants))))
    (setf (gethash type *types*) constants)
    (when (consp constants)
      (dolist (constant constants)
        (check-type constant (or symbol real list))
        (when (symbolp constant)
          (setf (gethash (list 'something constant) *static-db*) t)
          (setf (gethash (list type constant) *static-db*) t))))))

;    (when (consp constants)
;      (appending constants into everything))
;    (finally (setf (gethash 'something *types*)  ;every constant is a something
;               (remove-duplicates everything :test #'eql)))))


(defun symmetric-type-indexes (types)
  ;Returns the set of type indexes for the multi-types of a symmetric relation.
  (let ((dups (remove-duplicates types)))
    (iter (for dup in dups)
      (collect (iter (for type in types)
                 (for i from 0)
                 (when (eql type dup)
                   (collect i)))
               into indices)
      (finally (return (remove-if (lambda (elt)
                                    (alexandria:length= 1 (length elt)))
                         indices))))))


(defun sort-either-types (relation)
  ;Alphabetically sorts the 'either' types in a relation.
  (mapcar (lambda (item)  ;cannonically orders 'either' types
            (if (symbolp item)
              item
              (cons 'either (sort (cdr item) #'string<
                                  :key #'symbol-name))))
          relation))
            
            
(defun final-charp (final-char var)
  (and (symbolp var)
       (let ((str (symbol-name var)))
         (char= (elt str (1- (length str))) final-char))))


(defmacro define-dynamic-relations (&rest relations)
  `(install-dynamic-relations ',relations))


(defun install-dynamic-relations (relations)
  (format t "Installing dynamic relations...~%")
  (iter (for relation in relations)
    (check-type relation cons)
    (check-type (car relation) symbol)
    (setf (gethash (car relation) *relations*)
      (ut::if-it (cdr relation)
        (sort-either-types ut::it)
        t))
    (ut::if-it (iter (for arg in (cdr relation))
                 (for i from 1)
                 (check-type arg (satisfies type-description))
                 (when ($varp arg)
                   (collect i)))
      (setf (gethash (car relation) *fluent-relation-indices*)
        ut::it))
    (finally (maphash #'(lambda (key val)  ;install implied unary relations
                          (declare (ignore val))
                          (setf (gethash key *static-relations*) '(something)))
               *types*)
      (add-proposition '(always-true) *static-db*)
      (setf (gethash 'always-true *static-relations*) '(always-true))))
  (iter (for (key val) in-hashtable *relations*)  ;install symmetric relations
    (when (and (not (eql val t))
               (not (alexandria:setp val))  ;multiple types
               (not (final-charp #\> key)))   ;not explicitly directed
      (setf (gethash key *symmetrics*) (symmetric-type-indexes val))))
  t)


(defmacro define-static-relations (&rest relations)
  `(install-static-relations ',relations))


(defun install-static-relations (relations)
  (format t "Installing static relations...~%")
  (iter (for relation in relations)
    (check-type relation cons)
    (check-type (car relation) symbol)
    (setf (gethash (car relation) *static-relations*)
      (ut::if-it (cdr relation) 
        (sort-either-types ut::it)
        nil))
    (ut::if-it (iter (for arg in (cdr relation))
                (for i from 1)
                (check-type arg (satisfies type-description))
                (when ($varp arg)
                  (collect i)))
      (setf (gethash (car relation) *fluent-relation-indices*) ut::it))
    (finally (maphash #'(lambda (key val)  ;install implied unary relations
                          (declare (ignore val))
                          (setf (gethash key *static-relations*) '(everything)))
               *types*)))
  (iter (for (key val) in-hashtable *static-relations*)  ;install symmetric relations
    (when (and (not (eql val t))
               (not (alexandria:setp val))  ;multiple types
               (not (final-charp #\> key)))   ;not explicitly directed
      (setf (gethash key *symmetrics*) (symmetric-type-indexes val))))
  (setf (gethash 'always-true *static-relations*) t)
  (setf (gethash 'waiting *static-relations*) t)
  t)


(defmacro define-complementary-relations (&rest positives->negatives)
  `(install-complementary-relations ',positives->negatives))


(defun install-complementary-relations (positives->negatives)
  (format t "Installing complementary relations...~%")
  (iter (for (positive nil negative) on positives->negatives by #'cdddr)
    (let ((ordered-pos (sort-either-types positive))
          (ordered-neg (list 'not (sort-either-types (second negative)))))
      (check-type ordered-pos (satisfies relation))
      (check-type ordered-neg (satisfies negative-relation)) 
      (setf (gethash (car positive) *complements*)
        (list ordered-pos ordered-neg)))))

        
(defmacro define-happening (object &rest plist)
  `(install-happening ',object ',plist))


(defun install-happening (object plist)
  (format t "Installing happening for ~A~%" object)
  (check-type object symbol)
  (check-type plist (satisfies key-list))
  (setf (symbol-plist object) nil)  ;overwrite any settings from a prior problem
  (makunbound object)
  (setf (the simple-vector (get object :events))
    (coerce (getf plist :events) 'simple-vector))
  (when (getf plist :inits)
    (setf (get object :inits) (getf plist :inits)))
  (ut::if-it (getf plist :interrupt)
    (let (($vars (get-all-nonspecial-vars #'$varp ut::it)))
      (setf (get object :interrupt) 
        `(lambda (state)
           (let ,$vars
             ,(when $vars
                `(declare (ignorable @,$vars)))
                ,(translate (getf plist :interrupt) 'pre))))))
  (fix-if-ignore '(state) (get object :interrupt))
  (when (getf plist :repeat)
    (setf (get object :repeat) (getf plist :repeat)))
  ;(when (get object :rebound) 
  ;  (setf (get object :rebound) `(lambda (state)
  ;                                 ,(translate (get object :rebound) 'pre)))
  ;  (setf (the function (get object :rebound-fn)) (compile nil (get object :rebound))))
  (dolist (literal (get object :inits))
    ;(check-type literal (satisfies literal))
    (when (eql (car literal) #+sbcl 'sb-int:quasiquote #+allegro 'excl::backquote)
      (setq literal (eval literal)))
    (if (eql (car literal) 'not)
      (when (gethash (caadr literal) *relations*)
        (delete-proposition (second literal) *hap-db*))
      (when (gethash (car literal) *relations*)
        (add-proposition literal *hap-db*))))
  (push object *happenings*))
           

(defmacro define-query (name args body)
  `(install-query ',name ',args ',body))


(defun install-query (name args body)
  (format t "Installing ~A query...~%" name)
  (check-type args (satisfies list-of-variables))
  (fmakunbound `,name)  ;avoids compiler warning if recursive fn
  (push `,name *query-names*)
  (setf (get `,name 'formula) body)
  (let ((new-$vars (delete-duplicates 
                     (set-difference (get-all-nonspecial-vars #'$varp body) args))))
    (setf (get `,name 'fn)
      `(lambda (state ,@args)
         ,(format nil "~A query-fn" name)
         (block ,name
           (let ,new-$vars
             (declare (ignorable ,@new-$vars))
             ,(if (eql (car body) 'let)
                `(let ,(second body)
                   ,(third body)  ;should be a declare statement
                   ,(translate (fourth body) 'pre))
                (translate body 'pre)))))))
  (fix-if-ignore '(state) (get `,name 'fn))
  (setf (symbol-value `,name) (copy-tree (get `,name 'fn))))


(defmacro define-update (name args body)
  `(install-update ',name ',args ',body))


(defun install-update (name args body)
  (format t "Installing ~A update...~%" name)
  (check-type args (satisfies list-of-variables))
  (fmakunbound `,name)  ;avoids compiler warning if recursive fn
  (push `,name *update-names*)
  (setf (get `,name 'formula) body)
;  (let (eff-args eff-?vars)
  (ut::if-it (delete-duplicates
                 (set-difference
                   (get-all-nonspecial-vars #'$varp body) args))  ;get $vars for let
      (setf (get `,name 'fn)
        `(lambda (state idb ,@args)
           ,(format nil "~A update-fn" name)
           (declare (ignorable state))
           (let (updated-dbs ,@ut::it)
             (declare (ignorable updated-dbs ,@ut::it))
             ,(translate body 'eff)
             idb)))
      (setf (get `,name 'fn)
        `(lambda (state idb ,@args)
           ,(format nil "~A update-fn" name)
           (declare (ignorable state))
           (progn
             ,(translate body 'eff))
             idb)))
  (fix-if-ignore '(idb) (get `,name 'fn))
  (setf (symbol-value `,name) (copy-tree (get `,name 'fn))))

  
(defmacro define-constraint (form)
  `(install-constraint ',form))


(defun install-constraint (form)
  (format t "Installing constraint...~%")
  (setf (get '*constraint* 'formula) form)
  (let (($vars (get-all-nonspecial-vars #'$varp form)))
    (setf (get '*constraint* 'fn)
      `(lambda (state)
         (let ,$vars
           ,(when $vars
              `(declare (ignorable ,@$vars)))
           ,(translate form 'pre)))))
  (fix-if-ignore '(state) (get '*constraint* 'fn))
  (setf *constraint* (copy-tree (get '*constraint* 'fn))))
        

(defmacro define-action (name duration pre-params precondition eff-params effect)
  `(install-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-action (name duration pre-params precondition eff-params effect)
  (format t "Installing ~A action...~%" name)
  (push (create-action name duration pre-params precondition eff-params effect nil)
    *actions*))


(defmacro define-init-action (name duration pre-params precondition eff-params effect)
  `(install-init-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-init-action (name duration pre-params precondition eff-params effect)
  (declare (ignore duration))
  (format t "Installing ~A init action...~%" name)
  (push (create-action name 0 pre-params precondition eff-params effect t)
    *init-actions*))


(defun create-action (name duration pre-params precondition eff-params effect init-action)
  (check-type name symbol)
  (check-type duration real)
  (destructuring-bind (pre-param-?vars pre-user-types restriction) (dissect-parameters pre-params)
    (check-type pre-param-?vars (satisfies list-of-?variables))
    (check-type pre-user-types (satisfies list-of-parameter-types))
    (destructuring-bind (eff-param-vars eff-user-types *) (dissect-parameters eff-params)
      (check-type eff-param-vars (satisfies list-of-variables))
      (check-type eff-user-types (satisfies list-of-parameter-types))
      (let* ((pre-$vars (delete-duplicates (get-all-nonspecial-vars #'$varp precondition) :from-end t))
             (pre-special-$vars (get-special-vars precondition))
             (eff-$vars (delete-duplicates (get-all-nonspecial-vars #'$varp effect) :from-end t))
             (eff-args (append pre-param-?vars pre-$vars pre-special-$vars))
             (eff-?vars (delete-duplicates (get-all-nonspecial-vars #'?varp effect) :from-end t))
             (eff-bound-?vars (delete-duplicates (get-bound-?vars (list effect)) :from-end t))
             (eff-free-?vars (ut::list-difference eff-?vars eff-bound-?vars))
             (eff-extra-$vars (ut::list-difference (ut::list-difference eff-$vars pre-$vars)
                                                   pre-special-$vars))
             (eff-extra-?vars (ut::if-it (ut::list-difference eff-free-?vars pre-param-?vars )
                                (error "Extra effect ?vars in action ~A: ~A" name ut::it)))
             (eff-missing-vars (ut::list-difference eff-args (append eff-free-?vars eff-$vars)))
             (pre-types (transform-types pre-user-types))
             (queries (intersection (alexandria:flatten pre-types) *query-names*))
             (action nil))
;        (ut::prt pre-$vars pre-special-$vars eff-$vars eff-args eff-?vars eff-bound-?vars eff-free-?vars
;                 eff-extra-$vars eff-extra-?vars eff-missing-vars)
        (cond (init-action
                 (setq *objective-value-p* nil))  ;this is an init-action, disable $objective-value
              ((or (member '$objective-value pre-$vars)  ;used in translate-assert
                  (member '$objective-value eff-extra-$vars))
                 (setq *objective-value-p* t))  ;this is a normal action rule with optimization
              (t (setq *objective-value-p* nil)))  ;normal rule, but no optimization
        (setq *eff-param-vars* eff-param-vars)  ;used in translate-assert
        (setf action (make-action
                       :name name
                       :duration duration
                       :precondition-params pre-params
                       :precondition-variables (append pre-param-?vars pre-$vars)
                       :precondition-types pre-user-types
                       :restriction restriction
                       :dynamic (when queries pre-types)
                       :precondition-instantiations (or (unless queries
                                                      (instantiate-types pre-types restriction)) '(nil))
                       ;:precondition-lits nil
                       :precondition-lambda `(lambda (state ,@pre-param-?vars)
                                               ,(format nil "~A precondition" name)
                                               (let ,pre-$vars
                                                 (declare (ignorable ,@pre-$vars))
                                                 ,(if (eql (car precondition) 'let)
                                                    `(let ,(second precondition)
                                                       ,(third precondition)
                                                       (when ,(translate (fourth precondition) 'pre)
                                                         ,(if eff-args
                                                            `(list ,@eff-args)
                                                            `t)))
                                                    `(when ,(translate precondition 'pre)
                                                       ,(if eff-args
                                                          `(list ,@eff-args)
                                                          `t)))))
                       :effect-variables eff-param-vars  ;user listed parameter variables
                       :effect-types eff-user-types
                       :effect-lambda `(lambda (state ,@eff-args ,@eff-extra-?vars)
                                         ,(format nil "~A effect" name)
                                         (let (updated-dbs ,@(set-difference (set-difference eff-extra-$vars eff-args)
                                                                             eff-extra-?vars))  ;eff-extra-$vars)
                                           (declare (ignorable ,@eff-extra-$vars))
                                           ,(translate effect 'pre)  ;start as pre, shift to eff in assert
                                           updated-dbs))
                       :effect-adds nil))
        (fix-if-ignore '(state) (action.precondition-lambda action))
        (fix-if-ignore `(state ,@eff-missing-vars) (action.effect-lambda action))
        action))))


(defun get-bound-?vars (tree)
  "Searches tree for an atom or cons satisfying the predicate, and returns
   list of all such items. To include tree itself, pass in (list tree)."
  (iter (for item in tree)
        (when (consp item)
          (if (member (car item) '(exists exist forsome forall forevery doall))
            (nconcing (delete-if-not #'?varp (alexandria:flatten (second item))))
            (nconcing (get-bound-?vars item))))))


(defun get-special-vars (tree)
  "Collects any special declared variables from tree."
  (when (listp tree)
    (let (special-vars)
      (ut::walk-tree (lambda (item)
                       (if (and (listp item) (eql (car item) 'special))
                         (alexandria:appendf special-vars (cdr item))))
                     tree)
      special-vars)))


(defun get-all-vars (fn tree)
  "Selects one each of all variables in the tree satisfying fn."
  (remove-duplicates (remove-if-not fn (alexandria:flatten tree))))


(defun get-all-nonspecial-vars (fn tree)
  "Selects one each of non-special variables in the tree satisfying fn."
  (remove-duplicates (remove-if-not fn (set-difference (alexandria:flatten tree)
                                                       (get-special-vars tree)))))


(defun fix-if-ignore (symbols lambda-expr)
  "Ignores variable symbols that are not in the lambda-body."
  (let ((ignores (ut::list-difference
                    symbols (get-all-nonspecial-vars (lambda (x)
                                            (member x symbols))
                                          (cddr lambda-expr)))))
    (when ignores
      (push `(declare (ignorable ,@ignores)) (cddr lambda-expr)))))


(defmacro define-init (&rest literals)
  `(install-init ',literals))


(defun install-init (literals)
  (declare (hash-table *relations* *db* *static-db*))
  (format t "Creating initial propositional database...~%")
  (dolist (literal literals)
    ;(check-type literal (satisfies literal))
    (when (eql (car literal) #+sbcl 'sb-int:quasiquote #+allegro 'excl::backquote)
      (setq literal (eval literal)))
    (if (eql (car literal) 'not)
      (if (gethash (caadr literal) *relations*)
        (delete-proposition (second literal) *db*)
        (delete-proposition (second literal) *static-db*))
      (if (gethash (car literal) *relations*)
        (add-proposition literal *db*)
        (add-proposition literal *static-db*)))))


(defmacro define-goal (form)
  `(install-goal ',form))


(defun install-goal (form)
  (format t "Installing goal...~%")
  (when (and (null form)
             (not (eql *solution-type* 'min-value))
             (not (eql *solution-type* 'max-value)))
    (error "Goal is required unless searching for a *solution-type* of min-value or max-value."))
  (setf (get '*goal* 'formula) form)  ;save user's goal for summary printout
  (let (($vars (get-all-nonspecial-vars #'$varp form)))
    (setf (get '*goal* 'fn)
      `(lambda (state)  ;save uncoded goal translation
         (let ,$vars
           ,(when $vars
              `(declare (ignorable ,@$vars)))
           ,(translate form 'pre)))))
  (fix-if-ignore '(state) (get '*goal* 'fn))
  (setq *goal* (copy-tree (get '*goal* 'fn))))  ;to be compiled
    