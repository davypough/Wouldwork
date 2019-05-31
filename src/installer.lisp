;;; Filename: installer.lisp

;;; Installs a user domain file.


(in-package :ww)


(defmacro define-types (&rest types&constants)
  `(install-types ',types&constants))


(defun install-types (types&constants)
  (declare (hash-table *types* *static-db*))
  (format t "~%Installing object types...~%")
  (loop for (type constants) on types&constants by #'cddr
         do (check-type type symbol)
            (check-type constants list)
            (when (and (consp constants) (eql (car constants) 'either))
              (setf constants (apply #'either (cdr constants))))
            (when (eql (car constants) 'compute)
              (setf constants (eval (second constants))))
            (setf (gethash type *types*) constants)
            (when (consp constants)
              (dolist (constant constants)
                (check-type constant (or symbol real))
                (setf (gethash (list 'something constant) *static-db*) t)
                (setf (gethash (list type constant) *static-db*) t)))
        when (consp constants)
          append constants into everything
      finally (setf (gethash 'something *types*)  ;every constant is a something
                (remove-duplicates everything :test #'eql))))


(defmacro define-dynamic-relations (&rest relations)
  `(install-dynamic-relations ',relations))


(defun install-dynamic-relations (relations)
  (declare (hash-table *types* *relations*))
  (format t "Installing dynamic relations...~%")
  (loop for relation in relations
     do (check-type relation cons)
        (check-type (car relation) symbol)
        (setf (gethash (car relation) *relations*)
          (ut::if-it (cdr relation)
            (sort-either-types ut::it)
            nil))
        (ut::if-it (loop for arg in (cdr relation)
                        for i from 0
                          do (check-type arg (satisfies type-description))
                        when ($varp arg)
                          collect i)
          (setf (gethash (car relation) *fluent-relation-indices*) ut::it))
      finally (maphash #'(lambda (key value)  ;install implied unary relations
                           (declare (ignore value))
                           (setf (gethash key *static-relations*) '(something)))
                       *types*))
  (loop for key being the hash-keys of *relations*  ;install symmetric relations
        using (hash-value value)
      when (and (not (alexandria:setp value))  ;multiple types
                (not (final-charp #\> key)))   ;not explicitly directed
        do (setf (gethash key *symmetrics*) (symmetric-type-indexes value)))
  t)


(defmacro define-static-relations (&rest relations)
  `(install-static-relations ',relations))


(defun install-static-relations (relations)
  (declare (hash-table *types* *static-relations*))
  (format t "Installing static relations...~%")
  (loop for relation in relations
     do (check-type relation cons)
        (check-type (car relation) symbol)
        (setf (gethash (car relation) *static-relations*)
          (ut::if-it (cdr relation) 
            (sort-either-types ut::it)
            nil))
         (ut::if-it (loop for arg in (cdr relation)
                          for i from 0
                          do (check-type arg (satisfies type-description))
                          when ($varp arg)
                          collect i)
           (setf (gethash (car relation) *fluent-relation-indices*) ut::it))
      finally (maphash #'(lambda (key value)  ;install implied unary relations
                           (declare (ignore value))
                           (setf (gethash key *static-relations*) '(everything)))
                       *types*))
  (loop for key being the hash-keys of *static-relations*  ;install symmetric relations
        using (hash-value value)
      when (and (not (alexandria:setp value))  ;multiple types
                (not (final-charp #\> key)))   ;not explicitly directed
      do (setf (gethash key *symmetrics*) (symmetric-type-indexes value)))
  (setf (gethash 'always-true *static-relations*) t)
  (setf (gethash 'waiting *static-relations*) t)
  t)


(defmacro define-complementary-relations (&rest positives->negatives)
  `(install-complementary-relations ',positives->negatives))


(defun install-complementary-relations (positives->negatives)
  (format t "Installing complementary relations...~%")
  (loop for (positive nil negative) on positives->negatives by #'cdddr
     do 
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
  (setf (the simple-vector (get object :events)) (coerce (getf plist :events) 'simple-vector))
  (ut::if-it (getf plist :interrupt)
    (let (($vars (get-all-vars #'$varp ut::it)))
      (setf (get object :interrupt) `(lambda (state)
                                       (let ,$vars
                                         ,(when $vars
                                            `(declare (special @,$vars)))
                                         ,(translate (getf plist :interrupt) 'pre))))))
  (fix-if-ignore '(state) (get object :interrupt))
  (when (getf plist :repeat)
    (setf (get object :repeat) (getf plist :repeat)))
  ;(when (get object :rebound) 
  ;  (setf (get object :rebound) `(lambda (state)
  ;                                 ,(translate (get object :rebound) 'pre)))
  ;  (setf (the function (get object :rebound-fn)) (compile nil (get object :rebound))))
  (push object *happenings*))
           

(defmacro define-query (name args body)
  `(install-query ',name ',args ',body))


(defun install-query (name args body)
  (format t "Installing ~A query function...~%" name)
  (check-type args (satisfies list-of-variables))
  (push `,name *function-names*)
  (setf (get `,name 'formula) body)
  (let ((new-$vars (delete-duplicates (set-difference (get-all-vars #'$varp body) args))))
    (setf (get `,name 'fn) `(lambda (state ,@args)
                              (let ,new-$vars
                                ,(when new-$vars
                                   `(declare (special ,@new-$vars)))
                                ,(translate body 'pre)))))
  (fix-if-ignore '(state) (get `,name 'fn))
  (setf (symbol-value `,name) (copy-tree (get `,name 'fn))))


(defmacro define-update (name args body)
  `(install-update ',name ',args ',body))


(defun install-update (name args body)
  (format t "Installing ~A update function...~%" name)
  (check-type args (satisfies list-of-variables))
  (push `,name *function-names*)
  (setf (get `,name 'formula) body)
  (ut::if-it (delete-duplicates (set-difference (get-all-vars #'$varp body) args))
    (setf (get `,name 'fn) `(lambda (state ,@args)
                                 (let (changes ,@ut::it)
                                   ,(when ut::it
                                      `(declare (special ,@ut::it)))
                                   ,(translate body 'eff)
                                   changes)))
    (setf (get `,name 'fn) `(lambda (state ,@args)
                                 (let (changes)
                                   ,(translate body 'eff)
                                   changes))))
  (fix-if-ignore '(state) (get `,name 'fn))
  (setf (symbol-value `,name) (copy-tree (get `,name 'fn))))

  
(defmacro define-constraint (form)
  `(install-constraint ',form))


(defun install-constraint (form)
  (format t "Installing constraint...~%")
  (setf (get '*constraint* 'formula) form)
  (let (($vars (get-all-vars #'$varp form)))
    (setf (get '*constraint* 'fn) `(lambda (state)
                                     (let ,$vars
                                       ,(when $vars
                                          `(declare (special ,@$vars)))
                                       ,(translate form 'pre)))))
  (fix-if-ignore '(state) (get '*constraint* 'fn))
  (setf *constraint* (copy-tree (get '*constraint* 'fn))))
        

(defmacro define-action (name duration pre-params precondition eff-params effect)
  `(install-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-action (name duration pre-params precondition eff-params effect)
  (format t "Installing ~A action...~%" name)
  (push (create-action name duration pre-params precondition eff-params effect)
        *actions*))


(defmacro define-init-action (name duration pre-params precondition eff-params effect)
  `(install-init-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-init-action (name duration pre-params precondition eff-params effect)
  (declare (ignore duration))
  (format t "Installing ~A init action...~%" name)
  (push (create-action name 0 pre-params precondition eff-params effect)
        *init-actions*))


(defun create-action (name duration pre-params precondition eff-params effect)
  (check-type name symbol)
  (check-type duration real)
  (destructuring-bind (pre-param-?vars pre-types) (dissect-parameters pre-params)
    (check-type pre-param-?vars (satisfies list-of-?variables))
    (check-type pre-types (satisfies list-of-parameter-types))
    (destructuring-bind (eff-param-vars eff-types) (dissect-parameters eff-params)
      (check-type eff-param-vars (satisfies list-of-variables))
      (check-type eff-types (satisfies list-of-parameter-types))
      (let* ((pre-$vars (delete-duplicates (get-all-vars #'$varp precondition) :from-end t))
             (eff-$vars (delete-duplicates (get-all-vars #'$varp effect) :from-end t))
             (eff-args (append pre-param-?vars pre-$vars))
             (eff-?vars (delete-duplicates (get-all-vars #'?varp effect) :from-end t))
             (eff-bound-?vars (delete-duplicates (get-bound-?vars (list effect)) :from-end t))
             (eff-free-?vars (ut::list-difference eff-?vars eff-bound-?vars))
             (eff-extra-$vars (ut::list-difference eff-$vars pre-$vars))
             (eff-extra-?vars (ut::if-it (ut::list-difference eff-free-?vars pre-param-?vars)
                                (error "Extra effect ?vars in action ~A: ~A"
                                       name ut::it)))
             (eff-missing-vars (ut::list-difference eff-args (append eff-free-?vars eff-$vars)))
             (action (make-action
                       :name name
                       :duration duration
                       :precondition-variables (append pre-param-?vars pre-$vars)
                       :precondition-types pre-types
                       :precondition-instantiations (or (type-instantiations pre-types) '(nil))
                       :precondition-lits nil
                       :precondition-lambda `(lambda (state ,@pre-param-?vars)
                                               (let ,pre-$vars
                                                 (declare (special ,@pre-$vars))
                                                 (when ,(translate precondition 'pre)
                                                   ;return satisfied pre values
                                                   (list ,@eff-args))))
                       :effect-variables eff-param-vars  ;user listed parameter variables
                       :effect-types eff-types
                       :effect-lambda `(lambda (state ,@eff-args ,@eff-extra-?vars)
                                         (let (changes followups ,@eff-extra-$vars)
                                           (declare (special ,@eff-extra-$vars))
                                           ,(translate effect 'eff)
                                           (make-update :changes changes 
                                                        :instantiations (list ,@eff-param-vars)
                                                        :followups (nreverse followups))))
                      :effect-adds nil)))
        (fix-if-ignore '(state) (action-precondition-lambda action))
        (fix-if-ignore `(state ,@eff-missing-vars) (action-effect-lambda action))
        action))))



(defmacro define-init (&rest propositions)
  `(install-init ',propositions))


(defun install-init (propositions)
  (declare (hash-table *db* *static-db*))
  (format t "Creating initial propositional database...~%")
  (dolist (proposition propositions)
    (check-type proposition (satisfies proposition))
    (if (gethash (car proposition) *relations*)
      (add-proposition proposition *db*)  ;dynamic database
      (add-proposition proposition *static-db*)))
  (add-proposition '(always-true) *static-db*)
  t)


(defmacro define-goal (form)
  `(install-goal ',form))


(defun install-goal (form)
  (format t "Installing goal...~%")
  (setf (get '*goal* 'formula) form)  ;save user's goal for summary printout
  (let (($vars (get-all-vars #'$varp form)))
    (setf (get '*goal* 'fn) `(lambda (state)  ;save uncoded goal translation
                               (let ,$vars
                                 ,(when $vars
                                    `(declare (special ,@$vars)))
                                 ,(translate form 'pre)))))
  (fix-if-ignore '(state) (get '*goal* 'fn))
  (setq *goal* (copy-tree (get '*goal* 'fn)))  ;to be compiled
  (setup))
    