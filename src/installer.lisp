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


(defmacro define-derived-relations (&rest conditions&bodies)
  `(install-derived-relations ',conditions&bodies))


(defun install-derived-relations (conditions&bodies)
  (format t "Installing derived relations...~%")
  (loop for (condition body) on conditions&bodies by #'cddr
     do (check-type condition list)
        (check-type (car condition) symbol)
        (setf (gethash (car condition) *derived*)
          (list (loop for item in (cdr condition)
                      do (check-type item (or (satisfies varp) symbol))
                     collect item)
                 body))))


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
  (when (getf plist :interrupt)
    (setf (get object :interrupt) `(lambda (state)
                                     ,(translate (getf plist :interrupt) 'pre))))
  (fix-if-ignore-state (get object :interrupt))
  (when (getf plist :repeat)
    (setf (get object :repeat) (getf plist :repeat)))
  ;(when (get object :rebound) 
  ;  (setf (get object :rebound) `(lambda (state)
  ;                                 ,(translate (get object :rebound) 'pre)))
  ;  (setf (the function (get object :rebound-fn)) (compile nil (get object :rebound))))
  (push object *happenings*))
           

(defmacro define-precondition-function (name args body)
  `(install-precondition-function ',name ',args ',body))


(defun install-precondition-function (name args body)
  (format t "Installing ~A precondition function...~%" name)
  (check-type args (satisfies list-of-variables))
  (push `,name *function-names*)
  (setf (get `,name 'formula) body)
  (setf (get `,name 'fn) `(lambda (state ,@args)
                            ,(translate body 'pre)))
  (fix-if-ignore-state (get `,name 'fn))
  (setf (symbol-value `,name) (copy-tree (get `,name 'fn))))


(defmacro define-effect-function (name args body)
  `(install-effect-function ',name ',args ',body))


(defun install-effect-function (name args body)
  (format t "Installing ~A effect function...~%" name)
  (check-type args (satisfies list-of-variables))
  (push `,name *function-names*)
  (setf (get `,name 'formula) body)
  (setf (get `,name 'fn) `(lambda (state ,@args)
                                 (let (changes)
                                   ,(translate body 'eff)
                                   changes)))
  (fix-if-ignore-state (get `,name 'fn))
  (setf (symbol-value `,name) (copy-tree (get `,name 'fn))))

  
(defmacro define-constraint (form)
  `(install-constraint ',form))


(defun install-constraint (form)
  (format t "Installing constraint...~%")
  (setf (get '*constraint* 'formula) form)
  (setf (get '*constraint* 'fn) `(lambda (state) ,(translate form 'pre)))
  (fix-if-ignore-state (get '*constraint* 'fn))
  (setf *constraint* (copy-tree (get '*constraint* 'fn))))
        

(defmacro define-action (name duration pre-params precondition eff-params effect)
  `(install-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-action (name duration pre-params precondition eff-params effect)
  (declare (special *actions* *types*))
  (format t "Installing ~A action...~%" name)
  (check-type name symbol)
  (check-type duration real)
  (destructuring-bind (pre-vars pre-types) (dissect-parameters pre-params)
    (check-type pre-vars (satisfies list-of-variables))
    (check-type pre-types (satisfies list-of-parameter-types))
    (let ((pre-?vars (remove-if-not #'?varp pre-vars))
          (pre-$vars (remove-if-not #'$varp pre-vars)))
      (destructuring-bind (eff-vars eff-types) (dissect-parameters eff-params)
        (check-type eff-vars (satisfies list-of-variables))
        (check-type eff-types (satisfies list-of-parameter-types))
        (let ((eff-$vars (remove-if-not #'$varp eff-vars))
              (pre-return-vars (ut::list-difference eff-vars
                                                    (ut::list-difference eff-vars pre-vars))))
          (let* ((*current-precondition-fluents* pre-$vars) ;init first-fluent-appearance
                 (*current-effect-fluents* (set-difference eff-$vars pre-$vars))
                 (action (make-action
                         :name name
                         :duration duration
                         :precondition-variables pre-vars
                         :precondition-types pre-types
                         :precondition-instantiations (type-instantiations pre-types)
                                                                 
                         :precondition-lits nil
                         :precondition-lambda `(lambda (state ,@pre-?vars)
                                                 (let ,pre-$vars
                                                   (declare (special ,@pre-$vars))  ;needed for (set $var ...)
                                                   (when ,(translate precondition 'pre)
                                                     ;return values or satisfied pre
                                                     (or (mapcar #'list (list ,@pre-return-vars))
                                                         '(nil)))))
                         :effect-variables eff-vars
                         :effect-types eff-types
                         :effect-instantiations (type-instantiations  ;new eff types only
                                                  (ut::list-difference eff-types pre-types))
                         :effect-lambda `(lambda (state ,@eff-vars)
                                           (let (changes ,@(ut::list-difference eff-$vars pre-$vars))
                                             ,(translate effect 'eff)
                                             changes))
                          :effect-adds nil)))
            (fix-if-ignore-state (action-precondition-lambda action))
            (fix-if-ignore-state (action-effect-lambda action))
            (loop with effect = (action-effect-lambda action)
                  for var in eff-vars
                  when (not (ut::walk-tree-until (lambda (x)
                                                   (eql x var))
                                                 (cddr effect)))
                collect var into vars
                  finally (push `(declare (ignore ,@vars)) (cddr effect)))
            (push action *actions*)
            t))))))


(defmacro define-init-action (name duration pre-params precondition eff-params effect)
  `(install-init-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-init-action (name duration pre-params precondition eff-params effect)
  (format t "Installing ~A init action...~%" name)
  (destructuring-bind (pre-vars pre-types) (dissect-parameters pre-params)
    (check-type pre-vars (satisfies list-of-variables))
    (check-type pre-types (satisfies list-of-parameter-types))
    (let ((pre-?vars (remove-if-not #'?varp pre-vars))
          (pre-$vars (remove-if-not #'$varp pre-vars)))
      (destructuring-bind (eff-vars eff-types) (dissect-parameters eff-params)
        (check-type eff-vars (satisfies list-of-variables))
        (check-type eff-types (satisfies list-of-parameter-types))
        (let ((eff-$vars (remove-if-not #'$varp eff-vars))
              (pre-return-vars (ut::list-difference eff-vars
                                                    (ut::list-difference eff-vars pre-vars))))
          (let* ((*current-precondition-fluents* pre-$vars) ;init first-fluent-appearance
                 (*current-effect-fluents* (set-difference eff-$vars pre-$vars))
                 (action (make-action
                         :name name
                         :duration duration
                         :precondition-variables pre-vars
                         :precondition-types pre-types
                         :precondition-instantiations (type-instantiations pre-types)
                         :precondition-lits nil
                         :precondition-lambda `(lambda (state ,@pre-?vars)
                                                 (let ,pre-$vars
                                                   (declare (special ,@pre-$vars))  ;needed for (set $var ...)
                                                   (when ,(translate precondition 'pre)
                                                     ;return values or satisfied pre
                                                     (or (mapcar #'list (list ,@pre-return-vars))
                                                         t))))
                         :effect-variables eff-vars
                         :effect-types eff-types
                         :effect-instantiations (type-instantiations
                                                  (ut::list-difference eff-types pre-types))
                         :effect-lambda `(lambda (state ,@eff-vars)
                                           (let (changes ,@(set-difference eff-$vars pre-$vars))
                                             ,(translate effect 'eff)
                                             changes))
                         :effect-adds nil)))
            (fix-if-ignore-state (action-precondition-lambda action))
            (fix-if-ignore-state (action-effect-lambda action))
            (loop with effect = (action-effect-lambda action)
                  for var in eff-vars
                  when (not (ut::walk-tree-until (lambda (x)
                                                   (eql x var))
                                                 (cddr effect)))
                collect var into vars
                  finally (push `(declare (ignore ,@vars)) (cddr effect)))
            (push action *init-actions*)
            action))))))


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
  (setf (get '*goal* 'fn) `(lambda (state) ,(translate form 'pre)))  ;save uncoded goal translation
  (fix-if-ignore-state (get '*goal* 'fn))
  (setq *goal* (copy-tree (get '*goal* 'fn)))  ;to be compiled
  (setup))
