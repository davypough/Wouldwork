;;; Filename: installer.lisp

;;; Installs a user domain file.


(in-package :pl)


(defmacro define-types (&rest types&objects)
  `(install_types ',types&objects))


(defun install_types (types&objects)
  (declare (special *db* *types*))
  (format t "~%Installing object types...~%")
  (loop for (type objects) on types&objects by #'cddr
      when (eq (car objects) 'either)
      do (setf objects (apply #'either (cdr objects)))
      end
      do (setf (gethash type *types*) objects)
         (dolist (object objects)
           (setf (gethash (list type object) *db*) t))
      append objects into everything
      finally (setf (gethash 'everything *types*) 
                (delete-duplicates everything :test #'eq)))
  *types*)


(defmacro define-base-relations (&rest relations)
  `(install_base_relations ',relations))


(defun install_base_relations (relations)
  (declare (hash-table *types* *relations*) (special *types* *relations* *symmetrics*))
  (format t "Installing base relations...~%")
  (loop for relation in relations
      do (setf (gethash (car relation) *relations*) 
           (ut::if-it (cdr relation) ut::it t))
      finally (maphash #'(lambda (key value)  ;install implied unary relations
                           (declare (ignore value))
                           (setf (gethash key *relations*) '(everything)))
                       *types*))
  (loop for key being the hash-keys of *relations*  ;install symmetric relations
        using (hash-value value)
      when (not (alexandria:setp value))  ;multiple types
        do (setf (gethash key *symmetrics*) (symmetric_type_indexes value)))
  *relations*)


(defmacro define-monitored-relations (&rest relations)
  `(install_monitored_relations ',relations))


(defun install_monitored_relations (relations)
  (declare (special *monitored_relations*))
  (format t "Installing monitored relations...~%")
  (loop for relation in relations
        do (setf (gethash relation *monitored_relations*) t)))


(defmacro define-derived-relations (&rest conditions&bodies)
  `(install_derived_relations ',conditions&bodies))


(defun install_derived_relations (conditions&bodies)
  (declare (special *derived*))
  (format t "Installing derived relations...~%")
  (loop for (condition body) on conditions&bodies by #'cddr
      do (setf (gethash (car condition) *derived*)
           (list (loop for item in (cdr condition)
                     when (or (varp #\? item)
                              (varp #\$ item))
                     collect item)
                 body))))
        

(defmacro define-happening (object &rest plist)
  `(install_happening ',object ',plist))


(defun install_happening (object plist)
  (declare (special *happenings*))
  (format t "Installing happening for ~A~%" object)
  (setf (symbol-plist object) plist)  ;overwrite any settings from a prior problem
  (setf (the simple-vector (get object :events)) (coerce (get object :events) 'simple-vector))
  (when (get object :interrupt)
    (setf (get object :interrupt) `(lambda (state)
                                     ,(translate (get object :interrupt) 'pre)))
    (setf (the function (get object :interrupt_fn)) (compile nil (get object :interrupt))))
  (when (get object :rebound) 
    (setf (get object :rebound) `(lambda (state)
                                     ,(translate (get object :rebound) 'pre)))
    (setf (the function (get object :rebound_fn)) (compile nil (get object :rebound))))
  (push object *happenings*))
           

(defmacro define-function (name args body)
  `(install_function ',name ',args ',body))


(defun install_function (name args body)
  (declare (special *function_names*))
  (format t "Installing ~A function...~%" name)
  (setf (symbol-value name)
    (let* ((fbody (alexandria:flatten body))
           (rbody (remove-if-not #'(lambda (x) (varp #\$ x)) fbody))
           (dbody (delete-duplicates rbody))
           (sbody (set-difference dbody args)))
      `(lambda ,args
         (block ,name
           (let ,(map 'list #'(lambda ($var) (list $var 0)) sbody)
             (declare (special ,@sbody))
             ,(translate body 'pre)
             (list ,@(get_vars #\$ args)))))))  ;to prevent compiler warning
  (eval `(defun ,name ,@(cdr (symbol-value name))))
  (push name *function_names*))
  

(defmacro define-constraint (form)
  `(install_constraint ',form))


(defun install_constraint (form)
  (declare (special *constraint*))
  (format t "Installing constraint...~%")
  (setq *constraint* `(lambda (state) ,(translate form 'pre))))
        

(defmacro define-action (name duration pre_params precondition eff_params effect)
  `(install_action ',name ,duration ',pre_params ',precondition ',eff_params ',effect))


(defun install_action (name duration pre_params precondition eff_params effect)
  (declare (special *actions*))
  (format t "Installing ~A action...~%" name)
  (destructuring-bind (pre_vars pre_types) (ut::segregate-plist (expand_into_plist pre_params))
    (let ((pre_vars? (get_vars #\? pre_vars))
          (pre_vars$ (get_vars #\$ pre_vars))
          (pre_types (delete 'fluent pre_types)))
      (destructuring-bind (eff_vars eff_types) (ut::segregate-plist (expand_into_plist eff_params))
        (let ((eff_vars? (get_vars #\? eff_vars))
              (eff_vars$ (get_vars #\$ eff_vars))
              (eff_types (delete 'fluent eff_types)))
          (let ((action (make-action
                         :name name
                         :duration duration
                         :precondition_variables pre_vars?
                         :precondition_types pre_types
                         :precondition_lambda `(lambda ,(cons 'state pre_vars?)
                                                 (let ,(map 'list #'(lambda ($var)
                                                                       (list $var 0))
                                                           pre_vars$)
                                                   ;special allows setting of local $var
                                                   (declare (special ,@pre_vars$))
                                                   (when ,(translate precondition 'pre)
                                                     ;return fluent values or satisfied pre
                                                     (or (list ,@eff_vars$) t))))
                         :effect_variables eff_vars?
                         :effect_types eff_types
                         :effect_lambda `(lambda (state ,@eff_vars? ,@eff_vars$)
                                           (let (changes)
                                             ,(translate effect 'eff)
                                             changes)))))
            (dolist (form (list (action-precondition_lambda action) 
                                (action-effect_lambda action)))
              (when (not (ut::walk-tree-until (lambda (x)
                                                (eq x 'state))
                                              (third form))) ;db in the lambda body?
                 (push '(declare (ignore state)) (cddr form))))
            (pushnew action *actions* :test #'equalp)
            action))))))


(defmacro define-init (&rest propositions)
  `(install_init ',propositions))


(defun install_init (propositions)
  (declare (hash-table *db*) (special *db*))
  (format t "Installing initial propositional database...~%")
  (loop for proposition in propositions
      do (add_proposition proposition *db*))
  (add_proposition '(always-true) *db*)
  *db*)


(defmacro define-goal (form)
  `(install_goal ',form))


(defun install_goal (form)
  (declare (special *goal*))
  (format t "Installing goal...~%")
  (setq *goal* `(lambda (state) ,(translate form 'pre)))
  (setup))
