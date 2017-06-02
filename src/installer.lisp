;;; Filename: installer.lisp

;;; Installs a user domain file.


(in-package :ww)


(defmacro define-types (&rest types&objects)
  `(install-types ',types&objects))


(defun install-types (types&objects)
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
  `(install-base-relations ',relations))


(defun install-base-relations (relations)
  (declare (hash-table *types* *relations*) (special *types* *relations* *symmetrics*))
  (format t "Installing base relations...~%")
  (loop for relation in relations
      do (setf (gethash (car relation) *relations*) 
           (ut::if-it (cdr relation) ut::it nil))
      finally (maphash #'(lambda (key value)  ;install implied unary relations
                           (declare (ignore value))
                           (setf (gethash key *relations*) '(everything)))
                       *types*))
  (loop for key being the hash-keys of *relations*  ;install symmetric relations
        using (hash-value value)
      when (not (alexandria:setp value))  ;multiple types
        do (setf (gethash key *symmetrics*) (symmetric-type-indexes value)))
  *relations*)


(defmacro define-monitored-relations (&rest relations)
  `(install-monitored-relations ',relations))


(defun install-monitored-relations (relations)
  (declare (special *monitored-relations*))
  (format t "Installing monitored relations...~%")
  (loop for relation in relations
        do (setf (gethash relation *monitored-relations*) t)))


(defmacro define-derived-relations (&rest conditions&bodies)
  `(install-derived-relations ',conditions&bodies))


(defun install-derived-relations (conditions&bodies)
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
  `(install-happening ',object ',plist))


(defun install-happening (object plist)
  (declare (special *happenings*))
  (format t "Installing happening for ~A~%" object)
  (setf (symbol-plist object) plist)  ;overwrite any settings from a prior problem
  (setf (the simple-vector (get object :events)) (coerce (get object :events) 'simple-vector))
  (when (get object :interrupt)
    (setf (get object :interrupt) `(lambda (state)
                                     ,(translate (get object :interrupt) 'pre)))
    (setf (the function (get object :interrupt-fn)) (compile nil (get object :interrupt))))
  (when (get object :rebound) 
    (setf (get object :rebound) `(lambda (state)
                                     ,(translate (get object :rebound) 'pre)))
    (setf (the function (get object :rebound-fn)) (compile nil (get object :rebound))))
  (push object *happenings*))
           

(defmacro define-function (name args body)
  `(install-function ',name ',args ',body))


(defun install-function (name args body)
  (declare (special *function-names*))
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
             (list ,@(get-vars #\$ args)))))))  ;to prevent compiler warning
  (eval `(defun ,name ,@(cdr (symbol-value name))))
  (push name *function-names*))
  

(defmacro define-constraint (form)
  `(install-constraint ',form))


(defun install-constraint (form)
  (declare (special *constraint*))
  (format t "Installing constraint...~%")
  (setq *constraint* `(lambda (state) ,(translate form 'pre))))
        

(defmacro define-action (name duration pre-params precondition eff-params effect)
  `(install-action ',name ,duration ',pre-params ',precondition ',eff-params ',effect))


(defun install-action (name duration pre-params precondition eff-params effect)
  (declare (special *actions*))
  (format t "Installing ~A action...~%" name)
  (destructuring-bind (pre-vars pre-types) (ut::segregate-plist (expand-into-plist pre-params))
    (let ((pre-vars? (get-vars #\? pre-vars))
          (pre-vars$ (get-vars #\$ pre-vars))
          (pre-types (delete 'fluent pre-types)))
      (destructuring-bind (eff-vars eff-types) (ut::segregate-plist (expand-into-plist eff-params))
        (let ((eff-vars? (get-vars #\? eff-vars))
              (eff-vars$ (get-vars #\$ eff-vars))
              (eff-types (delete 'fluent eff-types)))
          (let ((action (make-action
                         :name name
                         :duration duration
                         :precondition-variables pre-vars?
                         :precondition-types pre-types
                         :precondition-lambda `(lambda ,(cons 'state pre-vars?)
                                                 (let ,(map 'list #'(lambda ($var)
                                                                       (list $var 0))
                                                           pre-vars$)
                                                   ;special allows setting of local $var
                                                   (declare (special ,@pre-vars$))
                                                   (when ,(translate precondition 'pre)
                                                     ;return fluent values or satisfied pre
                                                     (or (list ,@eff-vars$) t))))
                         :effect-variables eff-vars?
                         :effect-types eff-types
                         :effect-lambda `(lambda (state ,@eff-vars? ,@eff-vars$)
                                           (let (changes)
                                             ,(translate effect 'eff)
                                             changes)))))
            (dolist (form (list (action-precondition-lambda action) 
                                (action-effect-lambda action)))
              (when (not (ut::walk-tree-until (lambda (x)
                                                (eq x 'state))
                                              (third form))) ;db in the lambda body?
                 (push '(declare (ignore state)) (cddr form))))
            (pushnew action *actions* :test #'equalp)
            action))))))


(defmacro define-init (&rest propositions)
  `(install-init ',propositions))


(defun install-init (propositions)
  (declare (hash-table *db*) (special *db*))
  (format t "Installing initial propositional database...~%")
  (loop for proposition in propositions
      do (add-proposition proposition *db*))
  (add-proposition '(always-true) *db*)
  *db*)


(defmacro define-goal (form)
  `(install-goal ',form))


(defun install-goal (form)
  (declare (special *goal*))
  (format t "Installing goal...~%")
  (setq *goal* `(lambda (state) ,(translate form 'pre)))
  (setup))
