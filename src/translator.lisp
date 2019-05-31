;;; Filename: translator.lisp

;;; Translates a domain file containing formulas into lisp.


(in-package :ww)


(defun translate-atom (form)
  ;Eg, (velocity ?car wheel1 50 $direction) -> (list 'velocity ?car 'wheel1 50 $direction)
  (check-type form (satisfies atomic-formula))
  `(list ,@(loop for item in form
               if (or (numberp item)   ;numerical fluent
                      (and (symbolp item)  ;variable
                           (varp item))
                      (listp item))  ;lisp expression
               collect item
               else collect `(quote ,item))))  ;relation or object


(defun translate-std-relation (form flag)
  ;form consists only of the relation, ?vars, and/or objects.
  ;Eg, (loc me ?area) -> (gethash (list 'loc 'me ?area) (problem-state-db state))  ;pre
  ;                   -> (push (list 'loc 'me ?area) changes)  ;eff
  (ecase flag
    ((pre ante) `(gethash ,(translate-atom form) 
                          ,(if (gethash (car form) *relations*)
                             '(problem-state-db state)
                             '*static-db*)))
    (eff `(push ,(translate-atom form) changes))))


(defun translate-function (form flag)
  ;form consists only of the relation, ?vars, and/or objects.
  ;Eg, (elevation! ?support)  ;pre
  ;    (append (disengage-jammer! ?jammer $target) changes)  ;eff
  (check-type form (satisfies function-formula))
  (let ((fn-call (concatenate 'list (list (car form))
                                    (list 'state)
                                    (mapcar (lambda (arg)
                                              (if (and (symbolp arg)
                                                       (not (varp arg)))
                                                `(quote ,arg)
                                                arg))
                                            (cdr form)))))
    (ecase flag
      ((pre ante) `,fn-call)
      (eff `(let ((new-changes ,fn-call))
              (when new-changes
                (setf changes (append changes new-changes))))))))


(defun translate-negative-relation (form flag)
  ;Translates a negated form.
  (declare (hash-table *relations*))
  (if (gethash (car (second form)) *relations*)
    (ecase flag
      ((pre ante) `(not ,(translate-std-relation (second form) flag)))
      (eff `(push (list 'not ,(translate-atom (second form))) changes)))
    (ecase flag
      ((pre ante) `(not ,(translate (second form) flag)))
      (eff (error "~%Error: Only literals allowed in effect statement negative assertions: ~A~2%"
                  form)))))


(defun translate-binding (form flag)
  ;Translates a binding for a positive form, returns nil if there are no value bindings.
  (declare (ignore flag))
  `(iter (with values = (gethash ,(translate-atom (remove-if #'$varp (second form)))
                                 ,(if (gethash (caadr form) *relations*)
                                    '(problem-state-db state)
                                    '*static-db*)))
         (for var in ',(remove-if-not #'$varp (second form)))
         (for val in values)
         (setf (symbol-value var) val)
         (finally (return values))))


(defun translate-followup (form flag)
  ;Processes a trigger followup form for next & finally.
  (declare (ignore flag))
  (let ((base-form (second form)))
    `(push (list ',(car base-form) ,@(cdr base-form)) followups)))


(defun translate-commit (form flag)
  ;Commits a form directly to the current state database.
  (declare (ignore flag))
  `(progn 
     ,@(iter (for item in (cdr form))
             (if (eql (car item) 'not)
               (collect `(update (problem-state-db state)
                                 (list 'not ,(translate-atom (second item)))))
               (collect `(update (problem-state-db state)
                                 ,(translate-atom item)))))))


(defun translate-existential (form flag)
  ;The existential is interpreted differently for pre vs eff.  
  ;In pre, (exists (vars) form) is true when form is true for any instantiation of vars, otherwise false.
  ;In eff, it asserts form for the first instantiation of the vars, and then exits.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-?variables))
      (check-type types (satisfies list-of-parameter-types))
        `(some (lambda ,vars
                   ,(translate body flag))
               ,@(or (mapcar (lambda (x) `(quote ,x))
                             (ut::regroup (type-instantiations types)))
                     (make-list (length vars) :initial-element nil))))))
         

(defun translate-universal (form flag)
  ;The universal is interpreted differently for pre vs eff.  
  ;In pre, (forall (vars) form) is true when form is always true, otherwise false.
  ;In eff, it asserts form for every instantiation of the vars.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-?variables))
      (check-type types (satisfies list-of-parameter-types))
        `(every (lambda ,vars
                    ,(translate body flag))
                ,@(or (mapcar (lambda (x) `(quote ,x))
                             (ut::regroup (type-instantiations types)))
                     (make-list (length vars) :initial-element nil))))))


(defun translate-doall (form flag)
  ;The doall form is a generator for all its variable instances. It always returns true. 
  ;It can be used to do something for all instances of its variables.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-?variables))
      (check-type types (satisfies list-of-parameter-types))
        `(mapcar (lambda ,vars
                     ,(translate body flag))
                 ,@(or (mapcar (lambda (x) `(quote ,x))
                               (ut::regroup (type-instantiations types)))
                       (make-list (length vars) :initial-element nil))))))


(defun translate-connective (form flag)
  ;Translates and, or statements.
  (check-type (car form) (member not and or))
  (ecase flag
    ((pre ante)  `(,(car form) ,@(loop for item in (cdr form)
                                     collect (translate item flag))))
    (eff (error "~%Error: AND/OR not allowed in effect statement: ~A~2%"
                form))))


(defun translate-conditional (form flag)
  ;Returns t if condition satisfied or nil if condition not satisfied.
  (declare (ignore flag))
  (when (or (and (third form) (eql (car (third form)) 'and))
            (and (fourth form) (eql (car (fourth form)) 'and)))
    (error "~%ERROR: AND not allowed in <then> or <else> clause of IF statement: ~A~2%"
           form))
  (if (fourth form)  ;else clause
    `(if ,(translate (second form) 'ante)
       ,(translate (third form) 'eff)
       ,(translate (fourth form) 'eff))
    `(if ,(translate (second form) 'ante)
       ,(translate (third form) 'eff))))


(defun translate-assertion (form flag)
  ;Translates an assert relation.
  (ecase flag
    ((pre ante) (error "~%Error: ASSERT statement not allowed as a precondion~%~A~%"
                       form))
    (eff `(progn ,@(loop for statement in (cdr form)
                         collect (translate statement flag))))))


(defun translate-do (form flag)
  ;Translates a do set of clauses.
  `(progn ,@(loop for statement in (cdr form)
                  collect (translate statement flag))))


(defun translate-setq (form flag)  ;always need to return t
  ;Translates a setq statement. Used to assign a variable the value of a function.
  `(progn (setq ,(second form) ,(translate (third form) flag)) t))


(defun translate-print (form flag)
  ;Translates a print statement for debugging actions.
  `(print ,(translate (second form) flag)))


(defun translate (form flag)  ;test-then distinguishes between if stmt forms
  ;Beginning translator for all forms in actions.
  (cond ((atom form) form)  ;atom translates as itself
        ((eql form nil) t)  ;if form=nil simply continue processing
        ((eql (car form) 'assert) (translate-assertion form flag))
        ((member (car form) '(forsome exists exist)) (translate-existential form flag))  ;specialty first
        ((member (car form) '(forall forevery every)) (translate-universal form flag))
        ((eql (car form) 'doall) (translate-doall form flag))
        ((eql (car form) 'if) (translate-conditional form flag))
        ((eql (car form) 'do) (translate-do form flag))
        ((eql (car form) 'bind) (translate-binding form flag))
        ((member (car form) '(finally next)) (translate-followup form flag))
        ((eql (car form) 'commit) (translate-commit form flag))
        ((eql (car form) 'setq) (translate-setq form flag))
        ((eql (car form) 'print) (translate-print form flag))
        ((and (eq (car form) 'not)     ;(gethash (caadr form) *relations*))  ;before connectives
           (translate-negative-relation form flag)))
        ((member (car form) *connectives*) (translate-connective form flag))
        ((or (gethash (car form) *relations*) (gethash (car form) *static-relations*))
           (translate-std-relation form flag))
        ((member (car form) *function-names*) (translate-function form flag))
        ((fboundp (car form)) form)   ;any lisp function
        (t (translate-function form flag))))
