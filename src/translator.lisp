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


(defun translate-std-function (form flag)
  ;form consists only of the relation, ?vars, and/or objects.
  ;Eg, (elevation* state ?support)  ;pre
  ;    (append (disengage-jammer* ?jammer $target) changes)  ;eff
  (check-type form (satisfies function-formula))
  (let ((fn-call (concatenate 'list (list (car form)) (list 'state) (cdr form))))
    (ecase flag
      ((pre ante) `,fn-call)
      (eff `(append ,fn-call changes)))))


(defun translate-fluent-relation (form flag)
  ;Eg, (destructuring-setq '(?target) (gethash (list 'jamming ?jammer) (problem-state-db state))
  ;Returns t if fluent binding occurs, nil if form not in db.
  (declare (ignore flag))
  `(ut::destructuring-setq ',(remove-if-not #'$varp form)
                           (gethash ,(translate-atom (remove-if #'$varp form))
                              ,(if (gethash (car form) *relations*)
                                 '(problem-state-db state)
                                 '*static-db*))))


(defun translate-function (form flag)
  ;Assigns value on first variable appearance, uses value subsequently
  (if (first-fluent-appearance form flag)  ;a new fluent in form
    (translate-std-function form flag)
    (translate-std-function form flag))) ;no new fluents
    
    
(defun translate-fluent-function (form flag)
  (declare (ignore flag))
  `(ut::destructuring-setq ',(remove-if-not #'$varp form)
                           (gethash ,(translate-atom (remove-if #'$varp form))
                              ,(if (gethash (car form) *relations*)
                                 '(problem-state-db state)
                                 '*static-db*))))


(defun translate-relation (form flag)
  ;Assigns value on first variable appearance, uses value subsequently
  (if (first-fluent-appearance form flag)  ;a new fluent in form
    (translate-fluent-relation form flag)
    (translate-std-relation form flag))) ;no new fluents


(defun translate-negative-relation (form flag)
  ;Translates a 'not literal' form (ie, a negated literal).
  (if (first-fluent-appearance (second form) flag)  ;a new fluent in form
      `(not ,(translate-fluent-relation (second form) flag))
    (ecase flag
      ((pre ante) `(not ,(translate-std-relation (second form) flag)))
      (eff `(push (list 'not ,(translate-atom (second form))) changes))))) ;no new fluents


(defun translate-binding (form flag)
  ;Translates a binding for a positive form (including nil) and always returns t.
  (declare (ignore flag))
  `(progn (ut::destructuring-setq ',(remove-if-not #'$varp (second form))
                                  (gethash ,(translate-atom (remove-if #'$varp (second form)))
                                           ,(if (gethash (caadr form) *relations*)
                                              '(problem-state-db state)
                                              '*static-db*)))
          t))  ;always return t after binding, even if binding = nil


 
(defun translate-existential (form flag)
  ;The existential is interpreted differently for pre vs eff.  
  ;In pre, (exists (vars) form) is true when form is true for any instantiation of vars, otherwise false.
  ;In eff, it asserts form for the first instantiation of the vars, and then exits.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-variables))
      (check-type types (satisfies list-of-parameter-types))
      (let* ((?vars (remove-if-not #'?varp vars))
             ($vars (remove-if-not #'$varp vars))
             (*current-precondition-fluents* $vars)
             (*current-effect-fluents* $vars))
        (if $vars
          `(some (lambda ,?vars
                   (let ,$vars
                     (declare (special ,@$vars))  ;needed for (set $var ...)
                     ,(translate body flag)))
                 ,@(mapcar (lambda (x) `(quote ,x))
                     (ut::regroup (type-instantiations types))))
          `(some (lambda ,?vars
                   ,(translate body flag))
                 ,@(mapcar (lambda (x) `(quote ,x))
                     (ut::regroup (type-instantiations types)))))))))
         

(defun translate-universal (form flag)
  ;The universal is interpreted differently for pre vs eff.  
  ;In pre, (forall (vars) form) is true when form is always true, otherwise false.
  ;In eff, it asserts form for every instantiation of the vars.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-variables))
      (check-type types (satisfies list-of-parameter-types))
      (let* ((?vars (remove-if-not #'?varp vars))
             ($vars (remove-if-not #'$varp vars))
             (*current-precondition-fluents* $vars)
             (*current-effect-fluents* $vars))
        (if $vars
          `(every (lambda ,?vars
                    (let ,$vars
                      (declare (special ,@$vars))  ;needed for (set $var ...)
                      ,(translate body flag)))
                  ,@(mapcar (lambda (x) `(quote ,x))
                      (ut::regroup (type-instantiations types))))
          `(every (lambda ,?vars
                    ,(translate body flag))
                  ,@(mapcar (lambda (x) `(quote ,x))
                      (ut::regroup (type-instantiations types)))))))))


(defun translate-forfluent (form flag)
  ;The forfluent construction provides a parameter list for fluents, similar to the
  ;existential & universal quantifiers for generated vars. It provides binding for fluents for pre & eff.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-variables))
      (check-type types (satisfies list-of-parameter-types))
      (let* (($vars (remove-if-not #'$varp vars))
             (*current-precondition-fluents* $vars)
             (*current-effect-fluents* $vars))
        `(let ,$vars
           (declare (special ,@$vars))  ;needed for (set $var ...)
           ,(translate body flag))))))
  

(defun translate-connective (form flag)
  ;Translates and, or, not.
  (check-type (car form) (member not and or))
  `(,(car form) ,@(loop for item in (cdr form)
                      collect (translate item flag))))


(defun translate-conditional (form flag)
  ;Returns t if condition satisfied or nil if condition not satisfied.
  (declare (ignore flag))
  (if (fourth form)  ;else clause
    `(if ,(translate (second form) 'ante)
       ,(translate (third form) 'eff)
       ,(translate (fourth form) 'eff))
    `(if ,(translate (second form) 'ante)
       ,(translate (third form) 'eff))))


(defun translate-derived (form flag)
  ;Translates a derived relation.
  (declare (hash-table *derived*))
  (let* ((alist (pairlis (first (gethash (car form) *derived*))
                         (cdr form)))
         (new-form (sublis alist (second (gethash (car form) *derived*)))))
    (translate new-form flag)))


(defun translate-assertion (form flag)
  ;Translates an assert relation.
  (ecase flag
    ((pre ante) (format t "~%Error: ASSERT statement not allowed as a precondion~%~A~%"
                          form))
    (eff `(progn ,@(loop for statement in (cdr form)
                         collect (translate statement flag))))))


(defun translate-let (form flag)
  ;Translates a let clause. Used to bind & initialize $ variables.
  (let* (($vars (second form))
         (*current-precondition-fluents* $vars)
         (*current-effect-fluents* $vars))
    `(let ,(second form)
       (declare (special ,@(second form)))
       ,@(loop for statement in (cddr form)
               collect (translate statement flag)))))


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
        ((eql (car form) 'forall) (translate-universal form flag))
        ((member (car form) '(forfluent forfluents)) (translate-forfluent form flag))
        ((eql (car form) 'if) (translate-conditional form flag))
        ((eql (car form) 'bind) (translate-binding form flag))
        ((eql (car form) 'let) (translate-let form flag))
        ((eql (car form) 'setq) (translate-setq form flag))
        ((eql (car form) 'print) (translate-print form flag))
        ((and (eq (car form) 'not) (gethash (caadr form) *relations*))  ;before connectives
           (translate-negative-relation form flag))
        ((member (car form) *connectives*) (translate-connective form flag))
        ((gethash (car form) *derived*) (translate-derived form flag))  ;derived before relations
        ((or (gethash (car form) *relations*) (gethash (car form) *static-relations*))
           (translate-relation form flag))
        ((member (car form) *function-names*) (translate-function form flag))
        ((fboundp (car form)) form)   ;any lisp function
        (t (translate-function form flag))))
