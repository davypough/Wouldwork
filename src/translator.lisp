;;; Filename: translator.lisp

;;; Translates a domain file into lisp.


(in-package :pl)


(defun translate_atom (form)
  ;Eg, (velocity ?car wheel1 50 $direction) -> (list 'velocity ?car 'wheel1 50 $direction)
  `(list ,@(loop for item in form
               if (or (numberp item)   ;numerical fluent
                      (and (symbolp item)  ;variable
                           (or (varp #\? item) (varp #\$ item)))
                      (listp item))  ;lisp expression
               collect item
               else collect `(quote ,item))))  ;relation or object

#|
(defun translate_negative_atom (form)
  ;Eg, (not (loc me ?area)) -> (list 'not (list 'loc 'me ?area))
  `(list 'not ,(translate_atom (second form))))


(defun translate_literal (form)
  (if (eq (car form) 'not)
      (translate_negative_atom form)
    (translate_atom form)))
|#

(defun translate_std_relation (form flag)
  ;Form consists only of the relation, ?vars, and/or objects.
  ;Eg, (loc me ?area) -> (gethash (list 'loc 'me ?area) (problem_state-db state))  ;pre
  ;                   -> (push (list 'loc 'me ?area) changes)  ;eff
  (ecase flag
    (pre `(gethash ,(translate_atom form) (problem_state-db state)))
    (eff `(push ,(translate_atom form) changes))))


(defun translate_fluent_relation (form flag)
  ;Eg, (contents ?jug $amt) -> (loop ... (set $amt value)) ;pre
  ;                         -> (push (list 'contents ?jug 15) changes) ;eff
  (ecase flag
    (pre `(loop for fluent in ',(get_fluents form)
              for value in (gethash ,(translate_atom (cull_fluents form))
                                    (problem_state-db state))
                do (cond ((symbolp fluent) (set fluent value))
                         ((realp fluent) (when (not (= fluent value)) (return nil)))
                         (t (error "Given fluent not a $var or real number: ~A" fluent)))
                finally (return t)))
    (eff `(push ,(translate_atom form) changes))))


(defun translate_relation (form flag)
  ;Eg, (contents ?jug $amt) -> (translate_fluent_relation)
  ;                         -> (push (list contents ?jug 15) changes)
  (ecase flag
    (pre (cond ((form_has_fluents form)  ;numbers and/or $vars
                  (translate_fluent_relation form flag))
               (t (translate_std_relation form flag)))) ;no fluents
    (eff `(push ,(translate_atom form) changes))))


(defun translate_function (form flag)
  ;Eg, (elevation ?support $elevation) -> (translate_fluent_function)
  ;                                    -> (push (list contents ?jug 15) changes)
  (ecase flag
    (pre (cond ((form_has_fluents form)  ;numbers and/or $vars
                  (translate_fluent_function form flag))
               (t (translate_std_relation form flag)))) ;no fluents
    (eff `(push ,(translate_atom form) changes))))


(defun translate_fluent_function (form flag)
  ;Eg, (elevation ?support $elevation) -> (translate_fluent_function)
  ;                                    -> (push (list contents ?jug 15) changes)
  (ecase flag
    (pre `(multiple-value-setq ,(get_fluents form)
            (funcall ',(car form) state ,@(cull_fluents (cdr form)))))
                     
;                     ,@(map 'list #'(lambda (var) (if (varp #\$ var) `(quote ,var) var))
;                                      (cdr form)))))
    
    (eff `(push ,(translate_atom form) changes))))


(defun translate_negative_relation (form flag)
  ;Translates a 'not literal' form (ie, a negated literal).
  (ecase flag
    (pre (cond ((form_has_fluents (cadr form))    ;numbers and/or $vars
                  `(not ,(translate_fluent_relation (cadr form) flag)))
               (t `(not ,(translate_std_relation (cadr form) flag))))) ;no fluents
    (eff `(push (list 'not ,(translate_atom (cadr form))) changes))))
  
  
(defun translate_existential (form flag)
  ;Translate to universal equivalence if in eff. To do.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (ut::segregate-plist parameters)
      `(some #'(lambda ,vars
                 ,(translate body flag))
             ,@(map 'list #'(lambda (x) `(quote ,x))
                 (ut::regroup (distinct_instantiations types)))))))


(defun translate_universal (form flag)
  ;The universal is interpreted differently for pre vs eff. In pre, 
  ;(forall (vars) (if form1 form2)) is true if form1 & form2 are both always true,
  ;or if form1 is always false. In eff, it is always true, since effect processing
  ;continues no matter what.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (ut::segregate-plist parameters)
      (ecase flag
        (pre `(every #'(lambda ,vars
                         ,(translate body flag))
                     ,@(map 'list #'(lambda (x) `(quote ,x))
                         (ut::regroup (distinct_instantiations types)))))
        (eff `(progn (every #'(lambda ,vars
                                ,(translate body flag))
                            ,@(map 'list #'(lambda (x) `(quote ,x))
                                (ut::regroup (distinct_instantiations types))))
                t))))))

     
(defun translate_connective (form flag)
  `(,(car form) ,@(loop for item in (cdr form)
                      collect (translate item flag))))


(defun translate_conditional (form flag)
  ;The IF form is logically true (by default) even if condition is false.
  ;Normally flag=pre not used, but available for side effects, if needed.
  (ecase flag
    (pre `(if ,(translate (second form) 'pre)
            ,(translate (third form) 'pre)
            (progn ,(translate (fourth form) 'pre) t)))
    (eff `(if ,(translate (second form) 'pre)
            (progn ,(translate (third form) 'eff) t)
            (progn ,(translate (fourth form) 'eff) t)))))


(defun translate_derived (form flag)
  (let* ((alist (pairlis (cull_fluents (first (gethash (car form) *derived*)))
                         (get_vars #\? (cdr form))))
         (new_form (sublis alist (second (gethash (car form) *derived*)))))
    (translate new_form flag)))


;(defun translate_function (form flag)
;  (declare (ignore flag))
;  `(funcall (gethash ',(car form) *function_names*) 
;            state ,@(map 'list #'(lambda (var) (if (varp #\$ var) `(quote ,var) var))
;                         (cdr form))))


(defun translate (form flag)
  (declare (special *relations* *derived* *connectives*))
  (cond ((eq form nil) t)  ;if form=nil simply continue processing
        ((eq (car form) 'exists) (translate_existential form flag))  ;specialty first
        ((eq (car form) 'forall) (translate_universal form flag))
        ((eq (car form) 'if) (translate_conditional form flag))
        ((and (eq (car form) 'not) (gethash (caadr form) *relations*))  ;before connectives
           (translate_negative_relation form flag))
        ((member (car form) *connectives*) (translate_connective form flag))
        ((gethash (car form) *derived*) (translate_derived form flag))  ;derived before relations
        ((gethash (car form) *relations*) (translate_relation form flag))
        ((member (car form) *function_names*) (translate_function form flag))
        ((fboundp (car form)) form)   ;any lisp function
        (t (translate_function form flag))))
