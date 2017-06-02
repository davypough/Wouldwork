;;; Filename: translator.lisp

;;; Translates a domain file into lisp.


(in-package :ww)


(defun translate-atom (form)
  ;Eg, (velocity ?car wheel1 50 $direction) -> (list 'velocity ?car 'wheel1 50 $direction)
  `(list ,@(loop for item in form
               if (or (numberp item)   ;numerical fluent
                      (and (symbolp item)  ;variable
                           (or (varp #\? item) (varp #\$ item)))
                      (listp item))  ;lisp expression
               collect item
               else collect `(quote ,item))))  ;relation or object

#|
(defun translate-negative-atom (form)
  ;Eg, (not (loc me ?area)) -> (list 'not (list 'loc 'me ?area))
  `(list 'not ,(translate-atom (second form))))


(defun translate-literal (form)
  (if (eq (car form) 'not)
      (translate-negative-atom form)
    (translate-atom form)))
|#

(defun translate-std-relation (form flag)
  ;Form consists only of the relation, ?vars, and/or objects.
  ;Eg, (loc me ?area) -> (gethash (list 'loc 'me ?area) (problem-state-db state))  ;pre
  ;                   -> (push (list 'loc 'me ?area) changes)  ;eff
  (ecase flag
    (pre `(gethash ,(translate-atom form) (problem-state-db state)))
    (eff `(push ,(translate-atom form) changes))))


(defun translate-fluent-relation (form flag)
  ;Eg, (contents ?jug $amt) -> (loop ... (set $amt value)) ;pre
  ;                         -> (push (list 'contents ?jug 15) changes) ;eff
  (ecase flag
    (pre `(let ((fluents ',(get-fluents form))
                (values (gethash ,(translate-atom (cull-fluents form))
                                 (problem-state-db state))))
            (when values
              (loop for fluent in fluents
                    for value in values do
                    (cond ((symbolp fluent) (set fluent value))
                          ((realp fluent) (when (not (= fluent value)) (return nil)))
                          (t (error "Given fluent not a $var or real number: ~A" fluent)))
                    finally (return t)))))
    (eff `(push ,(translate-atom form) changes))))


(defun translate-relation (form flag)
  ;Eg, (contents ?jug $amt) -> (translate-fluent-relation)
  ;                         -> (push (list contents ?jug 15) changes)
  (ecase flag
    (pre (cond ((form-has-fluents form)  ;numbers and/or $vars
                  (translate-fluent-relation form flag))
               (t (translate-std-relation form flag)))) ;no fluents
    (eff `(push ,(translate-atom form) changes))))


(defun translate-function (form flag)
  ;Eg, (elevation ?support $elevation) -> (translate-fluent-function)
  ;                                    -> (push (list contents ?jug 15) changes)
  (ecase flag
    (pre (cond ((form-has-fluents form)  ;numbers and/or $vars
                  (translate-fluent-function form flag))
               (t (translate-std-relation form flag)))) ;no fluents
    (eff `(push ,(translate-atom form) changes))))


(defun translate-fluent-function (form flag)
  ;Eg, (elevation ?support $elevation) -> (translate-fluent-function)
  ;                                    -> (push (list contents ?jug 15) changes)
  (ecase flag
    (pre `(multiple-value-setq ,(get-fluents form)
            (funcall ',(car form) state ,@(cull-fluents (cdr form)))))
                     
;                     ,@(map 'list #'(lambda (var) (if (varp #\$ var) `(quote ,var) var))
;                                      (cdr form)))))
    
    (eff `(push ,(translate-atom form) changes))))


(defun translate-negative-relation (form flag)
  ;Translates a 'not literal' form (ie, a negated literal).
  (ecase flag
    (pre (cond ((form-has-fluents (cadr form))    ;numbers and/or $vars
                  `(not ,(translate-fluent-relation (cadr form) flag)))
               (t `(not ,(translate-std-relation (cadr form) flag))))) ;no fluents
    (eff `(push (list 'not ,(translate-atom (cadr form))) changes))))
  
  
(defun translate-existential (form flag)
  ;Translate to universal equivalence if in eff. To do.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (ut::segregate-plist parameters)
      `(some (lambda ,(remove-if (lambda (v) (varp #\$ v)) vars)
                 (let ,(mapcar (lambda (f) (list f 0)) 
                         (remove-if-not (lambda (v) (varp #\$ v)) vars))
                   (declare (special ,@(remove-if-not (lambda (v) (varp #\$ v)) vars)))
                   ,(translate body flag)))
             ,@(map 'list (lambda (x) `(quote ,x))
                 (ut::regroup (distinct-instantiations (remove 'fluent types))))))))


(defun translate-universal (form flag)
  ;The universal is interpreted differently for pre vs eff. In pre, 
  ;(forall (vars) (if form1 form2)) is true if form1 & form2 are both always true,
  ;or if form1 is always false. In eff, it is always true, since effect processing
  ;continues no matter what.
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types) (ut::segregate-plist parameters)
      (ecase flag
        (pre `(every (lambda ,(remove-if (lambda (v) (varp #\$ v)) vars)
                       (let ,(mapcar (lambda (f) (list f 0)) 
                              (remove-if-not (lambda (v) (varp #\$ v)) vars))
                         (declare (special ,@(remove-if-not (lambda (v) (varp #\$ v)) vars)))
                         ,(translate body flag)))
                     ,@(map 'list #'(lambda (x) `(quote ,x))
                         (ut::regroup (distinct-instantiations (remove 'fluent types))))))
        (eff `(progn (every (lambda ,(remove-if (lambda (v) (varp #\$ v)) vars)
                              (let ,(mapcar (lambda (f) (list f 0)) 
                                      (remove-if-not (lambda (v) (varp #\$ v)) vars))
                                (declare (special ,@(remove-if-not (lambda (v) (varp #\$ v)) vars)))
                                ,(translate body flag)))
                            ,@(map 'list #'(lambda (x) `(quote ,x))
                                (ut::regroup (distinct-instantiations (remove 'fluent types)))))
                t))))))

     
(defun translate-connective (form flag)
  `(,(car form) ,@(loop for item in (cdr form)
                      collect (translate item flag))))


(defun translate-conditional (form flag)
  ;The IF form is logically true (by default) even if condition is false.
  ;Normally flag=pre not used, but available for side effects, if needed.
  (ecase flag
    (pre `(if ,(translate (second form) 'pre)
            ,(translate (third form) 'pre)
            (progn ,(translate (fourth form) 'pre) t)))
    (eff `(if ,(translate (second form) 'pre)
            (progn ,(translate (third form) 'eff) t)
            (progn ,(translate (fourth form) 'eff) t)))))


(defun translate-derived (form flag)
  (let* ((alist (pairlis (remove-if (lambda (v) (varp #\$ v))
                                    (first (gethash (car form) *derived*)))
                         (get-vars #\? (cdr form))))
         (new-form (sublis alist (second (gethash (car form) *derived*)))))
    (translate new-form flag)))


;(defun translate-function (form flag)
;  (declare (ignore flag))
;  `(funcall (gethash ',(car form) *function-names*) 
;            state ,@(map 'list #'(lambda (var) (if (varp #\$ var) `(quote ,var) var))
;                         (cdr form))))


(defun translate (form flag)
  (declare (special *relations* *derived* *connectives*))
  (cond ((eq form nil) t)  ;if form=nil simply continue processing
        ((eq (car form) 'exists) (translate-existential form flag))  ;specialty first
        ((eq (car form) 'forall) (translate-universal form flag))
        ((eq (car form) 'if) (translate-conditional form flag))
        ((and (eq (car form) 'not) (gethash (caadr form) *relations*))  ;before connectives
           (translate-negative-relation form flag))
        ((member (car form) *connectives*) (translate-connective form flag))
        ((gethash (car form) *derived*) (translate-derived form flag))  ;derived before relations
        ((gethash (car form) *relations*) (translate-relation form flag))
        ((member (car form) *function-names*) (translate-function form flag))
        ((fboundp (car form)) form)   ;any lisp function
        (t (translate-function form flag))))
