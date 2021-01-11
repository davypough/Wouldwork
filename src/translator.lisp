;;; Filename: translator.lisp

;;; Translates a domain file containing formulas into lisp.


(in-package :ww)


(defun merge-idb-hidb (state)
  "Merges the two databases of state."
  (let ((idb (alexandria:copy-hash-table (problem-state.idb state))))
    (maphash (lambda (key val)
               (setf (gethash key idb) val))
             (problem-state.hidb state))
    idb))


(defun translate-list (form)
  "Most basic form translation."
  `(list ,@(iter (for item in form)
             (if (or (varp item)
                     (and (listp item) (fboundp (car item))))
               (collect item)
               (collect `(quote ,item))))))


(defun translate-simple-atom (form)
  "Pre/ante only.
   Eg, (velocity ?car wheel1 50 $direction) -> (list 'velocity ?car 'wheel1 50 $direction)."
  (check-type form (satisfies atomic-formula))
  `(gethash ,(translate-list form)
            ,(if (gethash (car form) *relations*)
               (if *happenings*
                 '(merge-db-hdb state)  ;dummy function replaced by merge-idb-hidb
                 '(problem-state.db state))
               '*static-db*)))


(defun translate-fluent-atom (form)
  "Pre/ante only."
  (let* ((fluent-indices (get-prop-fluent-indices form))
         (fluentless-atom (ut::remove-at-indexes fluent-indices form))
         (fluents (ut::collect-at-indexes fluent-indices form)))  ;eg, area1, ?area1, or $area1
    `(equal (gethash ,(translate-list fluentless-atom) ,(if (gethash (car form) *relations*)
                                                          '(problem-state.db state)
                                                          '*static-db*))
            (list ,@(mapcar (lambda (x)
                              (if (varp x) x `',x))
                            fluents)))))


(defun translate-proposition (form)
  "Distinguishes fluent from non-fluent propositions."
  (if (get-prop-fluent-indices form)
    (translate-fluent-atom form)
    (translate-simple-atom form)))


(defun translate-std-relation (form flag)
  "Distinguishes between pre/ante and eff relations. General form consisting of
   the relation, ?vars, $vars, numbers, symbols, lisp objects, and functions.
   Eg, (loc me ?area) -> (gethash (list 'loc 'me ?area) (problem-state.db state))  ;pre/ante
                      -> (push (list 'loc 'me ?area) changes)  ;eff"
  (ecase flag
    ((pre ante) (translate-proposition form))
    (eff `(push ,(translate-list form) changes))))


(defun translate-negative-relation (form flag)
  "Translates a negated relation form."
    (ecase flag
      ((pre ante) `(not ,(translate-std-relation (second form) flag)))
      (eff `(push (list 'not ,(translate-list (second form))) changes))))


(defun translate-function (form flag)
  "Form consists only of the relation, ?vars, and/or objects.
   Eg, (elevation? ?support)  ;pre
       (append (disengage-jammer! ?jammer $target) changes)  ;eff"
  (let ((fn-call (concatenate 'list (list (car form))
                                    '(state)
                                    (mapcar (lambda (arg)
                                              (if (and (symbolp arg)
                                                       (not (varp arg)))
                                                `(quote ,arg)
                                                arg))
                                            (cdr form)))))
    (ecase flag
      ((pre ante) `,fn-call)
      (eff `(setf changes (nconc changes ,fn-call))))))


(defun get-prop-fluents (proposition)
  "Returns the fluent values in an arbitrary proposition."
  (let ((indices (get-prop-fluent-indices proposition)))
    (when indices
      (mapcar (lambda (index)
                (let ((item (nth index proposition)))
                  (if (and (symbolp item) (boundp item))
                    (symbol-value item)
                    item)))
        indices))))


(defun translate-binding (form flag)
  "Translates a binding for a relation form, returns nil if there are no value bindings."
  (declare (ignore flag))
  (let* ((fluent-indices (get-prop-fluent-indices (second form)))
         (fluentless-atom (ut::remove-at-indexes fluent-indices (second form))))
    `(let ((values ,(translate-simple-atom fluentless-atom)))
       (when values
         (mapc #'set ',(get-prop-fluents (second form)) values)))))
         
         
(defun translate-followup (form flag)
  "Processes a trigger followup form for next & finally."
  (declare (ignore flag))
  (let ((base-form (second form)))
    `(push (list ',(car base-form) ,@(cdr base-form)) followups)))


(defun translate-commit (form flag)
  "Commits a form directly to the current state database."
  (declare (ignore flag))
  `(progn 
     ,@(iter (for item in (cdr form))
             (if (eql (car item) 'not)
               (collect `(commit1 (problem-state.db state)
                                  (list 'not ,(translate-list (second item)))))
               (collect `(commit1 (problem-state.db state)
                                  ,(translate-list item)))))))


(defun translate-existential (form flag)
  "The existential is interpreted differently for pre vs eff.  
   In pre, (exists (vars) form) is true when form is true
   for any instantiation of vars, otherwise false.
   In eff, it asserts form for the first instantiation of the vars, and then exits."
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types restriction) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-?variables))
      (check-type types (satisfies list-of-parameter-types))
      (let* ((dynamic (intersection (alexandria:flatten (mapcar (lambda (typ)
                                                                 (gethash typ *types*))
                                                               types))
                                   *query-names*))
             (quoted-instances (unless dynamic
                                 (ut::quote-elements (list (ut::regroup-by-index (type-instantiations types
                                                                                                restriction
                                                                                                nil)))))))
        `(apply #'some
                (lambda ,vars
                 ,(translate body flag))
                ,@(if dynamic
                    `((or (ut::regroup-by-index (type-instantiations ',types
                                                                     ,(case (car parameters)
                                                                        (combinations 'combinations)
                                                                        (dot-products 'dot-products))
                                                                     state))
                          '(nil)))
                    (if (equal quoted-instances '('nil))
                      (make-list (length types) :initial-element nil)
                      quoted-instances)))))))
      

(defun translate-universal (form flag)
  "The universal is interpreted differently for pre vs eff.  
   In pre, (forall (vars) form) is true when form is always true, otherwise false.
   In eff, it asserts form for every instantiation of the vars."
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types restriction) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-?variables))
      (check-type types (satisfies list-of-parameter-types))
      (let* ((dynamic (intersection (alexandria:flatten (mapcar (lambda (typ)
                                                                 (gethash typ *types*))
                                                               types))
                                   *query-names*))
             (quoted-instances (unless dynamic
                                 (ut::quote-elements (list (ut::regroup-by-index (type-instantiations types
                                                                                                restriction
                                                                                                nil)))))))
        `(apply #'every
                (lambda ,vars
                 ,(translate body flag))
                ,@(if dynamic
                    `((or (ut::regroup-by-index (type-instantiations ',types
                                                                     ,(case (car parameters)
                                                                        (combinations 'combinations)
                                                                        (dot-products 'dot-products))
                                                                     state))
                          '(nil)))
                    (if (equal quoted-instances '('nil))
                      (make-list (length types) :initial-element nil)
                      quoted-instances)))))))


(defun translate-doall (form flag)
  "The doall form is a generator for all its variable instances. It always returns true. 
   It can be used to do something for all instances of its variables."
  (let ((parameters (second form))
        (body (third form)))
    (destructuring-bind (vars types restriction) (dissect-parameters parameters)
      (check-type vars (satisfies list-of-?variables))
      (check-type types (satisfies list-of-parameter-types))
      (let* ((dynamic (intersection (alexandria:flatten (mapcar (lambda (typ)
                                                                 (gethash typ *types*))
                                                               types))
                                   *query-names*))
             (quoted-instances (unless dynamic
                                 (ut::quote-elements (list (ut::regroup-by-index (type-instantiations types
                                                                                                restriction
                                                                                                nil)))))))
        `(apply #'mapcar
                (lambda ,vars
                 ,(translate body flag))
                ,@(if dynamic
                    `((or (ut::regroup-by-index (type-instantiations ',types
                                                                     ,(case (car parameters)
                                                                        (combinations 'combinations)
                                                                        (dot-products 'dot-products))
                                                                     state))
                          '(nil)))
                    (if (equal quoted-instances '('nil))
                      (make-list (length types) :initial-element nil)
                      quoted-instances)))))))


(defun translate-connective (form flag)
  "Translates and, or statements."
  (check-type (car form) (member not and or))
  (ecase flag
    ((pre ante)  `(,(car form) ,@(iter (for item in (cdr form))
                                   (collect (translate item flag)))))
    (eff (error "~%Error: AND/OR not allowed in effect statement: ~A~2%"
                form))))


(defun translate-conditional (form flag)
  "Returns t if condition satisfied or nil if condition not satisfied."
  (declare (ignore flag))
  (when (or (and (third form) (eql (car (third form)) 'and))
            (and (fourth form) (eql (car (fourth form)) 'and)))
    (error "~%ERROR: AND not allowed in <then> or <else> clause of IF statement: ~A~2%"
           form))
  (if (fourth form)  ;else clause
    `(if ,(translate (second form) 'ante)
       ,(translate (third form) 'eff)
       ,(translate (fourth form) 'eff))
    `(when ,(translate (second form) 'ante)
       ,(translate (third form) 'eff))))


(defun translate-assertion (form flag)
  "Translates an assert relation."
  (declare (special eff-args eff-param-vars db-updates followups))
  (ecase flag
    ((pre ante) (error "~%Error: ASSERT statement not allowed as a precondion~%~A~%"
                       form))
    (eff `(let (changes)
            ,@(iter (for statement in (cdr form))
                (collect (translate statement flag)))
            (push (make-update :changes changes
                               :value ,(if (member '$objective-value eff-args)
                                         '$objective-value
                                         0.0)
                               :instantiations (list ,@eff-param-vars)
                               :followups (nreverse followups))
                  db-updates)))))


(defun translate-do (form flag)
  "Translates a simple set of clauses."
  `(progn ,@(iter (for statement in (cdr form))
              (collect (translate statement flag)))))


(defun translate-setq (form flag)  ;always need to return t, even if nil
  "Translates a setq statement. Used to assign a variable the value of a function."
  `(progn (setq ,(second form) ,(translate (third form) flag)) t))

#|
(defun translate-msetq (form flag)
  ;Translates a multi-setq statement.
  `(progn ,@(loop for var in (second form)
                for val in (translate (third form) flag)
                  collect (list 'setq var val))))
|#

(defun translate-case (form flag)
  "Translates a case statement."
  `(case ,(second form)
     ,@(iter (for clause in (cddr form))
         (collect `(,(first clause) ,@(iter (for statement in (rest clause))
                                        (collect (translate statement flag))))))))

(defun translate-print (form flag)
  "Translates a print statement for debugging actions."
  `(print ,(translate (second form) flag)))


(defun translate (form flag)  ;test-then distinguishes between if stmt forms
  "Beginning translator for all forms in actions."
  (cond ((atom form) form)  ;atom or (always-true) translates as itself
        ((null form) t)  ;if form=nil simply continue processing
        ((equal form '(always-true)) (translate-simple-atom form))
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
        ((eql (car form) 'case) (translate-case form flag))
        ((eql (car form) 'print) (translate-print form flag))
        ((eql (car form) #+sbcl 'sb-int:quasiquote #+allegro 'excl::backquote) (translate (eval form) flag))
        ((and (eql (car form) 'not) (gethash (caadr form) *relations*)) (translate-negative-relation form flag))
        ((member (car form) *connectives*) (translate-connective form flag))
        ((or (gethash (car form) *relations*) (gethash (car form) *static-relations*))
         (translate-std-relation form flag))
        ((member (car form) (append *query-names* *update-names*)) (translate-function form flag))
        ((fboundp (car form)) form)   ;any lisp function
        (t (translate-function form flag))))
