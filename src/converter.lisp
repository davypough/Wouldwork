;;; Filename:  converter.lisp

;;; Procedures for converting database hashtable lookups from symbols to integers


(in-package :ww)


(defun do-integer-conversion ()
  (associate-objects-with-integers)
  (iter (for (type constants) in-hashtable *types*)
        (iter (for constant in constants)
              (setf (gethash (convert-to-integer (list type constant)) *static-idb*) t)))
  (iter (for (proposition value) in-hashtable *db*)
        (for iproposition = (convert-to-integer proposition))
        (setf (gethash iproposition (problem-state-idb *start-state*)) value))
  (iter (for (proposition value) in-hashtable *static-db*)
        (for iproposition = (convert-to-integer proposition))
        (setf (gethash iproposition *static-idb*) value))
  (iter (for action in *actions*)
        (setf (action-iprecondition-lambda action)
          (subst-int-code (copy-tree (action-precondition-lambda action))))
        (setf (action-ieffect-lambda action)
          (subst-int-code (copy-tree (action-effect-lambda action)))))
  (iter (for fname in *function-names*)
        (set fname (subst-int-code (eval fname))))
  (iter (for obj in *happenings*)
        (set obj (subst-int-code (copy-tree (get obj :interrupt)))))
  (setf *goal* (subst-int-code *goal*))
  (setf *constraint* (subst-int-code *constraint*)))


(defun associate-objects-with-integers ()
  (let (objects)  ;build list of all object constants requiring conversion
    (push 'always-true objects)
    (push 'waiting objects)
    (iter (with flat-codes = (append (alexandria:flatten *goal*) (alexandria:flatten *constraint*)))
          (for item in flat-codes)
          (when (numberp item)
            (collecting item into numbers))
          (finally (alexandria:appendf objects numbers)))
;    (iter (for action in *actions*)
;          (for flat-codes = (append (alexandria:flatten (action-precondition-lambda action))
;                                    (alexandria:flatten (action-effect-lambda action))))
;          (for item in flat-codes)
;          (when (numberp item)
;            (collecting item into numbers))
;          (finally (alexandria:appendf objects numbers)))  
    (iter (for (prop nil) in-hashtable *db*)
          (appending (remove-if-not #'numberp prop) into numbers)
          (finally (alexandria:appendf objects numbers))) 
    (iter (for (prop nil) in-hashtable *static-db*)
          (appending (remove-if-not #'numberp prop) into numbers)
          (finally (alexandria:appendf objects numbers)))
    (alexandria:appendf objects (iter (for (type constants) in-hashtable *types*)
                                      (collecting type)
                                      (appending constants)))
    (alexandria:appendf objects (iter (for (predicate nil) in-hashtable *relations*)
                                      (collecting predicate)))
    (alexandria:appendf objects (iter (for (predicate nil) in-hashtable *static-relations*)
                                      (collecting predicate)))
    (setf objects (delete-duplicates objects))
    (setf objects (sort objects #'string< :key (lambda (obj)
                                                 (or (and (symbolp obj) (symbol-name obj))
                                                     (and (numberp obj) (princ-to-string obj))))))
    (iter (for obj in objects)
          (for i from 100)
          (setf (gethash obj *constant-integers*) i)
          (setf (gethash i *integer-constants*) obj)
          (finally (setf *last-object-index* i)))))
  

(defun subst-int-code (code-tree)
  (iter (for item in code-tree)
        (cond ((atom item)
               nil)
              ((and (consp item)
                    (eql (car item) 'gethash)
                    (eql (caadr item) 'list))
               (setf (second item) (convert-prop-list (second item)))
               (cond ((equal (third item) '(problem-state-db state))
                      (setf (third item) (list 'problem-state-idb 'state)))
                     ((eql (third item) '*static-db*)
                      (setf (third item) '*static-idb*))
                     (t (format t "~%Error in exchange-code: ~A~%" (third item)))))
              (t (subst-int-code item)))
        (finally (return code-tree))))


(defun convert-to-integer (proposition)
  (iter (for item in proposition)
        (for multiplier in '(1 1000 1000000 1000000000 1000000000000))
        (ut::if-it (gethash item *constant-integers*)
          (summing (* ut::it multiplier))
          (progn (incf *last-object-index*)
                 (setf (gethash item *constant-integers*) *last-object-index*)
                 (setf (gethash *last-object-index* *integer-constants*) item)
                 (summing (* *last-object-index* multiplier))))))


(defun convert-to-proposition (integer)
  (iter (with x = integer)
        (for (int triple) = (multiple-value-list (truncate x 1000)))
        (collecting triple into int-list)
        (until (zerop int))
        (setf x int)
        (finally (return (mapcar (lambda (i)
                                   (gethash i *integer-constants*))
                                 int-list)))))


(defun convert-prop-list (prop-list)
  ;Converts a statement form in an action--eg, (list 'loc ?jammer $area)
  ;to an integer key form for efficient db access.
  (iter (for item in (cdr prop-list))
        (for multiplier in '(1 1000 1000000 1000000000 1000000000000))
        (for new-item = (cond ((and (consp item) (eql (car item) 'quote))
                               (* (gethash (second item) *constant-integers*) multiplier))
                              ((varp item)
                               `(* (gethash ,item *constant-integers*) ,multiplier))
                              ((numberp item)
                               (* (gethash item *constant-integers*) multiplier))
                              (t (format t "~%Error in convert-prop-list: ~A invalid in ~A~%" item prop-list))))
        (collect new-item into new-items)
        (finally (return (cons '+ new-items)))))
        
