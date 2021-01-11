;;; Filename:  converter.lisp

;;; Procedures for converting database hashtable lookups from symbols to integers


(in-package :ww)


(defun do-integer-conversion ()
  "Convert all objects to integers & put in idatabases."
  (associate-objects-with-integers)
  (iter (for (type constants) in-hashtable *types*)
    (iter (for constant in constants)
      (setf (gethash (convert-to-integer (list type constant)) *static-idb*) t)))
  (iter (for (proposition value) in-hashtable *db*)
        (for iproposition = (convert-to-integer proposition))
        (setf (gethash iproposition *idb*) value)
        (setf (gethash iproposition (problem-state.idb *start-state*)) value))
  (iter (for (proposition value) in-hashtable *hdb*)
        (for iproposition = (convert-to-integer proposition))
        (setf (gethash iproposition *hidb*) value)
        (setf (gethash iproposition (problem-state.hidb *start-state*)) value))
  (iter (for (proposition value) in-hashtable *static-db*)
        (for iproposition = (convert-to-integer proposition))
        (setf (gethash iproposition *static-idb*) value))
  (iter (for (proposition value) in-hashtable *hap-db*)
        (for iproposition = (convert-to-integer proposition))
        (setf (gethash iproposition *hap-idb*) value)
        (setf (gethash iproposition (problem-state.hidb *start-state*)) value))
  (iter (for action in *actions*)
        (setf (action.iprecondition-lambda action)
          (subst-int-code (copy-tree (action.precondition-lambda action))))
        (setf (action.ieffect-lambda action)
          (subst-int-code (copy-tree (action.effect-lambda action)))))
  (iter (for fname in (append *query-names* *update-names*))
        (set fname (subst-int-code (symbol-value fname))))
  (iter (for obj in *happenings*)
        (set obj (subst-int-code (copy-tree (get obj :interrupt)))))
  (setf *goal* (subst-int-code *goal*))
  (setf *constraint* (subst-int-code *constraint*)))


(defun associate-objects-with-integers ()
  "Build list of all object constants requiring conversion."
  (let (objects)    (push 'always-true objects)
    (push 'waiting objects)
    (push nil objects)
    (iter (with flat-codes = (append (alexandria:flatten *goal*) (alexandria:flatten *constraint*)))
          (for item in flat-codes)
          (when (numberp item)
            (collecting item into numbers))
          (finally (alexandria:appendf objects numbers)))
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
        (cond ((atom item) nil)
              ((and (consp item) (eql (first item) 'gethash)
                    (consp (second item)) (eql (first (second item)) 'list))
                 (setf (second item) (convert-prop-list (second item)))
                 (cond ((equal (third item) '(problem-state.db state))
                          (setf (third item) (list 'problem-state.idb 'state)))
                       ((eql (third item) '*static-db*)
                          (setf (third item) '*static-idb*))
                       ((equal (third item) '(merge-db-hdb state))
                          (setf (third item) (list 'merge-idb-hidb 'state)))
                       (t (error "~%Error in subst-int-code: ~A~%" (third item)))))
              ((and (consp item) (eql (first item) 'commit1))
                 (setf (second item) (list 'problem-state.idb 'state)))
              (t (subst-int-code item)))
        (finally (return code-tree))))


(defun convert-to-integer (proposition)
  (iter (for item in proposition)
        (for multiplier in '(1 1000 1000000 1000000000 1000000000000))
        (ut::if-it (gethash item *constant-integers*)
          (summing (* ut::it multiplier))
          (progn (incf *last-object-index*)
                 (when (>= *last-object-index* 1000)
                   (error "~%Design Limit Error: Total # actual + derived planning objects > 999~%"))
                 (setf (gethash item *constant-integers*) *last-object-index*)
                 (setf (gethash *last-object-index* *integer-constants*) item)
                 (summing (* *last-object-index* multiplier))))))


(defun convert-prop-list (prop-list)
  "Converts a statement form in an action--eg, (list 'loc ?jammer $area)
   to an integer key form for efficient db access."
  (iter (for item in (cdr prop-list))
        (for multiplier in '(1 1000 1000000 1000000000 1000000000000))
        (for new-item = (cond ((and (consp item) (eql (car item) 'quote))
                                 (* (gethash (second item) *constant-integers*) multiplier))
                              ((and (symbolp item)
                                    (or (char= (char (symbol-name item) 0) #\$)
                                        (char= (char (symbol-name item) 0) #\?)))
                                 `(* (gethash ,item *constant-integers*) ,multiplier))
                              ((numberp item)
                                 (* (gethash item *constant-integers*) multiplier))
                              (t (error "~%Error in convert-prop-list: ~A invalid in ~A~%"
                                        item prop-list))))
        (collect new-item into new-items)
        (finally (return (cons '+ new-items)))))
        

