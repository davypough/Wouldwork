;;; Filename:  backchain.lisp

;;; Procedures for backchaining from goal


(in-package :ww)


(defun install-precondition-lits (action)
  (with-slots (precondition-lambda precondition-lits effect-lambda) action
    (let (neg)  ;signals when to generate a negative literal
      (subst-if t (constantly nil) precondition-lambda
                :key (lambda (item)
                       (when (consp item)
                         (or (when (and (eql (first item) 'let)
                                        (listp (first (second item)))
                                        (eql (first (first (second item))) 'fluents))
                               (pushnew (get-lit-from-let-vars (second item)) precondition-lits))
                             (when (and (eql (first item) 'not)
                                        (eql (first (second item)) 'gethash))
                               (setf neg t))  ;next time thru process negative literal
                             (when (and (eql (first item) 'gethash)
                                        (not (gethash (second (second (second item))) (ww-get 'fluent-relation-indices))))
                               (if neg
                                   (progn (push (cons 'not (list (get-pos-lit (cdr (second item)))))
                                                precondition-lits)
                                     (setf neg nil))
                                 (push (get-pos-lit (cdr (second item)))
                                       precondition-lits)))))))
      (subst-if t (constantly nil) effect-lambda 
                :key (lambda (item)
                       (when (consp item)
                         (or (when (and (eql (first item) 'not)
                                        (eql (first (second item)) 'gethash))
                               (setf neg t))  ;next time thru process negative literal
                             (when (eql (first item) 'gethash)
                               (if neg
                                   (progn (push (cons 'not (list (get-pos-lit (cdr (second item)))))
                                                precondition-lits)
                                     (setf neg nil))
                                 (push (get-pos-lit (cdr (second item)))
                                                    precondition-lits))))))))))


(defun install-effect-adds (action)
  (with-slots (effect-lambda effect-adds) action
    (subst-if t (constantly nil) effect-lambda 
              :key (lambda (item)
                     (when (and (consp item)
                                (eql (first item) 'push)
                                (not (equal (second (second item)) '(quote not))))
                       (let ((literal (cdr (second item))))
                         (pushnew (mapcar (lambda (elt)
                                            (if (and (listp elt)
                                                     (eql (car elt) 'quote))
                                              (second elt)
                                              elt))
                                          literal)
                                  effect-adds))
                       nil)))))


(defun get-pos-lit (form)
  (loop for item in form
      if (and (listp item)
              (eql (first item) 'quote))
      collect (second item)
      else collect item))


(defun get-lit-from-let-vars (var-form)
  (let* ((rel (second (second (second (second (second var-form))))))
         (fluents (second (second (first var-form))))
         (indices (gethash rel (ww-get 'fluent-relation-indices)))
         (lit (get-pos-lit (cdr (second (second (second var-form)))))))
    (loop for fluent in fluents
          for index in indices
        do (ut::ninsert-list fluent index lit)
          finally (return lit))))


