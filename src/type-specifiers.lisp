;;; Filename: type-specifiers.lisp

;;; Used for validating the user's problem specification.

(in-package :ww)


(defun $varp (item)
  (and (symbolp item)
       (char= (char (symbol-name item) 0) #\$)))


(defun ?varp (item)
  (and (symbolp item)
       (char= (char (symbol-name item) 0) #\?)))


(defun varp (sym)
  (or (?varp sym)
      ($varp sym)))


(defun extract-type (value-symbol)
  (if ($varp value-symbol)
      (intern (subseq (symbol-name value-symbol) 1))
    value-symbol))


(defun list-of-?variables (lst)
  (and (listp lst)
       (every #'?varp lst)))


(defun list-of-variables (lst)
  (and (listp lst)
       (every (lambda (arg)
                (or (varp arg)
                    (symbolp arg)
                    (realp arg)))
              lst)))


(defun type-description (descrip)
  (or (nth-value 1 (gethash descrip *types*))
      (eq descrip 'fluent)
      (and ($varp descrip)
           (or (nth-value 1 (gethash (extract-type descrip) *types*))
               (symbolp (extract-type descrip))))
      (and (consp descrip)
           (eql (car descrip) 'either)
           (or (every (lambda (typ)
                        (gethash (extract-type typ) *types*))
                      (cdr descrip))))))


(defun list-of-parameter-types (lst)
  (every (lambda (typ)
           (or (null typ)
               (ut::hash-table-present typ *types*)
               (eq typ 'fluent)
               (and (listp typ)
                    (list-of-parameter-types typ))))
         lst))


(defun relation (rel)
  (and (listp rel)
       (iter (for rel-item in (cdr rel))
             (for rel-type in (gethash (car rel) *relations*))
             (unless (or (eq rel-item rel-type)
                         (and (listp rel-type)
                              (member rel-item (cdr rel-type)))  ;either type
                         (subsetp (gethash rel-item *types*) (gethash rel-type *types*)))
                         ;(alexandria:set-equal rel-item rel-type))
               (leave nil))
             (finally (return t)))))


(defun negative-relation (neg-rel)
  (and (listp neg-rel)
       (eql (first neg-rel) 'not)
       (relation (second neg-rel))))


(defun key-list (lst)
  (and (consp lst)
       (loop for (keyword *) on lst by #'cddr
           when (not (member keyword '(:inits :events :repeat :rebound :interrupt)))
           do (return nil)
             finally (return t))))


(defun proposition (prop)
  (and (listp prop)
       (or (gethash (car prop) *relations*)
           (gethash (car prop) *static-relations*))
       (or (null (cdr prop))
           (every (lambda (const typ)
                    (or (member const (gethash (extract-type typ) *types*))
                        (and (listp typ)
                             (eql (car typ) 'either)
                             (member const
                               (reduce #'union 
                                       (mapcar (lambda (typ)
                                                 (gethash (extract-type typ) *types*))
                                               (cdr typ)))))
                        (typep const (extract-type typ))))
              (cdr prop) (or (gethash (car prop) *relations*)
                             (gethash (car prop) *static-relations*))))))


(defun atomic-formula (form)
  (and (listp form)
       (or (gethash (car form) *relations*)
           (gethash (car form) *static-relations*))
       (every (lambda (arg)
                (or (varp arg)
                    (symbolp arg)
                    (realp arg)
                    (and (listp arg)
                         (fboundp (car arg)))))
              (cdr form))))


(defun function-formula (form)
  (and (listp form)
       (member (car form) (append *query-names* *update-names*))
       (every (lambda (arg)
                (or (varp arg)
                    (symbolp arg)
                    (realp arg)))
              (cdr form))))
