;;;; Filename: utilities.lisp

;;;; Collection of utilities for the Wouldwork Planner.


(in-package :ut)


(set-pprint-dispatch '(array * (* *))
  (lambda (stream array)
    "User friendly printout for a state representation containing a 2D array."
    (declare (simple-array array))
    (loop for i below (first (array-dimensions array))
      do (format stream "~%    ")
         (loop for j below (second (array-dimensions array))
           do (let ((cell (aref array i j)))
                (format stream "~A" cell))))))


(defmacro if-it (form then &optional else)
  "Binds the variable 'it' to the result of evaluating the form,
   which can then be used subsequently in the then or else clauses."
  `(let ((it ,form))
     (if it ,then ,else)))


(defmacro prt (&rest vars)
  "Print the names & values of given variables or accessors.
   Can wrap around an expression, returning its value."
  `(progn ,@(loop for var in vars
              collect `(format t "~&  ~S => ~S~%" ',var ,var))
          ,@(last `,vars)))


(defun profile (fn &rest args)
  "Deterministically profiles a :ww function"
  (sb-profile:reset)
  (sb-profile:profile "WOULDWORK-PKG")
  (apply fn args)
  (sb-profile:report))


(defun ht-set-difference (hash-table-1 hash-table-2)
  "Returns difference as a list between two ht sets."
  (let (result)
    (maphash (lambda (key value)
               (declare (ignore value))
               (unless (gethash key hash-table-2)
                 (push key result)))
             hash-table-1)
    result))


(defun forget (sym)
  "Removes a symbol from the current package."
  (makunbound sym)
  (fmakunbound sym)
  (setf (symbol-plist sym) nil))


(defmacro mvb (vars values-form &rest body)
  "Abbreviation for multiple-value-bind."
  `(multiple-value-bind ,vars ,values-form ,@body))


(defmacro mvs (vars values-form &rest body)
  "Abbreviation for multiple-value-setq."
  `(multiple-value-setq ,vars ,values-form ,@body))


(define-modify-macro sortf (function &rest sort-key)  ;can include function in &rest
  sort
  "Modifies a referenced sequence by sorting it.")


(defun merge-sort (item items predicate)
  "Adds an item to list of items sorted."
  (merge 'list (list item) (copy-list items) predicate))
  
  
(declaim (ftype (function (list list) list) remove-at-indexes))
(defun remove-at-indexes (idxs lst)
  "Removes items at given indexes from a list."
  (loop for i from 0
    for elt in lst
    unless (member i idxs :test #'=)
    collect elt))


(declaim (ftype (function (list list) list) collect-at-indexes))
(defun collect-at-indexes (idxs lst)
  "Collect items at given indexes from a list."
  (loop for i from 0
    for elt in lst
    when (member i idxs :test #'=)
    collect elt))


(defun subst-items-at-ascending-indexes (items idxs lst)
  "Substitutes for elements at given indexes in a list.
   Indexes & items must correspond and be in ascending order."
  (loop for i from 0
    for elt in lst
    if (and idxs (= i (first idxs)))
      collect (first items)
      and do (pop idxs)
             (pop items)
      else collect elt))


(declaim (ftype (function (list) list) segregate-plist))
(defun segregate-plist (plist)
  "Returns two lists, the list of properties and the list of values in plist.
   Ex call: (destructuring-bind (properties values) (segregate-plist plist) ..."
  (loop with properties and values
    for (property value) on plist by #'cddr
    if (listp property)
      do (loop for item in property
           do (push item properties)
              (push value values))
      else do (push property properties)
              (push value values)
    finally (return (list (reverse properties) (reverse values)))))


(declaim (ftype (function (&rest t) (values symbol boolean)) intern-symbol))
(defun intern-symbol (&rest args)
  "Interns a symbol created by concatenating args.
   Based on symb in Let Over Lambda."
  (flet ((mkstr (&rest args)
           (with-output-to-string (s)
             (dolist (a args) (princ a s)))))
    (values (intern (apply #'mkstr args)))))


(declaim (ftype (function (list list) list) list-difference))
(defun list-difference (lst sublst)
  "Returns lst with elements in sublst removed."
  (remove-if (lambda (item)
               (member item sublst))
    lst))


(declaim (ftype (function (function list) list) walk-tree))
(defun walk-tree (fun tree)
  ;Apply fun to each cons and atom in tree.
  (subst-if t
            (constantly nil)
            tree
            :key fun))


(defun ninsert-list (new-element position lst)
  "Destructively inserts new element in a list at given position <= length lst"
  (if (zerop position)
    (push new-element lst)
    (push new-element (cdr (nthcdr (1- position) lst))))
  lst)


(declaim (ftype (function (t list) list) intersperse))
(defun intersperse (element lst)
  "Returns a list with element inserted at odd indexed locations."
  (loop for item in lst
    append (list item element)))


(declaim (ftype (function (list) string) interleave+))
(defun interleave+ (lst)
  "Inserts a + sign between list items."
  (format nil "~{~A~^+~}" lst))


(declaim (ftype (function (list) list) regroup-by-index))
(defun regroup-by-index (list-of-lists)
  "Regroups all first elements together, second elements together, etc into a new
   list-of-lists. Changes instantiate-types into arg format for some, every, etc."
  (if (every #'null list-of-lists)
    (mapcar #'list list-of-lists)
    (apply #'mapcar #'list list-of-lists)))

#|
(declaim (ftype (function (list) list) regroup-by-index))
(defun regroup-by-index (list-of-lists)
  "Regroups all first elements together, second elements together, etc into a new
   list-of-lists. Changes instantiate-types into arg format for some, every, etc."
  (let ((regrouping (apply #'mapcar #'list list-of-lists)))
    (if regrouping
      (mapcar (lambda (lst) `(quote ,lst)) regrouping)
      (mapcar (lambda (lst)
                (declare (ignore lst))
                `(quote ,nil))
              list-of-lists))))
|#     

(declaim (ftype (function (list) list) quote-elements))
(defun quote-elements (lst)
  "Quotes the individual elements of a list."
  (or (mapcar (lambda (elem) `',elem) lst)
      '((quote nil))))


(declaim (ftype (function (list) list) map-product-less-bags))
(defun map-product-less-bags (lists)
  "Performs alexandria:map-product but leaves out combinations with duplicates."
  (if (car lists)
    (mapcan (lambda (inner-val)
              (mapcan (lambda (outer-val)
                        (unless (member outer-val inner-val)
                          (list (cons outer-val inner-val))))
                (car lists)))
      (map-product-less-bags (cdr lists)))
    (list nil)))


(defgeneric show (object &rest rest)
  (:documentation "Displays an object in a user-friendly format."))


(defmethod show ((table hash-table) &key (sort-by 'key))
  "Displays a hash table line-by-line, sorted either by key or val."
  (declare (hash-table table))
  (when (= (hash-table-count table) 0)
    (format t "~&~A Empty~%" table)
    (return-from show))
  (let (alist)
    (maphash
      (lambda (key val)
        (push (cons (princ-to-string key) (prin1-to-string val))
          alist))
      table)
    (setf alist 
      (sort alist #'string< :key (ecase sort-by (key #'car) (val #'cdr))))
    (loop for (key . val) in alist
      do (format t "~&~A ->~10T ~A~%" key val)))
  t)


(defmethod show ((fn function) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" (function-lambda-expression fn))
  t)


(defmethod show ((lst list) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" lst))


(defmethod show ((object t) &rest rest)
  "Prints any basic lisp object."
  (declare (ignore rest))
  (format t "~&~S~%" object)
  t)


(defun print-ht (table) 
  "Prints a hash table line by line."
  (declare (hash-table table))
  (format t "~&~A" table)
  (maphash #'(lambda (key val) (format t "~&~A ->~10T ~A" key val)) table)
  (terpri)
  table)


(defun hash-table-same-keys-p (ht1 ht2)
  "Returns t if two hash tables have the same keys."
  (declare (hash-table ht1 ht2))
  (when (= (hash-table-count ht1) (hash-table-count ht2))
    (maphash (lambda (ht1-key ht1-value)
               (declare (ignore ht1-value))
               (unless (gethash ht1-key ht2)
                 (return-from hash-table-same-keys-p nil)))
             ht1)
    t))


(defun hash-table-present-p (key ht)
  "Determines if a key is present in ht."
  (declare (hash-table ht))
  (mvb (* present) (gethash key ht)
    present))


(defun save (object file)
  "Saves an object to a file so it can be read in later."
  (with-open-file (out-stream file :direction :output)
    (let ((*print-readably* t))
      (pprint object out-stream))))


(defun retrieve (file)
  "Retrieves one or more objects from a file."
  (with-open-file (in-stream file :direction :input)
    (loop for object = (read in-stream nil in-stream)
      until (eq object in-stream)
      collect object into objects
      finally (return (if (= (length objects) 1)
                        (first objects)
                        objects)))))


(defun extract-symbols (x.y)
  "Splits a symbol, whose print name contains the delimiter #\., into two symbols."
  (let* ((x.y-string (string x.y))
         (index (position #\. x.y-string :test #'char=)))
    (values (intern (subseq x.y-string 0 index)) (intern (subseq x.y-string (1+ index))))))
