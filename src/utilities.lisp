;;;; Filename: utilities.lisp

;;;; Collection of utilities for the Wouldwork Planner.


(in-package :ut)


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


(defmacro mvb (vars values-form &rest body)
  "Abbreviation for multiple-value-bind."
  `(multiple-value-bind ,vars ,values-form ,@body))


(define-modify-macro sortf (function &rest sort-key)  ;can include function in &rest
  sort
  "Modifies a referenced sequence by sorting it.")


(defun remove-at-indexes (idxs lst)
  "Removes items at given indexes from a list."
  (loop for i from 0
        for elt in lst
        when (not (member i idxs :test #'=))
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


(defun destructuring-setq (variables values)
  ;Assigns values to dynamic variables.
  (loop for var in variables
      for val in values do
        (setf (symbol-value var) val)
        finally (return values)))


(defun segregate-plist (plist)
  "Returns two lists, the list of properties and the list of values in plist.
   Ex call: (destructuring-bind (properties values) (segregate-plist plist) ..."
  (loop with properties and values
      for (property value) on plist by #'cddr
      if (listp property)
      do (loop for item in property do
               (push item properties)
               (push value values))
      else do (push property properties)
        (push value values)
      finally (return (list (reverse properties) (reverse values)))))


(defun make-plist (props values)
  "Returns a plist of properties and values."
  (loop for prop in props
        for value in values
        append (list prop value)))


(defun create-symbol (item &key (intern nil) (package *package*))
  "Creates a symbol from an object or list of objects."
  (let* ((objects (if (atom item) (list item) item))
         (item-string (format nil "~{~A~^~}" objects)))
    (if intern
      (intern item-string (or package *package*))
      (make-symbol item-string))))


(defun list-difference (lst sublst)
  (remove-if (lambda (item)
               (member item sublst))
             lst))


(defun pushend (item lst)
  "Pushes an item onto the end of a list and returns the new list."
  (nconc lst (list item)))


(defun ninsert-list (new-element position lst)
  "Destructively inserts new element in a list at given position <= length lst"
  (if (zerop position)
      (push new-element lst)
      (push new-element (cdr (nthcdr (1- position) lst))))
  lst)


(defun walk-tree-until (fun tree)
  "Walks a tree until the function returns true for an atom (ie, non-cons),
   returning the atom at that point, or nil if never true."
  (not (tree-equal tree tree
                :test (lambda (element-1 element-2)
                        (declare (ignore element-2))
                        (if (funcall fun element-1)
                          (return-from walk-tree-until element-1)
                          t)))))


(defun find-cons-in-tree (item tree &key (test #'eql) (key #'identity))
  ;Determines if a consp item appears somewhere in a list of lists tree.
  (labels ((find-cons-in-tree-aux (tree)                                   
             (cond ((funcall test item (funcall key tree)) (return-from find-cons-in-tree tree))
                   ((listp tree) (mapc #'find-cons-in-tree-aux tree) nil))))
    (find-cons-in-tree-aux tree)))


(defun delete-subsets (set-of-sets)
  "Destructively modifies set-of-sets with all subsets deleted."
  (declare (list set-of-sets))
  (setq set-of-sets (delete-duplicates set-of-sets :test #'subsetp))
  (setq set-of-sets (reverse set-of-sets))
  (delete-duplicates set-of-sets :test #'subsetp))


(defun intersperse (element lst)
  "Returns a list with element inserted at odd indexed locations."
  (loop for item in lst
      append (list item element)))


(defun interleave+ (lst)
  "Inserts a + sign between list items."
  (format nil "~{~A~^+~}" lst))


(defun regroup (list-of-lists)
  "Regroups all first elements together, second elements together, etc into
   a new list-of-lists."
  (let* ((n (length (car list-of-lists)))
         (regrouping (make-array n :initial-element nil)))
    (loop for list in list-of-lists
        do (loop for element in list
               for i upto n
               do (push element (aref regrouping i))))
    (coerce regrouping 'list)))


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
  "Displays a hash table line-by-line, sorted either by key or value."
  (declare (hash-table table))
  (let (alist)
    (maphash (lambda (key value)
               (push (cons (format nil "~A" key)
                           (format nil "~A" value))
                     alist))
             table)
    (setf alist (sort alist #'string< :key (ecase sort-by (key #'car) (value #'cdr))))
    (loop for (key . value) in alist
        do (format t "~&~A ->~10T ~A~%" key value)))
  table)


(defmethod show ((fn function) &rest rest)
  (declare (ignore rest))
  (format t "~&~A~%" (function-lambda-expression fn))
  fn)


(defmethod show ((lst list) &rest rest)
  (declare (ignore rest))
  (format t "~&(")
  (dolist (item lst)
    (show item))
  (format t ")")
  lst)


(defmethod show ((object t) &rest rest)
  "Prints any basic lisp object."
  (declare (ignore rest))
  (format t "~&~S~%" object)
  object)


(defun print-ht (table) 
  "Prints a hash table line by line."
  (declare (hash-table table))
  (maphash #'(lambda (key val) (format t "~&~A ->~10T ~A" key val)) table)
  (terpri)
  table)


(defun hash-table-same-keys (ht1 ht2)
  ;Returns t if two hash tables have the same keys.
  (declare (hash-table ht1 ht2))
  (when (= (hash-table-count ht1) (hash-table-count ht2))
    (maphash (lambda (ht1-key ht1-value)
               (declare (ignore ht1-value))
               (unless (gethash ht1-key ht2)
                 (return-from hash-table-same-keys nil)))
             ht1)
    t))

