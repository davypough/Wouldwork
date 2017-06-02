;;;; Filename: utilities.lisp

;;;; Collection of utilities for the Wouldwork Planner.


(in-package :ut)


(defmacro if-it (form then &optional else)
  "Binds the variable 'it' to the result of evaluating the form,
   which can then be used subsequently in the then or else clauses."
  `(let ((it ,form))
     (if it ,then ,else)))


(defmacro prt1 (var)
  "Print the name of a single variable or accessor and its value."
  `(format t "~&  ~S => ~S" ',var ,var))


(defmacro prt (&rest vars)
  "Print the names & values of a list of variables or accessors."
  `(progn ,@(loop for var in vars collect `(prt1 ,var))
          t))


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
     finally (return (list (nreverse properties) (nreverse values)))))


(defun walk-tree-until (fun tree)
  "Walks a tree until the function returns true,
   returning the atom at that point, or nil if never true."
  (not (tree-equal tree tree
                :test (lambda (element-1 element-2)
                        (declare (ignore element-2))
                        (let ((result (funcall fun element-1)))
                          (if result
                            (return-from walk-tree-until element-1)
                            t))))))


(defun delete-subsets (set-of-sets)
  "Destructively modifies set-of-sets with all subsets deleted."
  (declare (list set-of-sets))
  (setq set-of-sets (delete-duplicates set-of-sets :test #'subsetp))
  (setq set-of-sets (nreverse set-of-sets))
  (delete-duplicates set-of-sets :test #'subsetp))


(defun intersperse (element lst)
  "Returns a list with element inserted at odd indexed locations."
  (loop for item in lst
      append (list item element)))


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


(defmethod show ((function function) &rest rest)
  (declare (ignore rest))
  (format t "~A~%" (function-lambda-expression function))
  function)


(defmethod show ((object t) &rest rest)
  "Prints any basic lisp object."
  (declare (ignore rest))
  (format t "~A~%" object)
  object)
