;;; Filename: ww-macros.lisp

(in-package :ww)


(defmacro code-if-bounding-function? ()
   (when (fboundp 'bounding-function?)
     `(let ((fn (case *solution-type* (min-value #'+) (max-value #'-))))
        (ut::mvb (current-cost current-upper)
                 (funcall #'bounding-function? (node.state current-node))
         (setf current-cost (funcall fn current-cost))
         (setf current-upper (funcall fn current-upper))
         (cond ((> current-cost *upper-bound*)
                  (update-search-tree (node.state current-node) (node.depth current-node) "State killed by bounding")
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "~&State killed by bounding")
                                       (format t "~&current-cost = ~F > *upper-bound* = ~F~%" current-cost *upper-bound*))
                  (close-barren-nodes current-node open)
                  (finalize-path-depth (node.depth current-node))
                  (return-from df-bnb1 nil))
               ((< current-upper *upper-bound*)
                  #+:wouldwork-debug (when (>= *debug* 3)
                                       (format t "Updating *upper-bound* from ~F to current-upper ~F~%" *upper-bound* current-upper))
                  (setf *upper-bound* current-upper)))))))
