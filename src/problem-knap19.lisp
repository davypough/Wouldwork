;;;; Filename: problem-knap19.lisp

;;; Problem specification for a 19-item knapsack problem.


(in-package :ww)  ;required


(ww-set *problem* knap19)

(ww-set *solution-type* max-value)


(defstruct item  ;an item template
  (name nil :type symbol)  ;item name--eg, ITEM14
  (value 0 :type fixnum)  ;value of that item
  (weight 0 :type fixnum)  ;weight of that item
  (value/weight 0 :type ratio))  ;the ratio of the item's value/weight

(defparameter *items* nil)  ;the list of available items
(defparameter *num-items* 0)  ;total number of items
(defparameter *max-knapsack-weight* 0)  ;knapsack max weight

(defun readin (knapsack-data-file)
  ;Read in knapsack data from a file.
  (with-open-file (ifile knapsack-data-file :direction :input :if-does-not-exist nil)
    (when (not (streamp ifile)) (error "File does not exist!"))
    (setq *num-items* (read ifile))
    (setq *max-knapsack-weight* (read ifile))
    (dotimes (i *num-items*)
      (let ((value (read ifile))
            (weight (read ifile)))
        (push (make-item :name (intern (concatenate 'string "ITEM"
                                         (write-to-string (1+ i))))
                         :value value
                         :weight weight
                         :value/weight (/ value weight))
               *items*)))
    (setq *items* (sort *items* #'> :key #'item-value/weight))))
        
(setq *default-pathname-defaults*  ;point to where knapsack data file is stored
   #P"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\")

(readin "data-knap19.lisp")  ;name of the knapsack data file


(define-types
  knapsack (knapsack1)
  item (compute (loop for item in *items*
                   collect (item-name item))))


(define-dynamic-relations
  (in item knapsack)
  (load knapsack $fixnum)
  (worth knapsack $fixnum))


(define-static-relations
  (value item $fixnum)
  (weight item $fixnum))


(define-query get-best-relaxed-value? ()  ;for a succ state
  (do (bind (load knapsack1 $load))
      (bind (worth knapsack1 $worth))
      (doall (?item item)  ;items previously ordered by value/weight ratio
        (and (not (in ?item knapsack1))
             (bind (weight ?item $weight))
             (bind (value ?item $value))
             (if (< (+ $load $weight) *max-knapsack-weight*)
               (setq $load (+ $load $weight)
                     $worth (+ $worth $value))
               (do (setq $fraction (/ (- *max-knapsack-weight* $load) $weight))
                   (setq $worth (+ $worth (* $fraction $value)))
                   (return-from get-best-relaxed-value? $worth)))))))


(define-action put
    1
  (?knapsack knapsack ?item item)
  (and (not (in ?item ?knapsack))
       (bind (weight ?item $item-weight))
       (bind (load ?knapsack $knapsack-load))
       (setq $new-knapsack-load (+ $knapsack-load $item-weight))
       (<= $new-knapsack-load *max-knapsack-weight*)
       (bind (worth ?knapsack $knapsack-worth))
       (bind (value ?item $item-value))       
       (setq $new-knapsack-worth (+ $knapsack-worth $item-value))
       (setq $objective-value $new-knapsack-worth))
  (?item item ?knapsack knapsack)
  (assert (in ?item ?knapsack)
          (load ?knapsack $new-knapsack-load)
          (worth ?knapsack $new-knapsack-worth)))


(define-init
  `(capacity knapsack1 ,*max-knapsack-weight*)
  (load knapsack1 0)
  (worth knapsack1 0))


(define-init-action init-item-weights&values
    0
  (?item item)
  (setq $item-structure (find ?item *items* :key #'item-name))
  ()
  (assert (weight ?item (item-weight $item-structure))
          (value ?item (item-value $item-structure))))
    

(define-goal  ;can't put any further item into the knapsack
  (not (exists (?knapsack knapsack ?item item)
         (and (not (in ?item ?knapsack))
              (bind (weight ?item $item-weight))
              (bind (load ?knapsack $knapsack-load))
              (<= (+ $knapsack-load $item-weight) *max-knapsack-weight*)
              (bind (worth ?knapsack $knapsack-worth))))))
  