;;;; Filename: problem-crossword5-1.lisp

;;; Problem specification for 5x5 crossword, best filling

;;; This is the first (simplest) version of crossword5. It generates parameters by
;;; straightforward field and word types, leading to F*W total instantiations
;;; of all (?field ?word) pairs. Uses
;;; hash-table checking for partial word compatibility with dictionary.

;;; Optimize dictionary-compatible. Produces no significant improvement.


(in-package :ww)


(ww-set *problem* crossword5-1)


(ww-set *tree-or-graph* graph)


(ww-set *solution-type* max-value)  ;maximize number of used words


(ww-set *progress-reporting-interval* 1000)


(defparameter *fields*  ;(field length)
  '((1across 4) (5across 4) (6across 5) (7across 4) (8across 4)
    (1down 5) (2down 5) (3down 5) (4down 3) (6down 3)))


(defparameter *words* '(pique squid sound swiss plate  ;ordered by length
                        suns weds epic psst ;iowa tad
                        ssw was))


(defparameter *crosscuts*  ;intersecting fields
  '((1across (1down 0 0 2down 0 1 3down 0 2 4down 0 3))
    (5across (1down 1 0 2down 1 1 3down 1 2 4down 1 3))
    (6across (6down 0 0 1down 2 1 2down 2 2 3down 2 3 4down 2 4))
    (7across (6down 1 0 1down 3 1 2down 3 2 3down 3 3))
    (8across (6down 2 0 1down 4 1 2down 4 2 3down 4 3))
    (1down (1across 0 0 5across 0 1 6across 1 2 7across 1 3 8across 1 4))
    (2down (1across 1 0 5across 1 1 6across 2 2 7across 2 3 8across 2 4))
    (3down (1across 2 0 5across 2 1 6across 3 2 7across 3 3 8across 3 4))
    (4down (1across 3 0 5across 3 1 6across 4 2))
    (6down (6across 0 0 7across 0 1 8across 0 2))))


(defparameter *sorted-fields*  ;longer fields first
  (sort (copy-list *fields*) #'> :key #'second))


(defparameter *sorted-words*  ;longer words first
  (sort (copy-list *words*) #'> :key (lambda (x)
                                       (length (string x)))))


(defparameter *lci-dictionary* (make-hash-table :test #'equal :size 10000))  
  "Hash-table key = length|char|index concatenated together (eg, 5A0),
   hash-table values = list of matching symbols (eg, (ABATE ABBEY ...))."


(setq *default-pathname-defaults*  ;point to where dictionary data file is stored
   #P"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\")


(defun encode-dictionary (dictionary-file)
  ;Read in dictionary word strings from a file and store codes in lci-dictionary.
  (with-open-file (infile dictionary-file :direction :input :if-does-not-exist nil)
    (when (not (streamp infile)) (error "File does not exist!"))
    (let ((word-strings (uiop:read-file-lines infile)))  ;stream)))
      (iter (for word-string in word-strings)
            (iter (for char in-sequence word-string)
                  (for index from 0)
                  (push (intern word-string)
                        (gethash (format nil "~D~C~D" (length word-string) char index)
                                 *lci-dictionary*)))))))
                             
       
(encode-dictionary "English words (455K).txt")  ;length|letter|index encoding of the dictionary


(define-types
    field (compute (iter (for (field nil) in *sorted-fields*)
                         (collect field)))
    word (compute (iter (for word in *words*)
                        (collect word))))                        


(define-dynamic-relations
    (used-fields $list)
    (used-words $list)
    (text field $string))


(define-static-relations
    (crosscuts field $list))


(defun full-word ($word-string)
  (notany (lambda (char)
            (char= char #\?))
          $word-string))


(declaim (ftype (function (simple-base-string) list) dictionary-compatible))

(defun dictionary-compatible ($new-cross-str)  ;eg, "A?R??" of length 5
  "Tests if a string (with uppercase alphabetic and ? characters)
   is compatible with the dictionary words (symbols) in a hash-table."
  (declare (optimize (speed 3)))
  (iter (with len = (length $new-cross-str))
        (declare (fixnum len))
        (for chr in-vector $new-cross-str)
        (declare ((or null base-char) chr))
        (for index from 0)
        (declare (fixnum index))
        (when (char/= chr #\?)
          (collect (gethash (the simple-base-string (format nil "~D~C~D" len chr index))
                            *lci-dictionary*)
                   into dict-words at beginning))
        (finally (return (reduce #'intersection dict-words)))))


(define-query word-compatible? ($word $field)
   (and (bind (text $field $field-string))
        (setf $word-string (string $word))
        (= (length $word-string) (length $field-string))
        (every (lambda (char1 char2)
                  (or (char= char1 char2)
                      (char= char2 #\?)))
               $word-string $field-string)))


(define-query crosscuts-compatible? ($word $field $used-fields)  
  (let ()
    (declare (special $new-cross-fills))
    (and (bind (crosscuts $field $crosscuts))  ;(ut::prt $word $field)
         (using ($cross-field $cross-index $word-index) on $crosscuts by #'cdddr
           always (do (bind (text $cross-field $cross-str))
                      (setf $new-cross-str  ;replace one letter from word-string into cross-str
                            (replace (copy-seq $cross-str) (string $word) 
                                     :start1 $cross-index :start2 $word-index :end2 (1+ $word-index)))  ;(ut::prt $cross-str $new-cross-str)
                      (if (string= $new-cross-str $cross-str)  ;no change?
                        t  ;returns true
                        (if (dictionary-compatible (coerce $new-cross-str 'simple-base-string))
                          (if (not (member $cross-field $used-fields))  ;field not already filled in
                            (push (list $cross-field $new-cross-str) $new-cross-fills)))))))))  ;returns true

  
(define-action fill
    1
    (?field field ?word word)
    (let ($new-cross-fills)
      (declare (special $new-cross-fills))
      (and (bind (used-fields $used-fields))
           (not (member ?field $used-fields))
           (bind (used-words $used-words))
           (not (member ?word $used-words))
           (word-compatible? ?word ?field)
           (crosscuts-compatible? ?word ?field $used-words)))
    (?field field ?word word)
    (assert (setf $new-used-words (copy-list $used-words))
            (setf $new-used-fields (copy-list $used-fields))
            (text ?field (string ?word))  ;fills in a word given cross letters compatible
            (push ?word $new-used-words)
            (push ?field $new-used-fields)
            (using ($cross-field $new-cross-str) in $new-cross-fills
              do (text $cross-field $new-cross-str)
                 (if (full-word $new-cross-str)
                   (do (setf $new-cross-word (intern $new-cross-str))
                       (if (member $new-cross-word *words*)  ;otherwise leave dictionary filling
                         (do (push $new-cross-word $new-used-words)
                             (push $cross-field $new-used-fields))))))
            (used-words $new-used-words)
            (used-fields $new-used-fields)
            (setq $objective-value (length $new-used-fields))))


(define-init
  (used-fields nil)
  (used-words nil))


(define-init-action initialize-crosscuts
  0
  ()
  (always-true)
  ()
  (assert (using ($field $field-cuts) in *crosscuts*
            do (crosscuts $field $field-cuts))
          (using ($field $length) in *fields*
            do (text $field (make-string $length :initial-element #\?)))))


(define-goal  ;just find the best (max-value) of all states
  nil)
