;;;; Filename: problem-crossword5-6.lisp

;;; Problem specification for 5x5 crossword, best filling

;;; Generates instantiations dynamically with get-remaining-fields? and
;;; get-remaining-words?
;;; Requires hashtable instead of list for set representation in relations.
;;; Adds trie structure for dictionary lookup.


(in-package :ww)


(ww-set *problem* crossword5-6)


(ww-set *tree-or-graph* graph)


(ww-set *solution-type* max-value)  ;maximize number of used words


(ww-set *progress-reporting-interval* 1000)


(defparameter *fields*  ;(field length)
  '((1across 4) (5across 4) (6across 5) (7across 4) (8across 4)
    (1down 5) (2down 5) (3down 5) (4down 3) (6down 3)))


(defparameter *words*
  '(pique squid sound swiss plate
    suns weds epic psst ;tad iowa 
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


(defparameter *sorted-field-names*
  (iter (for (field nil) in *sorted-fields*)
        (collect field)))


(defparameter *sorted-words*  ;longer words first
  (sort (copy-list *words*) #'> :key (lambda (x)
                                       (length (string x)))))


(defparameter *words-ht*
  (let ((ht (make-hash-table :test #'eq)))
    (iter (for word in *sorted-words*)
          (setf (gethash word ht) t))
    ht))


(defparameter *fields-ht*
  (let ((ht (make-hash-table :test #'eq)))
    (iter (for field in *sorted-field-names*)
          (setf (gethash field ht) t))
    ht))


(setq *default-pathname-defaults*  ;point to where dictionary data file is stored
   #P"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\")


(defparameter *trie-ht* (make-hash-table))


(defun trie-insert (trie word)
  (let ((node trie))
    (dotimes (i (length word) node)
      (let ((c (char word i)))
        (setf node (or (gethash c node)
                       (setf (gethash c node) (make-hash-table))))))))


(defun encode-dictionary (dictionary-file)
  ;Read in dictionary word strings from a file and store codes in *trie-ht*.
  (with-open-file (infile dictionary-file :direction :input :if-does-not-exist nil)
    (when (not (streamp infile)) (error "File does not exist!"))
    (let ((word-strings (uiop:read-file-lines infile)))
      (iter (for word-string in word-strings)
            (trie-insert *trie-ht* word-string)))))


(encode-dictionary "data-eng_news_2020_1M-words.txt")


(defun trie-search (trie pattern)
  ;Searches a trie ht for a pattern list of characters.
  (labels ((search-node (node pattern)
             (if (null pattern)
                 t
                 (let ((chr (car pattern))
                       (rest-pattern (cdr pattern)))
                   (if (eql chr #\?)
                     ;; wildcard character
                     (some #'(lambda (child-node)
                               (search-node child-node rest-pattern))
                           (alexandria:hash-table-values node))
                     ;; regular character
                     (let ((child-node (gethash chr node)))
                       (when child-node
                         (search-node child-node rest-pattern))))))))
    ;; start search at root node
    (search-node trie pattern)))


(define-types
  field (compute *sorted-field-names*)
  word (compute *sorted-words*))                        


(define-dynamic-relations
  (used-fields $hash-table)
  (used-words $hash-table)
  (text field $string))


(define-static-relations
  (crosscuts field $list))


(define-query get-remaining-fields? ()
  (do (bind (used-fields $used-fields))
      (ut::ht-set-difference *fields-ht* $used-fields)))


(define-query get-remaining-words? ()
  (do (bind (used-words $used-words))
      (ut::ht-set-difference *words-ht* $used-words)))


(defun full-word ($word-string)
  (notany (lambda (char)
            (char= char #\?))
          $word-string))


(defun dictionary-compatible ($new-cross-str)  ;eg, "A?R??" of length 5
  "Tests if a string (with uppercase alphabetic and ? characters)
   is compatible with the dictionary words (symbols) in a hash-table."
  (trie-search *trie-ht* (coerce $new-cross-str 'list)))


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
                        (if (dictionary-compatible $new-cross-str)
                          (if (not (gethash $cross-field $used-fields))  ;field not already filled in
                            (push (list $cross-field $new-cross-str) $new-cross-fills)))))))))  ;returns true

  
(define-action fill
    1
    (?field (get-remaining-fields?) ?word (get-remaining-words?))
    (let ($new-cross-fills)
      (declare (special $new-cross-fills))
      (and (word-compatible? ?word ?field)
           (bind (used-words $used-words))
           (crosscuts-compatible? ?word ?field $used-words)))
    (?field field ?word word)
    (assert (setf $new-used-words (alexandria:copy-hash-table $used-words))
            (bind (used-fields $used-fields))
            (setf $new-used-fields (alexandria:copy-hash-table $used-fields))
            (text ?field (string ?word))  ;fills in a word given cross letters compatible
            (setf (gethash ?word $new-used-words) t)
            (setf (gethash ?field $new-used-fields) t)
            (using ($cross-field $new-cross-str) in $new-cross-fills
              do (text $cross-field $new-cross-str)
                 (if (full-word $new-cross-str)
                   (do (setf $new-cross-word (intern $new-cross-str))
                       (if (gethash $new-cross-word *words-ht*)  ;otherwise leave dictionary filling
                         (do (setf (gethash $new-cross-word $new-used-words) t)
                             (setf (gethash $cross-field $new-used-fields) t))))))
            (used-words $new-used-words)
            (used-fields $new-used-fields)
            (setq $objective-value (hash-table-count $new-used-fields))))


(define-init
  `(used-fields ,(make-hash-table :test #'eq))
  `(used-words ,(make-hash-table :test #'eq)))


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
