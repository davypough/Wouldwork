;;;; Filename: problem-crossword5-8.lisp

;;; Problem specification for 5x5 crossword, best filling

;;; Generates instantiations dynamically with get-remaining-fields? and
;;; get-remaining-words?
;;; Requires hashtable instead of list for set representation in relations.
;;; Adds trie structure for dictionary lookup.
;;; Adds bounding function.
;;; Ads post-processing fill options with dictionary words


(in-package :ww)


(ww-set *problem* crossword5-8)


(ww-set *tree-or-graph* graph)


(ww-set *solution-type* max-value)  ;maximize number of used words


(ww-set *progress-reporting-interval* 1000)


(defparameter *fields*  ;(field length)
  '((1across 4) (5across 4) (6across 5) (7across 4) (8across 4)
    (1down 5) (2down 5) (3down 5) (4down 3) (6down 3)))


(defparameter *words*
  '(pique squid sound swiss plate
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


;-------------- dictionary -----------------------


(setq *default-pathname-defaults*  ;point to where dictionary data file is stored
   #P"D:\\Users Data\\Dave\\SW Library\\AI\\Planning\\Wouldwork Planner\\")


(defparameter *trie-ht* (make-hash-table))


(defun trie-insert (trie word)
  (let ((node trie))
    (dotimes (i (length word) node)
      (let ((c (char word i)))
        (setf node (or (gethash c node)
                       (setf (gethash c node) (make-hash-table))))))
    ;; set :word key for last node
    (setf (gethash :word node) t)))



(defun encode-dictionary (dictionary-file)
  ;Read in dictionary word strings from a file and store codes in *trie-ht*.
  (with-open-file (infile dictionary-file :direction :input :if-does-not-exist nil)
    (when (not (streamp infile)) (error "File does not exist!"))
    (let ((word-strings (uiop:read-file-lines infile)))
      (iter (for word-string in word-strings)
            (trie-insert *trie-ht* word-string)))))


(encode-dictionary "data-eng_news_2020_1M-words.txt")


(defun trie-search (trie pattern)
  ;Searches a trie ht for the first word that matches a pattern.
  (labels ((search-node (node pattern current-word index)
             (if (= index (length pattern))
                 ;; check if node represents a valid word
                 (when (gethash :word node) current-word)
                 (let ((chr (char pattern index)))
                   (if (eql chr #\?)
                     ;; wildcard character
                     (loop for key being the hash-keys of node using (hash-value child-node)
                        do (when (hash-table-p child-node)
                             (let ((result (search-node child-node pattern
                                                        (concatenate 'string current-word
                                                                     (string key))
                                                        (1+ index))))
                               (when result
                                 (return result)))))
                     ;; regular character
                     (let ((child-node (gethash chr node)))
                       (when child-node
                         (search-node child-node pattern
                                      (concatenate 'string current-word
                                                   (string chr))
                                      (1+ index)))))))))
    ;; start search at root node
    (search-node trie pattern "" 0)))


(defun trie-search-all (trie pattern)
  ;Searches a trie ht for all words that match a pattern.
  (labels ((search-node (node pattern current-word index)
             (if (= index (length pattern))
                 ;; check if node represents a valid word
                 (when (gethash :word node) (list current-word))
                 (let ((chr (char pattern index)))
                   (if (eql chr #\?)
                     ;; wildcard character
                     (loop for key being the hash-keys of node using (hash-value child-node)
                        when (hash-table-p child-node)
                        append (search-node child-node pattern
                                            (concatenate 'string current-word
                                                         (string key))
                                            (1+ index)))
                     ;; regular character
                     (let ((child-node (gethash chr node)))
                       (when child-node
                         (search-node child-node pattern
                                      (concatenate 'string current-word
                                                   (string chr))
                                      (1+ index)))))))))
    ;; start search at root node
    (search-node trie pattern "" 0)))


(defun dictionary-compatible ($new-cross-str)  ;eg, "A?R??" of length 5
  "Tests if a string (with uppercase alphabetic and ? characters)
   is compatible with any dictionary word (symbol) in a hash-table."
  (trie-search *trie-ht* $new-cross-str))


(defun dictionary-compatible-all ($new-cross-str)  ;eg, "A?R??" of length 5
  "For a pattern string (with uppercase alphabetic and ? characters)
   returns all compatible dictionary words (symbols) in a hash-table."
  (trie-search-all *trie-ht* $new-cross-str))


;---------------- types ---------------------


(defparameter *num-fields*
  (length *sorted-fields*))


(defparameter +field-ids+
  (cdr (alexandria:iota (1+ *num-fields*))))


(define-types
  field (compute *sorted-field-names*)
  word (compute *sorted-words*)                        
  field-id (compute +field-ids+))


(define-dynamic-relations
  (used-fields $hash-table)
  (used-words $hash-table)
  (text field $string)
  (used-field-ids-ht $hash-table))  ;change to used-field-ids


(define-static-relations
  (crosscuts field $list)
  (field-id field $fixnum)
  (value field-id $fixnum)
  (weight field-id $fixnum))


(define-query get-remaining-fields? ()
  (do (bind (used-fields $used-fields))
      (ut::ht-set-difference *fields-ht* $used-fields)))


(define-query get-remaining-words? ()
  (do (bind (used-words $used-words))
      (ut::ht-set-difference *words-ht* $used-words)))


;-------------- bounding ----------------------


(define-query compute-bounds? ($used-field-ids)  ;sorted increasing
  ;Computes cost and upper bounds for a state
  (do (setf $capacity *num-fields*)                                              ;(ut::prt $used-field-ids $capacity)
      (setf $max-field-id (or (car (last $used-field-ids)) 0))                   ;(ut::prt $max-field-id)
      (if (= (length $used-field-ids) $max-field-id)
        (setf $missing-field-ids nil)
        (do (setf $initial-field-ids (cdr (alexandria:iota (1+ $max-field-id)))) ;(ut::prt $initial-field-ids)
            (setf $missing-field-ids
                  (nreverse (set-difference $initial-field-ids $used-field-ids)))))
      (setf $all-field-ids +field-ids+)                                           ;(ut::prt $missing-field-ids $all-field-ids)
      (setf $wt 0 $cost 0 $upper 0)
      (using $field-id in $all-field-ids do  ;run thru all field-ids until capacity exceeded
        (if (and (not (member $field-id $missing-field-ids))  ;except those missing
                 (bind (weight $field-id $field-weight))
                 (bind (value $field-id $field-value))                             ;(ut::prt "" $field-id $field-weight $field-value $wt)
                 (if (<= (+ $wt $field-weight) $capacity)
                   (do (incf $wt $field-weight)                                    ;(ut::prt $wt)
                       (incf $cost $field-value)
                       (incf $upper $field-value))
                   (do (setf $fraction (/ (- $capacity $wt) $field-weight))
                       (incf $cost (* $fraction $field-value))                     ;(ut::prt $fraction (- $cost) (- $upper))
                       (return-from compute-bounds? (values (- $cost) (- $upper))))))))  ;(ut::prt (- $cost) (- $upper))
      (return-from compute-bounds? (values (- $cost) (- $upper)))))


(defun successors-p ($used-field-ids)
  "Tests for a succession of integers (ie, field-ids)."
  (iter (for field-id in $used-field-ids)
        (for prev-field-id previous field-id)
        (when prev-field-id
          (always (= field-id (1+ prev-field-id))))))


(defun sort-field-ids ($used-field-ids-ht)
  (when (and $used-field-ids-ht (/= (hash-table-count $used-field-ids-ht) 0))
    (sort (iter (for (field-id *) in-hashtable $used-field-ids-ht)
                (collect field-id at beginning))
          #'<)))


(define-query bounding-function? ()
  (do (bind (used-field-ids-ht $used-field-ids-ht))
      (setf $used-field-ids (sort-field-ids $used-field-ids-ht))  ;(ut::prt $used-field-ids)
      (if (successors-p $used-field-ids)
        (if (= *cost* *upper* 0)
          (do (multiple-value-setq (*cost* *upper*)
                                   (compute-bounds? $used-field-ids))
                 (values *cost* *upper*))
          (values *cost* *upper*))
        (do (setf *cost* 0 *upper* 0)
            (compute-bounds? $used-field-ids)))))


;------------------------- action rule -----------------------


(defun full-word ($word-string)
  (notany (lambda (char)
            (char= char #\?))
          $word-string))


(define-query word-compatible? ($word $field)
   (and (bind (text $field $field-string))
        (setf $word-string (string $word))
        (= (length $word-string) (length $field-string))
        (every (lambda (char1 char2)
                  (or (char= char1 char2)
                      (char= char2 #\?)))
               $word-string $field-string)))


(define-query crosscuts-compatible? ($word $field $used-field-ids-ht)  
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
                          (if (not (gethash $cross-field $used-field-ids-ht))  ;field not already filled in
                            (push (list $cross-field $new-cross-str) $new-cross-fills)))))))))  ;returns true

  
(define-action fill
    1
    (?field (get-remaining-fields?) ?word (get-remaining-words?))
    (let ($new-cross-fills)
      (declare (special $new-cross-fills))
      (and (word-compatible? ?word ?field)
           (bind (used-field-ids-ht $used-field-ids-ht))
           (crosscuts-compatible? ?word ?field $used-field-ids-ht)))
    (?field field ?word word)
    (assert (bind (used-words $used-words))
            (setf $new-used-words (alexandria:copy-hash-table $used-words))
            (bind (used-fields $used-fields))
            (setf $new-used-fields (alexandria:copy-hash-table $used-fields))
            (setf $new-used-field-ids-ht (alexandria:copy-hash-table $used-field-ids-ht))
            (text ?field (string ?word))  ;fills in a word given cross letters compatible
            (setf (gethash ?word $new-used-words) t)
            (setf (gethash ?field $new-used-fields) t)
            (bind (field-id ?field $field-id))
            (setf (gethash $field-id $new-used-field-ids-ht) t)
            (using ($cross-field $new-cross-str) in $new-cross-fills
              do (text $cross-field $new-cross-str)
                 (if (full-word $new-cross-str)
                   (do (setf $new-cross-word (intern $new-cross-str))
                       (if (gethash $new-cross-word *words-ht*)  ;otherwise leave dictionary filling
                         (do (setf (gethash $new-cross-word $new-used-words) t)
                             (setf (gethash $cross-field $new-used-fields) t)
                             (bind (field-id $cross-field $cross-field-id))
                             (setf (gethash $cross-field-id $new-used-field-ids-ht) t))))))
            (used-words $new-used-words)
            (used-fields $new-used-fields)
            (used-field-ids-ht $new-used-field-ids-ht)
            (setq $objective-value (hash-table-count $new-used-fields))))


;------------------ initializations ----------------


(define-init
  `(used-fields ,(make-hash-table :test #'eq))
  `(used-words ,(make-hash-table :test #'eq))
  `(used-field-ids-ht ,(make-hash-table :test #'eql)))


(define-init-action initialize-crosscuts&text
  0
  ()
  (always-true)
  ()
  (assert (using ($field $field-cuts) in *crosscuts*
            do (crosscuts $field $field-cuts))))


(define-init-action initialize-fields
    ;eg, asserts (text 20across "????")
    0
    ()
    (always-true)
    ()
    (assert (using ($field $field-length) in *sorted-fields*
              for $field-id from 1
                do (text $field (make-string $field-length :initial-element #\?))
                   (field-id $field $field-id)
                   (setf $field-length+1 (1+ $field-length))
                   (value $field-id $field-length+1)
                   (weight $field-id $field-length+1))))  ;one added for each cross-field



(define-goal  ;just find the best (max-value) of all states
  nil)


;------------- post-processing ----------------

(defun cull-best-states ()
  (remove-duplicates *best-states* :from-end t :test #'state-equal-p))


(defun corresponding-char-lists (char-lists1 index1 char-lists2 index2)
  ;Returns char-lists in char-list1 that are compatible with char-lists2
  (let ((char-lists2 (mapcar #'(lambda (char-list) (nth index2 char-list)) char-lists2)))
    (remove-if-not #'(lambda (char-list) (member (nth index1 char-list) char-lists2)) char-lists1)))


(define-query collect-matches? ()
  (using $field in *sorted-field-names*
    with $final-matches = nil
    do (bind (crosscuts $field $crosscuts))
       (bind (text $field $text))
       (if (full-word $text)
         (push (list $field (coerce $text 'list)) $final-matches)
         (using $chr across $text
           with $corresponding = (dictionary-compatible-all $text)  ;progressively reduce list of field matches
           for ($cross-field $cross-index $index) on $crosscuts by #'cdddr
             when (char= $chr #\?)
               do (bind (text $cross-field $cross-text))
                  (setf $cross-matches (dictionary-compatible-all $cross-text))
                  (setf $corresponding (corresponding-char-lists $corresponding $index $cross-matches $cross-index))
           finally (push (cons $field $corresponding) $final-matches)))
    finally (return $final-matches)))


(defun fillin (state)  ;finish filling in puzzle with dictionary words
  (let ((matches (sort (collect-matches? state) #'< :key #'length)))
    (mapcar (lambda (match)
              (cons (car match)
                    (iter (for item in (cdr match))
                          (collect (etypecase item
                                     (symbol item)
                                     (list (coerce item 'string)))))))
            matches)))
