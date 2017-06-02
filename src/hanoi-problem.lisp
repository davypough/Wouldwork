;;;; Filename: hanoi-problem.lisp

;;; Problem specification for the tower of hanoi.


(in-package :ww)  ;required


(setq *depth-cutoff* 15)


(define-types
  base    (base1 base2 base3)  ;the 3 peg bases on which disks will be placed
  disk    (disk1 disk2 disk3)
  support (either base disk))


(define-base-relations
  (on disk support)
  (size ?support !fixnum))


(define-monitored-relations
  on)


(define-derived-relations
  (cleartop> ?disk)        (not (exists (?d disk)
                                  (on ?d ?disk)))
)


(define-action move
    1
  (?disk disk (?support1 ?support2) support ($d $s) fluent)
  (and (cleartop> ?disk)
       (size ?disk $d)
       (on ?disk ?support1)
       (cleartop> ?support2)
       (size ?support2 $s)
       (< $d $s))
  (?disk disk (?support1 ?support2) support)
  (and (not (on ?disk ?support1))
       (on ?disk ?support2)))


(define-init
    ;dynamic
  (on disk3 base1)
  (on disk2 disk3)
  (on disk1 disk2)    
  ;static
  (size disk1 1)
  (size disk2 2)
  (size disk3 3)
  (size base1 10)
  (size base2 10)
  (size base3 10)
)


(define-goal
  (and (on disk3 base3)
       (on disk2 disk3)
       (on disk1 disk2)))
