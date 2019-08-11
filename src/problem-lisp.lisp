;;;; Filename: problem-lisp.lisp


;;; Problem specification for experiments with generating lisp expressions


(in-package :ww)  ;required

(ww-set 'problem 'lisp)

(ww-set 'solution-type 'min-length)

(ww-set 'tree-or-graph 'tree)

(ww-set 'depth-cutoff 2)


(define-types
  expression (nil 1)
  selection (1 2 3))


(define-dynamic-relations
    (code $list))


(define-query cons-products? (lst1 lst2)
  (let (prods)
    (alexandria:map-product (lambda (x1 x2)
                              (push (list 'cons x1 x2) prods))
                            lst1 lst2)
    (alexandria:map-product (lambda (x1 x2)
                              (push (list 'cons x1 x2) prods))
                            lst2 lst1)
    (alexandria:map-product (lambda (x1 x2)
                              (push (list 'cons x1 x2) prods))
                            lst2 lst2)
    prods))
                            

(define-action cons
    1
  (?selection selection ?expression expression)
  (always-true)
  ()
  (if (bind (code $code))
    (case ?selection
      (1 (assert (code `(cons ,$code nil))))
      (2 (assert (code `(cons nil ,$code))))
      (3 (assert (code `(cons ,$code ,$code)))))
    (case ?selection
      (1 (assert (code `(cons ,?expression nil))))
      (2 (assert (code `(cons nil ,?expression))))
      (3 (assert (code `(cons ,?expression ,?expression)))))))


(define-goal
  (and (bind (code $code))
       (equal (eval $code) '((1) NIL))))