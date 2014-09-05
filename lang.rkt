#lang plai-typed

(define-type ArithC
  (numC  (n : number))
  (plusC (l : ArithC) (r : ArithC))
  (multC (l : ArithC) (r : ArithC)))


(define ten  (numC 10))
(define five (numC 5))


(define (interp (inp : ArithC)) : number
  (type-case ArithC inp
    (numC  (n)   n)
    (plusC (l r) (+ (interp l) (interp r)))
    (multC (l r) (* (interp l) (interp r)))))

(define fifty (multC ten five))

(define (parse (s : s-expression)) : ArithC
  (cond ((s-exp-number? s) (numC (s-exp->number s)))
        ((s-exp-list? s)
         (let ((s1 (s-exp->list s)))
           (case (s-exp->symbol (first s1))
             ((+) (plusC (parse (second s1)) (parse (third s1))))
             ((*) (multC (parse (second s1)) (parse (third s1))))
             ((-) (plusC (parse (second s1))
                         (multC (numC -1)
                                (parse (third s1)))))
             (else (error 'parse "invalid syntax")))))
        (else (error 'parse "invalid syntax"))))
