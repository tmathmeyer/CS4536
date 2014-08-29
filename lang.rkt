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

(define (parse (inp : s-expression)) : ArithC
  