#lang plai-typed

(require "type-inference.rkt")

(define (strcomb (l : (listof string))) : string
  (foldr string-append "" l))

(define (pretty (t : Type)) : string
  (type-case Type t
    (numT () "num")
    (boolT () "bool")
    (tlistT (e) (strcomb (list "[" (pretty e) "]")))
    (funT (a t) (strcomb (list "(" (pretty a) " -> " (pretty t) ")"))) 
    (varT (n)   (symbol->string n))))


(test (pretty (infer-type '5)) "num")
(test (pretty (infer-type '(+ 5 7))) "num")
(test (pretty (infer-type '(* 5 7))) "num")
(test (pretty (infer-type '(- 5 7))) "num")
(test (pretty (infer-type '(iszero 5))) "bool")

(test (pretty (infer-type '(tempty? tempty))) "bool")
(test (pretty (infer-type '(tcons 5 tempty))) "[num]")

(test (pretty (infer-type '(bif (iszeroS 5) 5 7))) "num")

(test (pretty (infer-type '(tcons 6 (tcons 5 tempty)))) "[num]")

(test (pretty (infer-type '(lambda (x) (tcons x tempty)))) "(a -> [a])")

(test (pretty (infer-type '(with ((x (tcons 6 (tcons 5 tempty)))) (tfirst x)))) "num")

(test (pretty (infer-type '(with ((x (tcons 6 (tcons 5 tempty)))) (trest x)))) "[num]")

(test (pretty (infer-type '(lambda (x) (lambda (y) (+ y x))))) "(num -> (num -> num))")

(test (pretty (infer-type '(lambda (x) (+ x 1)))) "(num -> num)")

(test (pretty (infer-type '(with ((x (lambda (x) (tcons x tempty)))) (x 3)))) "[num]")

(test (pretty (infer-type '(with ((id (lambda (x) x))) (id 5)))) "num")

(test (pretty (infer-type '(with ((id (lambda (x) x))) (id tempty)))) "[a]")

(test (pretty (infer-type '(with ((id (lambda (x) x))) (id (tcons 5 tempty))))) "[num]")

(test (pretty (infer-type '((lambda (x) (lambda (x) x)) 5))) "(a -> a)")

(test (pretty (infer-type '((lambda (x) (lambda (x) (lambda (x) x))) 5))) "(a -> (b -> b))")

(test (pretty (infer-type '(with ((f (lambda (x) x))) (f f)))) "x -> x")
