#lang plai-typed

(require "type-inference.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     HELPER FUNCTIONS TO MAKE TESTING REALLY EASY     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all the pretty variables
(define PRETTY (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))

;; a binding from an ugly variable to a clean one
;; except that plastic surgery generally doesn't
;; really make people look any better
(define-type PlasticSurgery
  (bind (from : symbol)
        (to   : symbol)))

;; combine strings together
(define (strcomb (l : (listof string))) : string
  (foldr string-append "" l))

;; make a type pretty (as opposed to the hideous to-string)
(define (pretty (t : Type)) : string
  (type-case Type t
    (numT () "num")
    (boolT () "bool")
    (tlistT (e) (strcomb (list "[" (pretty e) "]")))
    (funT (a t) (strcomb (list "(" (pretty a) " -> " (pretty t) ")"))) 
    (varT (n)   (symbol->string n))))

;; character length of a symbol
(define (symlen (s : symbol)) : number
  (string-length (symbol->string s)))

;; get the "ugly" vars
(define (get-ugly-vars (t : Type)) : (listof symbol)
  (type-case Type t
    (tlistT (e) (get-ugly-vars e))
    (funT (a t) (append (get-ugly-vars a) (get-ugly-vars t)))
    (varT (n)   (cond ((> (symlen n) 1) (list n))))
    (else empty)))

;; remove duplicates from a list
(define (remove-dups from)
  (cond ((empty? from) empty)
        ((cons? from) (let ((f (first from)) (r (rest from)))
                        (let ((r (filter (λ (x) (not (equal? x f))) r)))
                          (cons f (remove-dups r)))))))

;; pair up symbols for replacement purposes
(define (match-sym (bad : (listof symbol)) (good : (listof symbol))) : (listof PlasticSurgery)
  (cond ((empty? bad) empty)
        ((cons? bad)  (cons (bind (first bad) (first good))
                            (match-sym (rest bad) (rest good))))))

;; rebind all the ugly variables to their proper clean ones
(define (rebind-ugly (t : Type) (with : (listof PlasticSurgery))) : Type
  (type-case Type t
    (numT ()  (numT))
    (boolT () (boolT))
    (tlistT (e) (tlistT (rebind-ugly e with)))
    (funT (a t) (funT (rebind-ugly a with) (rebind-ugly t with)))
    (varT (n)   (varT (searchfor n with)))))

;; get a symbol from a list of plastic surgeries
(define (searchfor (s : symbol) (map : (listof PlasticSurgery))) : symbol
  (let ((match (filter (λ (each) (symbol=? s (bind-from each))) map)))
    (cond ((empty? match) s)
          ((cons? match) (bind-to (first match))))))

;; make a type beautiful, ie, remove all the ugly variables from it
(define (beautify-type (t : Type)) : Type
  (rebind-ugly t (match-sym (remove-dups (get-ugly-vars t)) PRETTY)))

;; call all the helpers on infer
(define (infer x)
  (pretty (beautify-type (infer-type x))))











;;;;;;;;;;;;;;;;;;;
;;     TESTS     ;;
;;;;;;;;;;;;;;;;;;;


(test (infer '5) "num")
(test (infer '(+ 5 7)) "num")
(test (infer '(* 5 7)) "num")
(test (infer '(- 5 7)) "num")
(test (infer '(iszero 5)) "bool")

(test (infer '(tempty? tempty)) "bool")
(test (infer '(tcons 5 tempty)) "[num]")

(test (infer '(bif (iszeroS 5) 5 7)) "num")

(test (infer '(tcons 6 (tcons 5 tempty))) "[num]")

(test (infer '(lambda (x) (tcons x tempty))) "(a -> [a])")

(test (infer '(with ((x (tcons 6 (tcons 5 tempty)))) (tfirst x))) "num")

(test (infer '(with ((x (tcons 6 (tcons 5 tempty)))) (trest x))) "[num]")

(test (infer '(lambda (x) (lambda (y) (+ y x)))) "(num -> (num -> num))")

(test (infer '(lambda (x) (+ x 1))) "(num -> num)")

(test (infer '(with ((x (lambda (x) (tcons x tempty)))) (x 3))) "[num]")

(test (infer '(with ((id (lambda (x) x))) (id 5))) "num")

(test (infer '(with ((id (lambda (x) x))) (id tempty))) "[a]")

(test (infer '(with ((id (lambda (x) x))) (id (tcons 5 tempty)))) "[num]")

(test (infer '((lambda (x) (lambda (x) x)) 5)) "(a -> a)")

(test (infer '((lambda (x) (lambda (x) (lambda (x) x))) 5)) "(a -> (b -> b))")

(test (infer '(rec ((f (lambda (x) (bif (iszero x) 0 (f (- x 1)))))) (f 4))) "num")

(test/exn (infer '(with ((f (lambda (x) x))) (f f))) "occurs check")

(test/exn (infer '(tcons 5 4)) "type error")

(test/exn (infer '(tcons 5 (tcons (iszero 5) tempty))) "type error")

(test/exn (infer '(with ((x (tcons (lambda (x) ((tfirst x) x)) tempty))) ((tfirst x) x))) "occurs check")

