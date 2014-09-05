#lang plai-typed

;; starter file for the extended basic interpreter

;; surface syntax and parser 

; type used to capture a with-binding
(define-type DefS
  (defS (name : symbol) (val : ExprS)))

(define-type ExprS
  (numS (n : number))
  (plusS (l : ExprS) (r : ExprS))
  (bminusS (l : ExprS) (r : ExprS))
  (multS (l : ExprS) (r : ExprS))
  (idS (i : symbol))
  (appS (f : ExprS) (arg : (listof ExprS)))
  (if0S (c : ExprS) (t : ExprS) (e : ExprS))
  (lamS (arg : (listof symbol)) (body : ExprS))
  (withS (bindings : (listof DefS)) (body : ExprS))
  )

; parses s-expressions into surface syntax
(define (parse (s : s-expression)) : ExprS
  (cond
    ((s-exp-number? s) (numS (s-exp->number s)))
    ((s-exp-symbol? s) (idS (s-exp->symbol s)))
    ((s-exp-list? s)
     (let ((sl (s-exp->list s)))
       (cond ((s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                ((+) (plusS (parse (second sl)) (parse (third sl))))
                ((*) (multS (parse (second sl)) (parse (third sl))))
                ((-) (bminusS (parse (second sl)) (parse (third sl))))
                ((if0) (if0S (parse (second sl)) (parse (third sl)) (parse (fourth sl))))
                ((lambda) (lamS (map s-exp->symbol (s-exp->list (second sl))) (parse (third sl))))
                ((with) (withS (map (lambda (b) 
                                      (let ((bl (s-exp->list b)))
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl))))
                (else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (map parse (rest sl))))))
             ((s-exp-list? (first sl)) ;; app with complex expression in function position
              (appS (parse (first sl))
                    (map parse (rest sl))))
             (else (error 'parse "expected symbol or list after parenthesis")))))
    (else (error 'parse "unexpected input format"))))
     
;; abstract syntax and desugar
     
(define-type ExprC
  (numC (n : number))
  (plusC (l : ExprC) (r : ExprC))
  (multC (l : ExprC) (r : ExprC))
  (idC (i : symbol))
  (appC (f : ExprC) (arg : (listof ExprC)))
  (if0C (c : ExprC) (t : ExprC) (e : ExprC))
  (lamC (arg : (listof symbol)) (body : ExprC))
  )

;; output values

(define-type Value
  (numV (n : number))
  (closV (args : (listof symbol)) (body : ExprC) (env : Env)))

;; Environments

;; binding an identifier to a value
(define-type Binding
  (bind (name : symbol) (val : Value)))
 
(define-type-alias Env (listof Binding))
(define mt-env empty)

;; API for running programs

; evaluates a program starting with a pre-populated environment
; (this can be helpful in testing)
(define (run/env sexp env)
  (interp (desugar (parse sexp)) env))

; evaluates a program in the empty environment
(define (run sexp)
  (run/env sexp mt-env))

(define (lookup (id : symbol) (context : Env)) : Value
  (cond ((empty? context) (error 'fail "undefined variable"))
        ((symbol=? (bind-name (first context)) id) (bind-val (first context)))
        (else (lookup id (rest context)))))

(define (operate-on-numeric-values (op : (number number -> number)) (init : number) (values : (listof Value))) : number
  (foldl (λ ((x : Value) (y : number)) (type-case Value x
                    (numV (n) (op y n))
                    (closV (a b e) (error 'fail "shit you cant do that")))) init values))

(define (numV=? (a : number) (b : Value))
  (= a (numV-n b)))


(define (interp (e : ExprC) (c : Env)) : Value
  (type-case ExprC e
    (if0C (con tr fa) (cond ((numV=? 0 (interp con c)) (interp tr c)) (else (interp fa c))))
    (plusC (l r) (numV (operate-on-numeric-values + 0 (list (interp l c) (interp r c)))))
    (multC (l r) (numV (operate-on-numeric-values * 1 (list (interp l c) (interp r c)))))
    (lamC (a b) (closV a b c))
    (idC (id) (lookup id c))
    (numC (n) (numV n))
    (appC (func args) (let ((clos (interp func c)))
                        (interp (closV-body clos)
                                (append (closV-env clos)
                                        (map2 (λ (name expr) (bind name (interp expr c))) (closV-args clos) args)))))))
    
    
         

(define (desugar (x : ExprS)) : ExprC
  (type-case ExprS x
    (numS (n) (numC n))
    (plusS (l r) (plusC (desugar l) (desugar r)))
    (bminusS (l r) (plusC (multC (numC -1) (desugar r)) (desugar l)))
    (multS (l r) (multC (desugar l) (desugar r)))
    (idS (i) (idC i))
    (appS (f args) (appC (desugar f) (map desugar args)))
    (if0S (c t e) (if0C (desugar c) (desugar t) (desugar e)))
    (lamS (args body) (lamC args (desugar body)))
    (withS (bin bod) (appC (lamC (map defS-name bin) (desugar bod)) (map (λ (x) (desugar (defS-val x))) bin)))))


(define PARSED (parse '(with ((x 5)) x)))
(define PARSDE (parse '(with ((f (lambda (x) (+ 1 x)))
                              (g (lambda (x) (+ x x)))
                              (m (lambda (x) (* x x)))
                              (n (lambda (x) (* x 5)))
                              (combo (lambda (a b c d) (* (+ (f a) (g b)) (+ (m c) (n d))))))
                             (combo 1 2 3 4))))



;; basic numbers
(test (run '5) (numV 5))

;; basic arithmetic
(test (run '(+ 5 5)) (numV 10))
(test (run '(- 5 5)) (numV 0))
(test (run '(* 5 5)) (numV 25))

;; conditionals
(test (run '(if0 0 0 0)) (numV 0)) ;; does ANYTHING
(test (run '(if0 0 1 0)) (numV 1))
(test (run '(if0 1 0 1)) (numV 1))
(test (run '(if0 -1 0 1)) (numV 1))

;; local binding
(test (run '(with ((x 5)) x)) (numV 5))
(test (run '(with ((x 5)) (with ((y x)) (* y x)))) (numV 25))
(test (run '(with ((x 5)) (with ((y x)) (with ((x y)) (* x x))))) (numV 25))

;; local identifiers
(test (run/env '(+ x x) (cons (bind 'x (numV 5)) empty)) (numV 10))

;; functions
(test (run '(with ((double (lambda (x) (+ x x)))) (double 5))) (numV 10))

;(test (run '(with ((double (lambda (x) (+ x x)))
;                   (square (lambda (x) (* x x)))
;                   (recurs (lambda (x) (if0 x 0 (+ 1 (recurs (- x 1)))))))
;                  (recurs 5))) (numV 5))

(parse '(with ((recurs (lambda (x) (if0 x 0 (+ 1 (recurs (- x 1)))))))
              (recurs 5)))

