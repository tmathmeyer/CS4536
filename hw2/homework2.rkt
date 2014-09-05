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
  (appS (f : ExprS) (arg : ExprS))
  (if0S (c : ExprS) (t : ExprS) (e : ExprS))
  (lamS (arg : symbol) (body : ExprS))
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
                ((lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl))))
                ((with) (withS (map (lambda (b) 
                                      (let ((bl (s-exp->list b)))
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl))))
                (else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (parse (second sl))))))
             ((s-exp-list? (first sl)) ;; app with complex expression in function position
              (appS (parse (first sl))
                    (parse (second sl))))
             (else (error 'parse "expected symbol or list after parenthesis")))))
    (else (error 'parse "unexpected input format"))))
     
;; abstract syntax and desugar
     
(define-type ExprC
  (numC (n : number))
  (plusC (l : ExprC) (r : ExprC))
  (multC (l : ExprC) (r : ExprC))
  (idC (i : symbol))
  (appC (f : ExprC) (arg : ExprC))
  (if0C (c : ExprC) (t : ExprC) (e : ExprC))
  (lamC (arg : symbol) (body : ExprC))
  )

;; output values

(define-type Value
  (numV (n : number))
  (closV (arg : symbol) (body : ExprC) (env : Env)))

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

(define (interp x y)
  0)

(define (desugar x)
  0)






;; basic numbers
(test (run '5) 5)

;; basic arithmetic
(test (run '(+ 5 5)) 10)
(test (run '(- 5 5)) 0)
(test (run '(* 5 5)) 25)

;; conditionals
(test (run '(if0 0 0 0)) 0) ;; does ANYTHING
(test (run '(if0 0 1 0)) 1)
(test (run '(if0 1 0 1)) 1)
(test (run '(if0 -1 0 1)) 1)

;; local binding
(test (run '(with ((x 5)) x)) 5)
(test (run '(with ((x 5) (y x)) (* y y))))
(test (run '(with ((x 5) (y x) (x y)) (* x y))) 25)


