#lang plai-typed

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)]))

;; the types in the interpreter

(define-type Value
 [numV (n : number)]
 [boxV (l : Location)]
 [closV (arg : symbol) (body : ExprC) (env : Env)])

; environment mapping to locations

(define-type Binding
 [bind (name : symbol) (loc : Location)])
    (define-type-alias Env (listof Binding))
(define-type-alias Location number)

    ; ExprC (captures single arg functions, but input language 
            ;   still supports multiple args, as shown in the grammar 
            ;   on the assignment page.  Recommend that you avoid writing
            ;   tests that result in closV with lambdas or apps in the body
            ;   for the first part of the assignment (having internal closV
                ;   in your tested expressions is just fine.))

    (define-type ExprC
     [numC (n : number)]
     [plusC (l : ExprC) (r : ExprC)]
     [multC (l : ExprC) (r : ExprC)]
     [idC (i : symbol)]
     [appC (f : ExprC) (arg : ExprC)]
     [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
     [lamC (arg : symbol) (body : ExprC)]
     [boxC (arg : ExprC)]
     [unboxC (arg : ExprC)]
     [setboxC (b : ExprC) (v : ExprC)]
     [seqC (b1 : ExprC) (b2 : ExprC)]
     [setC (var : symbol) (arg : ExprC)]
    )

    ;; a stub of a run function

    (define (run [sexp : s-expression]) : Value
     (call-with-limits 
      10 #f
      (lambda () (numV 0))))

                            ;; insert your tests here to check.  For example:

                            (test (run '(with ((x 3)) x))
                             (numV 3))

;; test arithmetic
(test (run '(+ 3 3)) (numV 6))
(test (run '(* 3 3)) (numV 9))
(test/exn (run '(+ 3 (lambda () 3))) "type")
(test/exn (run '(+ 3)) "expected")

;; test conditionals
(test (run '(if0 0 0 1)) (numV 0))
(test (run '(if0 1 0 1)) (numV 1))
(test/exn (run '(if0 0)) "expected")
(test/exn (run '(if0 (lambda () 3) 0 0)) "type")

;; test withs
(test (run '(with ((x 3)) x)) (numV 3))
(test (run '(with ((x 3) (y 4)) (+ x y))) (numV 7))
(test/exn (run '(with ((x 3) (x 4)) (+ x x))) "multiple")
(test (run '(with ((x 3)) (with ((y x)) (+ x y)))) (numV 6))
(test/exn (run '(with ((x 3) (y x)) x)) "unbound")
(test/exn (run '(with (4) 5)) "type")

;; test boxy things
(test (run '(box 5)) (boxV 0))
(test (run '(seq (box 5) (box 6))) (boxV 2))
(test (run '(unbox (box 4))) (numV 4))
(test (run '(with ((boxxy (box 4))) (unbox boxxy))) (numV 4))
(test (run '(with ((count (box 0)))
                  (lambda () (with ((x (unbox count)))
                                   (seq (setbox count (+ x 1)) x)))
(test (run '((with ((count (box 0))) (lambda () (with ((x (unbox count))) (seq (setbox count (+ x 1)) x)))))
(test (run '(
(test (run '(
(test (run '(
(test (run '(
(test (run '(
(test (run '(
















