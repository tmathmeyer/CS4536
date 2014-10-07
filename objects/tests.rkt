#lang plai-typed

(require "objects.rkt")

;; basic object
;; basid objects
(test (run/classes '(with ((one (new Parent 0)))
                          (send one add 10))
                    (list '(class Parent (var)
                            (parent Object)
                            (private)
                            (public)
                            (add (lambda (x) (+ x var))))))
      (numV 10))

;; basic example of inheritance
;; basid inheritance
(test (run/classes '(with ((child (new Child 5)))
                          (+ (send child add 10)
                             (send child add 5)))
                    (list '(class Parent (var)
                            (parent Object)
                            (private)
                            (public)
                            (add (lambda (x) (+ x var))))
                          '(class Child (mar)
                            (parent Parent (+ mar mar))
                            (private)
                            (public)
                            (id (lambda (p) p)))))
       (numV 35))

;; lists
;; THis was going to be map+filter+fold, but if0 cannot take a non-numV arg
;; I thought it was a shame to delete it though
(test (run/classes '(with ((a 0))
                      (with ((b (new List 5 a)))
                        (with ((c (new List 6 b)))
                          (with ((d (new List 7 c)))
                            (+ (+ (send d first)
                                  (send (send d rest) first))
                               (send (send (send d rest) rest) first))))))
                    (list '(class List (e r)
                            (parent Object)
                            (private)
                            (public (next r) (elem e))
                            (first (lambda () e))
                            (rest  (lambda () r)))))
      (numV 18))


;; setting in parents
;; test that setting works
(test (run/classes '(with ((A (new SetTest 4)))
                      (seq (send A set-m 5) (send A get-m)))
                    (list '(class SetTest (val)
                            (parent Object)
                            (private)
                            (public (m 10)))))
     (numV 5))



;; setting in parents
;; test that setting a value in a parent holds for the child
(test (run/classes '(with ((A (new SetChild 4)))
                      (seq (send A set-m 5) (send A get-m)))
                    (list '(class SetTest (val)
                            (parent Object)
                            (private)
                            (public (m 10)))
                          '(class SetChild (val)
                            (parent SetTest val)
                            (private)
                            (public))))
     (numV 5))

;; make sure that privates are private
;; tes that there is no way to extract privates with lambdas
(test/exn (run/classes '(with ((A (new Execer)))
                          (send A exec (lambda () (+ a b))))
                        (list '(class Execer ()
                                (parent Object)
                                (private (a 1) (b 2))
                                (public)
                                (exec (lambda (x) (x))))))
  "unbound")

;; make sure that privates are private
;; test tht children cannot access parent privates
(test/exn (run/classes '(with ((A (new C)))
                          (send A p))
                        (list '(class P ()
                                (parent Object)
                                (private (a 1)) (public)
                              '(class C ()
                                (parent P)
                                (private) (public)
                                (p (lambda () a))))))
  "unbound")


;; test method overloading
;; when a child overloads a parent method, the child method is called
(test (run/classes '(with ((A (new Test)))
                      (send A overload 3 4))
                    (list '(class Parent ()
                            (parent Object)
                            (private)
                            (public)
                            (overload (lambda (x y) x)))
                          '(class Test ()
                            (parent Parent)
                            (private)
                            (public)
                            (overload (lambda (x y) y)))))
  (numV 4))


;; strategy-factory pattern
;; because why not?
(define factory '(class Factory ()
                  (parent Object)
                  (private) (public)
                  (make-adder (lambda (x y) (new Adder x y)))
                  (make-subtr (lambda (x y) (new Subbr x y)))
                  (make-multr (lambda (x y) (new Multr x y)))))
(define strat '(class Strat ()
                (parent Object)
                (private) (public)
                (do (lambda () 0))))

(define Adder '(class Adder (x y)
                (parent Strat)
                (private) (public)
                (do (lambda () (+ x y)))))

(define Subbr '(class Subbr (x y)
                (parent Strat)
                (private) (public)
                (do (lambda () (- x y)))))

(define Multr '(class Multr (x y)
                (parent Strat)
                (private) (public)
                (do (lambda () (* x y)))))

(test (run/classes '(with ((fact (new Factory)))
                      (+ (+ (send (send fact make-adder 4 2) do)
                            (send (send fact make-subtr 4 2) do))
                          (send (send fact make-multr 4 2) do)))
                    (list factory strat Adder Subbr Multr))
      (numV 16))








;; static test
;; make sure that instances of classes are actually different
(test (run/classes '(with ((T1 (new Test))
                           (T2 (new Test)))
                      (seq (send T1 set-T 100) (send T2 get-T)))
                    (list '(class Test ()
                            (parent Object)
                            (private)
                            (public (T 10)))))
      (numV 10))

;; method calls within method calls
;; make sure children can call parent methods
(test (run/classes '(with ((T1 (new Child)))
                      (send T1 foo 2))
                    (list '(class Test ()
                            (parent Object)
                            (private) (public)
                            (bar (lambda (x) (+ x x))))
                          '(class Child ()
                            (parent Test)
                            (private) (public)
                            (foo (lambda (x) (send parent bar (+ x x)))))))
      (numV 8))
