#lang plai-typed

(require "objects.rkt")

;; basic object
(test (run/classes '(with ((one (new Parent 0)))
                          (send one add 10))
                    (list '(class Parent (var)
                            (parent Object)
                            (private)
                            (public)
                            (add (lambda (x) (+ x var))))))
      (numV 10))

;;basic example of inheritance
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
(test (run/classes '(with ((A (new SetTest 4)))
                      (seq (send A set-m 5) (send A get-m)))
                    (list '(class SetTest (val)
                            (parent Object)
                            (private)
                            (public (m 10)))))
     (numV 5))



;; setting in parents
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
(test/exn (run/classes '(with ((A (new Execer)))
                          (send A exec (lambda () (+ a b))))
                        (list '(class Execer ()
                                (parent Object)
                                (private (a 1) (b 2))
                                (public)
                                (exec (lambda (x) (x))))))
  "unbound")


;; test method overloading
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
(test (run/classes '(with ((T1 (new Test))
                           (T2 (new Test)))
                      (seq (send T1 set-T 100) (send T2 get-T)))
                    (list '(class Test ()
                            (parent Object)
                            (private)
                            (public (T 10)))))
      (numV 10))