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