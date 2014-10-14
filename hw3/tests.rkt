#lang plai-typed

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
                  (with ((next (lambda () (with ((x (unbox count)))
                                                (seq (setbox count (+ x 1)) x)))))
                        (next)))) (numV 1))

(test (run '(with ((x (box 1))
                   (y (box 1)))
                  (+ (unbox x) (unbox y)))) (numV 2))

(test (run '(with ((x (box 1)))
                  (with ((y x))
                        (seq (setbox y 2)
                             (unbox x))))) (numV 1))


;; test setting things
(test (run '(with ((x 5))
                  (seq (set x 6) x))) (numV 6))



;; test the Y combinator
(test (run '(with ((Y (lambda (le)
                              ((lambda (f) (f f))
                               (lambda (f) (le (lambda (x) ((f f) x))))))))
                  (with ((factY (Y (lambda (factorial)
                                           (lambda (n)
                                                   (cond  ((= n 0) 1)
                                                          ((= n 1) 1)
                                                          (else (* n (factorial (- n 1))))))))))
                        (factY 5)))) (numV 120))
