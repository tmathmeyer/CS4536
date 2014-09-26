
;; test pass by value vs pass by reference
(with ((x 5)) (with ((y x)) (with ((z y)) (seq (set x 10) z))))
(lambda (p) (with ((x 5)) (with ((y x)) (with ((z y)) (seq (set x 10) z)))))
(with ((x 5)) (with ((y x)) (with ((z y)) (seq (set y 10) z))))
(with ((x 5)) (with ((y x)) (with ((z y)) (seq (set z 10) z))))
(with ((x 5)) (with ((y x)) (with ((z y)) (seq (set z 10) x))))


;; more reference / value
;; ref -> results in 0
;; val -> results in 5
(with ((mut (lambda (x)  (seq (set x 5) 0))) (init 0)) (seq (mut init) init))



;; examine the desugaring step of the languages and see how they are evaluated
(lambda (x) (with ((y 5)) (seq (set y 10) x)))
((lambda (x) (with ((y x)) (seq (set y 10) x))) 0)
((lambda (x) (with ((y x)) (seq (set x 10) y))) 0)



;; function currying tests
(lambda (x y z) (x y z))
;; strange way of passing, I've heard it called "pass by pointer"


;; box magic
(with ((x (box 5))) (unbox x))
(with ((x (box 5))) (with ((z (setbox x 6))) (unbox x)))
(with ((x (box 5))) (seq (setbox x x) (unbox x)))


;; with-based context evaluation tests !!should error!!
(lambda (z) (with ((x z) (y x) (p y)) p))
(with ((x 5) (y x)) y)



