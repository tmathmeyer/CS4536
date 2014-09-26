
;; static scoping holds
(define st_scope '(with ((F (lambda (a b c) (if0 a (F b a c) (F c a b))))) (F 1 3 0)))

;; pass by reference or pass by value
(define passby '(with ((x 6)) (with ((y x)) (seq (set x 4) y))))
(define passby2 '(with ((x (box 5))) (with ((y x)) (seq (set x (box 6)) (unbox y)))))

;; testing setbox (global state voodoo)
(define setbox '(with ((x (box 5))) (seq (setbox x 10) (unbox x))))
(define innerbox '(with ((x (box 5))) (with ((y (box x))) (seq (setbox (unbox y) 10) (unbox x)))))
(define innerbox2 '(with ((x (box 5))) (with ((y (box x))) (seq (setbox x 10) (unbox (unbox y))))))

