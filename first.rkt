#lang plai-typed

;; helpers
;; (num -> num) -> [num] -> [num]
(define (nmap [fn : (number -> number)] [ln : (listof number)]) : (listof number)
  (cond ((empty? ln) empty)
        ((cons? ln) (cons (fn (first ln)) (nmap fn (rest ln))))))

;; (num -> num -> num) -> [num] -> num
(define (nfold fn ln) : number
  (cond ((empty? ln) 0)
        ((empty? (rest ln)) (first ln))
        ((cons?  (rest ln)) (fn (first ln)
                                (nfold fn (rest ln))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; problem 1
;; [num] -> num 
(define (sum-num list-num) : number
  (nfold + list-num))
(test (sum-num (list 1 2 3 4 5 6 7 8 9 0)) 45)
(test (sum-num empty) 0)


;; problem 2
;; [num] -> num
(define (sum-neg list-num) : number
  (sum-num (filter (λ (x) (> 0 x)) list-num)))
(test (sum-neg (list 1 -2 3 -4 5 -6 7 -8 9)) -20)
(test (sum-neg (list 1 2 3 4 5)) 0)
(test (sum-neg empty) 0)


;; problem 3
;; [num] -> num
(define (raise list-num) : (listof number)
  (nmap (λ (x) (cond ((> 0 x) 0) (else x))) list-num))
(test (raise (list -1 -2 -3 -4)) (list 0 0 0 0))
(test (raise empty) empty)


;; problem 4
;; [num] -> [num]
(define (alternating list-e)
  (cond ((empty? list-e) empty)
        ((cons?  list-e) (cons (first list-e)
                               ((λ (ln-e)
                                  (cond ((empty? ln-e) empty)
                                        ((cons?  ln-e) (alternating (rest ln-e))))) (rest list-e))))))
(test (alternating (list 1 2 3 4 5)) (list 1 3 5))
(test (alternating empty) empty)
(test (alternating (list 1)) (list 1))
(test (alternating (list 1 2 3 4)) (list 1 3))
(test (alternating (list "hi" "there" "mom")) (list "hi" "mom"))


;; problem 5
(define-type Scores
  [midterm (humps : number)]
  [final   (humps : number)]
  [course  (humps : number)]