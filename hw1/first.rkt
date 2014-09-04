#lang plai-typed

;;;;
;
; Ted Meyer
;
; each question follows its numbered comment
; tests follow the function definitions
;
;;;;

;; a helper function
;; (num -> num -> num) -> [num] -> num
(define (nfold fn ln) : number
  (cond ((empty? ln) 0)
        ((empty? (rest ln)) (first ln))
        ((cons?  (rest ln)) (fn (first ln)
                                (nfold fn (rest ln))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; problem 1
;; [num] -> num 
(define (sum-num (list-num : (listof number))) : number
  (nfold + list-num))
(test (sum-num (list 1 2 3 4 5 6 7 8 9)) 45)
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
  (map (λ (x) (cond ((> 0 x) 0) (else x))) list-num))
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
  (scores (midterm : number) (final : number) (course : string)))
(define perfect  (scores 100 100 ""))
(define decent   (scores 90 90 ""))
(define passable (scores 80 80 ""))
(define poor     (scores 50 50 ""))
(define shitty   (scores 20 20 ""))

;; problem 6
(define-type Students
  (undergraduate (name : string) (grades : Scores) (graduation-year : number))
  (graduate      (name : string) (grades : Scores) (degree-program : symbol)))
(define stu-a (graduate "a" decent 'MS))
(define stu-b (graduate "b" passable 'PHD))
(define stu-c (graduate "c" poor 'PHD))
(define my-sister (undergraduate "jackie" shitty 2018))
(define me (undergraduate "ted" perfect 2016))











;; problem 7
;; [Student] -> [Student]
(define (assign-grades (lst : (listof Students))) : (listof Students)
  (let ((assign (λ (sco) (type-case Scores sco
                           (scores (m f c) (let ((aa (/ (+ m f) 2)))
                                            (cond ((> aa 84) (scores m f "high pass"))
                                                  ((> aa 64) (scores m f "pass"))
                                                  (else (scores m f "fail")))))))))
  (map (λ (stu) (type-case Students stu
                  (graduate (n g dp) (graduate n (assign g) dp))
                  (undergraduate (n g gy) (undergraduate n (assign g) gy)))) lst)))
(define pre-grade  (list my-sister me))
(define post-grade (list (undergraduate "jackie" (scores 20 20 "fail") 2018)
                         (undergraduate "ted"    (scores 100 100 "high pass") 2016)))
(test (assign-grades pre-grade) post-grade)
(test (assign-grades empty) empty)
     








         
;; problem 8
;; [Student] -> boolean
(define (all-phd-pass? (lst : (listof Students))) : boolean
  (empty? (filter (λ (stu) (type-case Students stu
                     (undergraduate (n g gy) false)
                     (graduate (n g dp) (and (symbol=? dp 'PHD)
                                             (type-case Scores g
                                               (scores (m f c) (not (or (string=? c "pass")
                                                                        (string=? c "high pass"))))))))) lst)))
(test (all-phd-pass? (assign-grades (list stu-a stu-b stu-c me my-sister))) #f) 
(test (all-phd-pass? (assign-grades (list stu-a stu-b me my-sister))) #t)
        
         














;; problem 9
(define (sort-by-midterm (ln : (listof Students))) : (listof Students)
  (student-sort midterm-order ln))

;; (student -> student -> boolean) -> [students] -> [students]
(define (student-sort (fn : (Students Students -> boolean)) (ln : (listof Students))) : (listof Students)
  (cond ((empty? ln) ln)
        ((cons?  ln) (insert-into-list (first ln) fn
                                       (student-sort fn (rest ln))))))

;; student -> [student] -> (student -> student -> boolean)-> [student]
(define (insert-into-list (st : Students) (fn : (Students Students -> boolean)) (ln : (listof Students))) : (listof Students)
  (cond ((empty? ln) (cons st empty))
        ((cons?  ln) (cond ((fn st (first ln)) (cons st ln))
                           (else (cons (first ln)
                                       (insert-into-list st fn (rest ln))))))))

;; student -> student -> boolean
(define (midterm-order (a : Students) (b : Students)) : boolean
  (let ((get-grades (λ (stu) (type-case Students stu
                               (undergraduate (n g gy) g)
                               (graduate (n g dp) g)))))
    (let ((get-midterm (λ (stu) (type-case Scores (get-grades stu)
                                  (scores (m f c) m)))))
      (> (get-midterm a) (get-midterm b)))))

(test (midterm-order me my-sister) #t)
(test (midterm-order stu-a stu-b) #t)
(test (midterm-order stu-c stu-b) #f)

(test (insert-into-list me midterm-order empty) (list me))
(test (insert-into-list stu-b midterm-order (list stu-a stu-c)) (list stu-a stu-b stu-c))

(test (student-sort midterm-order (list stu-c stu-b stu-a)) (list stu-a stu-b stu-c))
  
  









  
;; problem 10
; Yes, this could easily support surting by any criteria on the student type, as the sort-students
; function can take any function (Students -> Students -> boolean), which in the case of problem 9
; is called midterm-order. 
  
  
  
  