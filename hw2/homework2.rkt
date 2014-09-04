#lang plai-typed


(define-type expr
  (plusE (l : expr) (r : expr))
  (multE (l : expr) (r : expr))
  (subtE (l : expr) (r : expr))
  (id (id : symbol))
  (lambda (fp : (listof expr)) (b : expr)) ;fp = formal parameters
  (if0 (e : expr) (t : expr) (f : expr)))
  
(define-type nvar
  (var (lbl : symbol) (eval : expr)))



(define e








;; expr -> number
(define (interp (src : expr)) : number
  (interp-with-context src empty))

;; expr -> [nvar] -> number
(define (interp-with-context (src : expr) (context : (listof nvar))) : number
  (type-case expr src
    (plusE (l r) (+ (interp-with-context l context)
                    (interp-with-context r context)))
    (multE (l r) (* (interp-with-context l context)
                    (interp-with-context r context)))
    (subtE (l r) (- (interp-with-context l context)
                    (interp-with-context r context)))
    (id    (i)   (interp-with-context (lookup-id context i) context))
    (else 0)))

;; [nvar] -> symbol -> expr
(define (lookup-id (context : (listof nvar)) (find : symbol)) : expr
  (let ((results (filter (λ (v) (symbol=? (var-lbl v) find)) context)))
    (cond ((cons? results) (var-eval (first results)))
          (else (error 'failed "failed")))))