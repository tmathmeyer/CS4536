#lang plai-typed

(require "inf.rkt")

(define-type Lazy
  (ty (t : Type))
  (ex (e : ExprS)))

(define-type Binding
  (bind (name : symbol) (type : Lazy)))
 
(define-type-alias Env (listof Binding))

(define (easy-infer (ast : ExprS) (env : Env)) : Lazy
  (type-case ExprS ast
    (numS  (n)     (ty (numT)))
    (boolS (b)     (ty (boolT)))
    (temptyS ()    (ty (tlistT (varT 'a))))
    (plusS (a b)   (ty (numT)))
    (multS (a b)   (ty (numT)))
    (bminusS (a b) (ty (numT)))
    (iszeroS (a)   (ty (boolT)))
    (tisEmptyS (a) (ty (boolT)))
    (tconsS (f r)  (ty (parse-list f r env)))
    (bifS (c t f)  (ty (parse-bool t f env)))
    (tfirstS (l)   (ty (tlistT-elem (ty-t (easy-infer l env)))))
    (idS (n)       (lookup n env))
    (trestS  (l)   (easy-infer l env))
    (withS (b t e) (easy-infer e (cons (bind b (easy-infer t env)) env)))
    (lamS (p b)    (ex (lamS p b)))
    (appS (f a)    (parse-fn-call f a env))
    (else (error 'very "odd"))))

(define (parse-fn-call (fnid : ExprS) (arg : ExprS) (env : Env)) : Lazy
  (let ((lam (type-case ExprS fnid
               (idS (fnname) (ex-e (lookup fnname env)))
               (else fnid))))
    (let ((fnarg (lamS-param lam)))
      (let ((tbind (bind fnarg (easy-infer arg env))))
        (let ((fnterp (easy-infer (lamS-body lam) (cons tbind env))))
          fnterp)))))

(define (parse-list (f : ExprS) (r : ExprS) (bind : Env)) : Type
  (let ((first (maybe-lookup (easy-infer f bind) bind))
        (rest  (tlistT-elem (maybe-lookup (easy-infer r bind) bind))))
   (tlistT (error-on-mismatch rest first))))
            
(define (parse-bool (t : ExprS) (f : ExprS) (bind : Env)) : Type
  (let ((a (maybe-lookup (easy-infer t bind) bind))
        (b (maybe-lookup (easy-infer f bind) bind)))
    (error-on-mismatch a b)))

(define (error-on-mismatch (a : Type) (b : Type)) : Type
   (type-case Type b
     (varT (n) a)
     (else (type-case Type a
             (varT (n) b)
             (else (cond ((type=? a b) a)
                         (else (error 'type-mismatch (strcomb (list "[" (t->s a) "]  [" (t->s b) "]"))))))))))

(define (lookup (n : symbol) (bind : Env)) : Lazy
  (let ((find (filter (lambda (x) (symbol=? (bind-name x) n)) bind)))
    (cond ((empty? find) (ty (varT n)))
          ((cons? find) (bind-type (first find))))))

(define (maybe-lookup (t : Lazy) (bind : Env)) : Type
  (type-case Type (ty-t t)
    (varT (n) (ty-t (lookup n bind)))
    (else (ty-t t))))

(define (strcomb (l : (listof string))) : string
  (foldr string-append "" l))

(define (t->s (t : Type)) : string
  (type-case Type t
    (numT () "num")
    (boolT () "bool")
    (tlistT (e) (strcomb (list "[" (t->s e) "]")))
    (funT (a t) (strcomb (list (t->s a) " -> " (t->s t)))) 
    (varT (n)   (string-append "_" (symbol->string n)))))


(define (infer (l : Lazy)) : Type
  (type-case Lazy l
    (ty (t) t)
    (ex (e) (let ((sym (lamS-param e)))
              (funT (varT sym) (infer (easy-infer (lamS-body e)
                                                  (cons (bind sym (ty (varT sym))) empty))))))))

(define (infer-to-string (exp : ExprS)) : string
  (t->s (infer (easy-infer exp empty))))

(define (ip-to-string (exp : s-expression)) : string
  (infer-to-string (parse exp)))

(test (ip-to-string '5) "num")
(test (infer-to-string (boolS false)) "bool")
(test (infer-to-string (temptyS)) "[_a]")
(test (ip-to-string '(+ 5 7)) "num")
(test (ip-to-string '(* 5 7)) "num")
(test (ip-to-string '(- 5 7)) "num")
(test (ip-to-string '(iszero 5)) "bool")

(test (ip-to-string '(tempty? tempty)) "bool")
(test (ip-to-string '(tcons 5 tempty)) "[num]")

(test (ip-to-string '(bif (iszeroS 5) 5 7)) "num")

(test (ip-to-string '(with ((x (tcons 6 (tcons 5 tempty))))
                           (tfirst x))) "num")

(test (ip-to-string '(with ((x (tcons 6 (tcons 5 tempty))))
                           (trest x))) "[num]")

(test (ip-to-string '(lambda (x) (lambda (y) (+ y x)))) "_x -> _y -> num")

(test (ip-to-string '(lambda (x) (+ x 1))) "_x -> num")

(test (ip-to-string '(with ((x (lambda (x) (tcons x tempty)))) (x 3))) "[num]")

(test (ip-to-string '(with ((id (lambda (x) x))) (id 5))) "num")

(test (ip-to-string '(with ((id (lambda (x) x))) (id tempty))) "[_a]")
(test (ip-to-string '(with ((id (lambda (x) x))) (id (tcons 5 tempty)))) "[num]")

(test (ip-to-string '(tcons tempty tempty)) "[[_a]]")

(test (ip-to-string '((lambda (x) (lambda (x) x)) 5)) "_x -> _x")

(test (ip-to-string '((lambda (x) (lambda (x) (lambda (x) x))) 5)) "_x -> _x -> _x")

(test (ip-to-string '(with ((f (lambda (x) x))) (f f))) "_x -> _x")
