#lang plai-typed

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)])
         (typed-in racket/base [gensym : (symbol -> symbol)])
         )

;; starter file for type inference assignment 

;;;;;;;;;; Surface Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprS
  [numS (n : number)]
  [boolS (b : boolean)]
  [temptyS]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (arg : ExprS)]
  [iszeroS (e : ExprS)]
  [bifS (c : ExprS) (t : ExprS) (e : ExprS)]
  [lamS (param : symbol) (body : ExprS)]
  [withS (var : symbol) (val : ExprS) (body : ExprS)]
  [recS (var : symbol) (val : ExprS) (body : ExprS)]
  [tconsS (e : ExprS) (l : ExprS)]
  [tisEmptyS (e : ExprS)]
  [tfirstS (e : ExprS)]
  [trestS (e : ExprS)]
  )

; the varT type supports type parameters.  For example, the
; identity function has type funT((varT'a) (varT 'a)), where
; 'a is variable over types
(define-type Type
  [numT]
  [boolT]
  [tlistT (elem : Type)]
  [funT (arg : Type) (return : Type)]
  [varT (v : symbol)])
  
; equality checker for types that supports type variables.
;  two types are equal if there exists a one-to-one mapping between
;  their variable names under which the types become lexically identical
;  (in addition to structurally identical)
(define (type=? (t1 : Type) (t2 : Type)) : boolean
  (local ([define ht1 (make-hash empty)] ; maps vars in t1 to vars in t2
          [define ht2 (make-hash empty)] ; vice versa
          [define (teq? t1 t2)
            (cond
              [(and (numT? t1) (numT? t2)) true]
              [(and (boolT? t1) (boolT? t2)) true]
              [(and (tlistT? t1) (tlistT? t2))
               (teq? (tlistT-elem t1) (tlistT-elem t2))]
              [(and (funT? t1) (funT? t2))
               (and (teq? (funT-arg t1) (funT-arg t2))
                    (teq? (funT-return t1) (funT-return t2)))]
              [(and (varT? t1) (varT? t2))
               ; v1 is the type that ht1 says that t1 maps to, or the var of t2 if no mapping exists
               ; v2 is analogous
               (let ([v1 (let ([r (hash-ref ht1 (varT-v t1))])
                           (if (some? r) 
                               ; var is in the hash, return the mapped value
                               (some-v r)
                               ; else add new mapping to hash and return the newly mapped variable
                               (begin (hash-set! ht1 (varT-v t1) (varT-v t2))
                                      (varT-v t2))))]
                     [v2 (let ([r (hash-ref ht2 (varT-v t2))])
                           (if (some? r)
                               (some-v r)
                               (begin (hash-set! ht2 (varT-v t2) (varT-v t1))
                                      (varT-v t1))))])
                 ; we have to check both mappings, so that distinct variables
                 ; are kept distinct. i.e. a -> b should not be isomorphic to
                 ; c -> c under the one-way mapping a => c, b => c.
                 (and (symbol=? (varT-v t2) v1) (symbol=? (varT-v t1) v2)))]
              [else false])]) ; types aren't equal
    (teq? t1 t2)))

#|
Here are some examples of type=? so you can see what it should do.  Feel free
to delete this from the starter file once you understand type=?

(test (type=? (varT 'a) (varT 'b)) true)

(test (type=? (funT (varT 'a) (varT 'a)) 
              (funT (numT) (numT)))
      false)

(test (type=? (funT (varT 'a) (varT 'a)) 
              (funT (varT 'b) (varT 'b)))
      true)

(test (type=? (funT (varT 'a) (varT 'b)) 
              (funT (varT 'b) (varT 'b)))
      false)

(test (type=? (funT (varT 'a) (varT 'b)) 
              (funT (varT 'b) (varT 'a)))
      true)

(test (type=? (funT (varT 'a) (varT 'a)) 
              (funT (varT 'b) (varT 'a)))
      false)
|#


;;;;;;;;;;;; Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; parses s-expressions into surface syntax
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) 
     (case (s-exp->symbol s)
       [(true) (boolS true)]
       [(false) (boolS false)]
       [(tempty) (temptyS)]
       [else (idS (s-exp->symbol s))])]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(iszero) (iszeroS (parse (second sl)))]
                [(bif) (bifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(lambda) (lamS (s-exp->symbol (first (s-exp->list (second sl)))) (parse (third sl)))]
                [(with) (let ([bindings (s-exp->list (second sl))]
                              [body (third sl)])
                          (begin (unless (= 1 (length bindings))
                                   (error 'parse (string-append "parse error: with expects list containing one binding but got " (to-string bindings))))
                                 (let ([binding (s-exp->list (first bindings))])
                                   (withS (s-exp->symbol (first binding))
                                          (parse (second binding))
                                          (parse body)))))]
                [(rec) (let ([bindings (s-exp->list (second sl))]
                              [body (third sl)])
                          (begin (unless (= 1 (length bindings))
                                   (error 'parse (string-append "parse error: with expects list containing one binding but got " (to-string bindings))))
                                 (let ([binding (s-exp->list (first bindings))])
                                   (recS (s-exp->symbol (first binding))
                                          (parse (second binding))
                                          (parse body)))))]                
                [(tcons) (tconsS (parse (second sl)) (parse (third sl)))]
                [(tempty?) (tisEmptyS (parse (second sl)))]
                [(tfirst) (tfirstS (parse (second sl)))]
                [(trest) (trestS (parse (second sl)))]
                [else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (parse (second sl)))])]
             [else (appS (parse (first sl))
                         (parse (second sl)))]))]
    [else (error 'parse "parse error: unexpected syntax")]))

;;;;;;;;;;;;;; type checker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; feel free to add any other helpers that you deem necessary

; generate-constraints :: <you decide the contract>
#;(define generate-constraints
  ...)

; unify :: <you decide the contract>
#;(define unify
  ...)

; type-of :: Expr -> Type
; this will call generate-constraints and unify, in a way that
;  is consistent with your types for these functions
(define (type-of (e : ExprS)) : Type
  (numT))

;;;;;;;;;;;;; API for type checking programs ;;;;;;;;;;;

(define (infer-type sexp)
  (call-with-limits 
   10 #f
   (lambda () (type-of (parse sexp)))))