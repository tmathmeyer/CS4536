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
(define PRETTY (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))

(define (readable-map list fxn)
  (map fxn list))

(define-type Constraint
  (eqCon (lhs : Term) (rhs : Term)))

(define-type Term
  (tExp (e : ExprS))
  (tVar (s : symbol))
  (tList (t : Term))
  (tNum)
  (tBool)
  (tArrow (i : Term) (o : Term)))
  
(define-type Rebinding
  (rebind (from : symbol)
          (to   : symbol)))

;; make all symbols globally unique, but still match on scope
(define (make-unique (ast : ExprS) (map : (listof Rebinding))) : ExprS
   (type-case ExprS ast
     (numS  (n)     (numS n))
     (boolS (b)     (boolS b))
     (temptyS ()    (temptyS))
     (plusS (a b)   (plusS (make-unique a map) (make-unique b map)))
     (multS (a b)   (multS (make-unique a map) (make-unique b map)))
     (bminusS (a b) (bminusS (make-unique a map) (make-unique b map)))
     (iszeroS (a)   (iszeroS (make-unique a map)))
     (tisEmptyS (a) (tisEmptyS (make-unique a map)))
     (tconsS (f r)  (tconsS (make-unique f map) (make-unique r map)))
     (bifS (c t f)  (bifS (make-unique c map) (make-unique t map) (make-unique f map)))
     (tfirstS (l)   (tfirstS (make-unique l map)))
     (trestS (l)    (trestS (make-unique l map)))
     (appS (f a)    (appS (make-unique f map) (make-unique a map)))
     (withS (b t e) (let ((new (gensym b))) (withS new (make-unique t map) (make-unique e (cons (rebind b new) map)))))
     (recS (b t e)  (let ((new (gensym b))) (recS new (make-unique t map) (make-unique e (cons (rebind b new) map)))))
     (lamS (p b)    (let ((new (gensym p))) (lamS new (make-unique b (cons (rebind p new) map)))))
     (idS (n)       (idS (lookup n map)))))

;; get a symbol from a list
(define (lookup (s : symbol) (map : (listof Rebinding))) : symbol
  (let ((match (filter (λ (each) (symbol=? s (rebind-from each))) map)))
    (cond ((empty? match) s)
          ((cons? match) (rebind-to (first match))))))

;; append three lists together
(define (tripend a b c)
  (append a (append b c)))
;; append four lists together
(define (quapend a b c d)
  (append (append a b) (append c d)))


;; build constraint list
(define (bc (ast : ExprS)) : (listof Constraint)
  (type-case ExprS ast
    (numS  (n)     (list (eqCon (tExp ast) (tNum))))
    (boolS (b)     (list (eqCon (tExp ast) (tBool))))
    (temptyS ()    (list (eqCon (tExp ast) (tList (tVar '_a)))))
    (plusS (a b)   (tripend (bc a) (bc b) (list (eqCon (tExp a) (tNum)) (eqCon (tExp b) (tNum)) (eqCon (tExp ast) (tNum)))))
    (multS (a b)   (tripend (bc a) (bc b) (list (eqCon (tExp a) (tNum)) (eqCon (tExp b) (tNum)) (eqCon (tExp ast) (tNum)))))
    (bminusS (a b) (tripend (bc a) (bc b) (list (eqCon (tExp a) (tNum)) (eqCon (tExp b) (tNum)) (eqCon (tExp ast) (tNum)))))
    (iszeroS (a)   (append (bc a) (list (eqCon (tExp ast) (tBool)) (eqCon (tExp a) (tNum)))))
    (tisEmptyS (a) (append (bc a) (list (eqCon (tExp ast) (tBool)) (eqCon (tExp a) (tList (tList (tVar (gensym 't)))))))) ;;todo
    (tconsS (e l)  (tripend (bc e) (bc l) (list (eqCon (tExp l) (tList (tExp e))) (eqCon (tExp ast) (tList (tExp e))))))
    (bifS (c t f)  (quapend (bc c) (bc t) (bc f) (list (eqCon (tExp c) (tBool)) (eqCon (tExp t) (tExp f)) (eqCon (tExp ast) (tExp t)))))
    (tfirstS (l)   (append (bc l) (let ((id (gensym 't)))
                                    (list (eqCon (tExp ast) (tVar id))
                                          (eqCon (tExp l) (tList (tVar id)))))))
    (trestS (l)    (append (bc l) (let ((id (gensym 't))) (list (eqCon (tExp ast) (tList (tVar id))) (eqCon (tExp l) (tList (tVar id)))))))
    (appS (f a)    (tripend (bc f) (bc a) (list (eqCon (tExp f) (tArrow (tExp a) (tExp ast))))))
    (withS (b t e) (tripend (bc t) (bc e) (list (eqCon (tExp ast) (tExp e)) (eqCon (tVar b) (tExp t)))))
    (lamS (p b)    (cons (eqCon (tExp ast) (tArrow (tVar p) (tExp b))) (bc b)))
    (idS (n)       (list (eqCon (tExp ast) (tVar n))))
    (recS (b t e)  empty)))



(define (unify (stack : (listof Constraint)) (subst : (listof Constraint))) : (listof Constraint)
  (cond ((empty? stack) subst)
        ((cons? stack) (let ((current (first stack)) (stack (rest stack)))
                         (cond ((equal? (eqCon-lhs current) (eqCon-rhs current))
                                (unify stack (cons current subst)))
                               ((is-identifier? (eqCon-lhs current))
                                (let ((stack (replace-in-stack (eqCon-lhs current) (eqCon-rhs current) stack))
                                      (subst (replace-in-stack (eqCon-lhs current) (eqCon-rhs current) subst)))
                                  (unify stack (cons current subst))))
                               ((is-identifier? (eqCon-rhs current))
                                (let ((stack (replace-in-stack (eqCon-rhs current) (eqCon-lhs current) stack))
                                      (subst (replace-in-stack (eqCon-rhs current) (eqCon-lhs current) subst)))
                                  (unify stack (cons current subst))))
                               ((matching-arity? current)
                                (let ((stack-additions (create-stack-additions current)))
                                  (let ((stack (append stack-additions stack)))
                                    (unify stack subst))))
                               (else (begin (display current)
                                            (error 'not-implemented "lol"))))))))
;; Constants:
;;   tNum, tBool, tList, tArrow
;;
;; Identifiers:
;;   tVar, tExp
(define (is-identifier? (t : Term)) : boolean
  (type-case Term t
    (tVar (s) true)
    (tExp (s) true)
    (tList (x) true)
    (else false)))

(define (is-fn? (t : Term)) : boolean
  (type-case Term t
    (tArrow (a b) true)
    (else false)))

(define (matching-arity? (c : Constraint)) : boolean
  (and (is-fn? (eqCon-lhs c))
       (is-fn? (eqCon-rhs c))))

(define (create-stack-additions (c : Constraint)) : (listof Constraint)
  (let ((X (eqCon-lhs c)) (Y (eqCon-rhs c)))
    (list (eqCon (tArrow-i X) (tArrow-i Y))
          (eqCon (tArrow-o X) (tArrow-o Y)))))
    

(define (replace-in-stack (from : Term) (to : Term) (st : (listof Constraint))) : (listof Constraint)
  (readable-map st (lambda (each)
                     (eqCon (replace-in-eqCon from to (eqCon-lhs each))
                            (replace-in-eqCon from to (eqCon-rhs each))))))

(define (replace-in-eqCon (from : Term) (to : Term) (of : Term)) : Term
  (cond ((equal? from of) to)
        (else (type-case Term of
                (tArrow (i o) (tArrow (replace-in-eqCon from to i) (replace-in-eqCon from to o)))
                (tList  (e)   (tList (replace-in-eqCon from to e)))
                (else of)))))

(define (find-in-constraints (con : (listof Constraint)) (search : ExprS)) : Term
  (let ((matching (filter (lambda (x) (contains-exprs x search)) con)))
    (cond ((empty? matching) (error 'no-matches "it seems that failed, sorry"))
          ((cons? matching) (let ((exp (first matching)))
                              (let ((rhs (eqCon-rhs exp)) (lhs (eqCon-lhs exp)))
                                (cond ((is-expr rhs search) lhs)
                                      ((is-expr lhs search) rhs)
                                      (else (error '??? "how did you even get here???")))))))))

(define (contains-exprs (con : Constraint) (search : ExprS)) : boolean
  (or (is-expr (eqCon-lhs con) search)
      (is-expr (eqCon-rhs con) search)))
           
(define (is-expr (t : Term) (s : ExprS)) : boolean
  (type-case Term t
    (tExp (exp) (equal? exp s))
    (else false)))

(define (symlen (s : symbol)) : number
  (string-length (symbol->string s)))

(define (get-ugly-vars (t : Type)) : (listof symbol)
  (type-case Type t
    (tlistT (e) (get-ugly-vars e))
    (funT (a t) (append (get-ugly-vars a) (get-ugly-vars t)))
    (varT (n)   (cond ((> (symlen n) 1) (list n))))
    (else empty)))

(define (remove-dups from)
  (cond ((empty? from) empty)
        ((cons? from) (let ((f (first from)) (r (rest from)))
                        (let ((r (filter (λ (x) (not (equal? x f))) r)))
                          (cons f (remove-dups r)))))))

(define (match-sym (bad : (listof symbol)) (good : (listof symbol))) : (listof Rebinding)
  (cond ((empty? bad) empty)
        ((cons? bad)  (cons (rebind (first bad) (first good))
                            (match-sym (rest bad) (rest good))))))

(define (rebind-ugly (t : Type) (with : (listof Rebinding))) : Type
  (type-case Type t
    (numT ()  (numT))
    (boolT () (boolT))
    (tlistT (e) (tlistT (rebind-ugly e with)))
    (funT (a t) (funT (rebind-ugly a with) (rebind-ugly t with)))
    (varT (n)   (varT (lookup n with)))))

(define (beautify-type (t : Type)) : Type
  (rebind-ugly t (match-sym (remove-dups (get-ugly-vars t)) PRETTY)))

(define (t->t (t : Term)) : Type
  (type-case Term t
    (tNum ()      (numT))
    (tBool ()     (boolT))
    (tList (t)    (tlistT (t->t t)))
    (tVar (s)     (varT s))
    (tArrow (i o) (funT (t->t i) (t->t o))) 
    (tExp (e)     (varT 'not-implemented))))

; type-of :: Expr -> Type
; this will call generate-constraints and unify, in a way that
;  is consistent with your types for these functions
(define (type-of (e : ExprS)) : Type
  (let ((utree (make-unique e empty)))
    ;;(t->t (find-in-constraints (unify (bc utree) empty) utree))))
    (beautify-type (t->t (find-in-constraints (unify (bc utree) empty) utree)))))

;;;;;;;;;;;;; API for type checking programs ;;;;;;;;;;;

(define (infer-type sexp)
  (call-with-limits 
   10 #f
   (lambda () (type-of (parse sexp)))))