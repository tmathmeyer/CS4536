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
                         (cond [(= 1 (length bindings)) ;; binding has form ((var val))
                                (let ([binding (s-exp->list (first bindings))])
                                  (recS (s-exp->symbol (first binding))
                                        (parse (second binding))
                                        (parse body)))]
                               [(= 2 (length bindings)) ;; binding has form (var val)
                                (recS (s-exp->symbol (first bindings))
                                      (parse (second bindings))
                                      (parse body))]
	       [else (error 'parse "parse error: unrecognized binding format in rec bindings")]))]                
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
    (tfirstS (l)   (append (bc l) (let ((id (gensym 't))) (list (eqCon (tExp ast) (tVar id)) (eqCon (tExp l) (tList (tVar id)))))))
    (trestS (l)    (append (bc l) (let ((id (gensym 't))) (list (eqCon (tExp ast) (tList (tVar id))) (eqCon (tExp l) (tList (tVar id)))))))
    (appS (f a)    (tripend (bc f) (bc a) (list (eqCon (tExp f) (tArrow (tExp a) (tExp ast))))))
    (withS (b t e) (tripend (bc t) (bc e) (list (eqCon (tExp ast) (tExp e)) (eqCon (tVar b) (tExp t)))))
    (lamS (p b)    (cons (eqCon (tExp ast) (tArrow (tVar p) (tExp b))) (bc b)))
    (idS (n)       (list (eqCon (tExp ast) (tVar n))))
    (recS (b t e)  (tripend (bc t) (bc e) (list (eqCon (tExp ast) (tExp e)) (eqCon (tVar b) (tExp t)))))))
          
          
          ;; sym expr expr



(define (unify (stack : (listof Constraint)) (subst : (listof Constraint))) : (listof Constraint)
  (cond ((empty? stack) subst)
        ((cons? stack) (let ((current (first stack)) (stack (rest stack)))
                         (cond ((occurs-error current) (error 'err "occurs check")) 
                               ((equal? (eqCon-lhs current) (eqCon-rhs current))
                                (unify stack (cons current subst)))
                               ((is-identifier? (eqCon-lhs current))
                                (let ((stack (replace-in-stack (eqCon-lhs current) (eqCon-rhs current) stack))
                                      (subst (replace-in-stack (eqCon-lhs current) (eqCon-rhs current) subst)))
                                  (unify stack (cons current subst))))
                               ((is-identifier? (eqCon-rhs current))
                                (let ((stack (replace-in-stack (eqCon-rhs current) (eqCon-lhs current) stack))
                                      (subst (replace-in-stack (eqCon-rhs current) (eqCon-lhs current) subst)))
                                  (unify stack (cons current subst))))
                               ((double-arity? current)
                                (let ((stack-additions (create-stack-additions current)))
                                  (let ((stack (append stack-additions stack)))
                                    (unify stack subst))))
                               ((single-arity? current)
                                (let ((X (eqCon-lhs current)) (Y (eqCon-rhs current)))
                                  (let ((stack-addition (eqCon (tList-t X) (tList-t Y))))
                                    (let ((stack (cons stack-addition stack)))
                                      (unify stack subst)))))
                               (else (let ((X (eqCon-lhs current)) (Y (eqCon-rhs current)))
                                       (error 'error (strc (list "type error: "
                                                                 (t->s (t->t X))
                                                                 " vs "
                                                                 (t->s (t->t Y))))))))))))
;; Constants:
;;   tNum, tBool, tList, tArrow
;;
;; Identifiers:
;;   tVar, tExp
(define (is-identifier? (t : Term)) : boolean
  (type-case Term t
    (tVar (s) true)
    (tExp (s) true)
    (else false)))

(define (is-fn? (t : Term)) : boolean
  (type-case Term t
    (tArrow (a b) true)
    (else false)))

(define (is-ls? (t : Term)) : boolean
  (type-case Term t
    (tList (b) true)
    (else false)))

(define (double-arity? (c : Constraint)) : boolean
  (and (is-fn? (eqCon-lhs c))
       (is-fn? (eqCon-rhs c))))

(define (single-arity? (c : Constraint)) : boolean
  (and (is-ls? (eqCon-lhs c))
       (is-ls? (eqCon-rhs c))))

(define (create-stack-additions (c : Constraint)) : (listof Constraint)
  (let ((X (eqCon-lhs c)) (Y (eqCon-rhs c)))
    (list (eqCon (tArrow-i X) (tArrow-i Y))
          (eqCon (tArrow-o X) (tArrow-o Y)))))
    

(define (replace-in-stack (from : Term) (to : Term) (st : (listof Constraint))) : (listof Constraint)
  (readable-map st (lambda (each)
                     (eqCon (replace-in-eqCon from to (eqCon-lhs each))
                            (replace-in-eqCon from to (eqCon-rhs each))))))

(define (occurs-check (from : Term) (to : Term)) : boolean
  (type-case Term to
    (tArrow (f t) (or (occurs-check from f) (occurs-check from t)))
    (tList  (e)   (occurs-check from e))
    (tVar   (s)   (equal? from to))
    (else false)))

(define (occurs-error (c : Constraint)) : boolean
  (let ((X (eqCon-lhs c)) (Y (eqCon-rhs c)))
    (or (occurs-check X Y)
        (occurs-check Y X))))

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

(define (t->t (t : Term)) : Type
  (type-case Term t
    (tNum ()      (numT))
    (tBool ()     (boolT))
    (tList (t)    (tlistT (t->t t)))
    (tVar (s)     (varT s))
    (tArrow (i o) (funT (t->t i) (t->t o))) 
    (tExp (e)     (varT 'not-implemented))))

(define (strc (l : (listof string))) : string
  (foldr string-append "" l))

(define (t->s (t : Type)) : string
  (type-case Type t
    (numT () "num")
    (boolT () "bool")
    (tlistT (e) (strc (list "[" (t->s e) "]")))
    (funT (a t) (strc (list "(" (t->s a) " -> " (t->s t) ")"))) 
    (varT (n)   (symbol->string n))))

; type-of :: Expr -> Type
; this will call generate-constraints and unify, in a way that
;  is consistent with your types for these functions
(define (type-of (e : ExprS)) : Type
  (let ((utree (make-unique e empty)))
    (t->t (find-in-constraints (unify (bc utree) empty) utree))))

;;;;;;;;;;;;; API for type checking programs ;;;;;;;;;;;

(define (infer-type sexp)
  (call-with-limits 
   10 #f
   (lambda () (type-of (parse sexp)))))