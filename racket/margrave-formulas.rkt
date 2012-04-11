#lang typed/racket



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type m-term (U m-constant-term 
                       m-variable-term
                       m-function-term))

(struct: m-constant-term ([id : Symbol]                          
                          [syn : (Option Syntax)]) #:transparent)

(struct: m-variable-term ([id : Symbol]                          
                          [syn : (Option Syntax)]) #:transparent)

(struct: m-function-term ([func : Symbol]
                          [args : (Listof m-term)]
                          [syn : (Option Syntax)]) #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type m-formula (U m-exists m-forall m-isa
                          m-and m-or m-not m-implies m-iff
                          m-true m-false m-equals               
                          m-atomic-idb m-atomic-edb))

(struct: m-true ([syn : (Option Syntax)])
  #:transparent)
(struct: m-false ([syn : (Option Syntax)])
  #:transparent)

(struct: m-and ([conjuncts : (Listof m-formula)]
                [syn : (Option Syntax)]) #:transparent)

(struct: m-or ([disjuncts : (Listof m-formula)]
               [syn : (Option Syntax)]) #:transparent)

(struct: m-implies ([premise : m-formula]
                    [conclusion : m-formula]
                    [syn : (Option Syntax)]) #:transparent)

(struct: m-iff ([subfmla1 : m-formula]
                [subfmla2 : m-formula]
                [syn : (Option Syntax)]) #:transparent)

(struct: m-not ([subfmla : m-formula]
                [syn : (Option Syntax)]) #:transparent)

(struct: m-exists ([var : Symbol]
                   [sort : Symbol]
                   [subfmla : m-formula]
                   [syn : (Option Syntax)])  
  #:transparent)

(struct: m-forall ([var : Symbol]
                   [sort : Symbol]
                   [subfmla : m-formula]
                   [syn : (Option Syntax)])  
  #:transparent)

(struct: m-isa ([var : Symbol]
                [sort : Symbol]
                [subfmla : m-formula]
                [syn : (Option Syntax)])  
  #:transparent)

(struct: m-atomic-edb ([pred : Symbol] 
                       [args : (Listof m-term)]                                            
                       [syn : (Option Syntax)])  
  #:transparent)

(struct: m-atomic-idb ([context : (Listof Symbol)]
                       [pred : Symbol] 
                       [args : (Listof m-term)]                                            
                       [syn : (Option Syntax)])  
  #:transparent)

(struct: m-equals ([term1 : m-term]
                   [term2 : m-term]                   
                   [syn : (Option Syntax)])  
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The type of a cond on failure is void. So type error if not all types accounted for
; This would not work if the function returned void by design sometimes, and void was allowed in the decl.
(: test (m-formula -> Integer))
(define (test f) 
  (cond [(m-true? f) 1]        
        [(m-false? f) 2]
        [(m-exists? f) 3]
        [(m-forall? f) 4]
        [(m-or? f) 5]        
        [(m-and? f) 6]
        [(m-not? f) 7]
        [(m-implies? f) 8]
        [(m-iff? f) 9]
        [(m-isa? f) 10]
        [(m-atomic-edb? f) 11]
        [(m-atomic-idb? f) 12]
        [(m-equals? f) 12]
  ))


(test (m-exists 'x 'S (m-true #f) #f))
(test (m-forall 'x 'S (m-true #'test) #'foo) )
(test (m-and (list (m-true #'bar) 
                   (m-atomic-edb 'P (list (m-variable-term 'x #'zot)) #'baz)) #'test))
(test (m-or (list (m-false #'bar) 
                  (m-atomic-idb '(mypolicyname) 'permit (list (m-constant-term 'c #'zot1)
                                                              (m-variable-term 'y #'zot2)
                                                              (m-function-term 'f (list (m-variable-term 'y #'zot2)) #'zot3)) #'baz)) #'test))

(: sexp->m-term (Datum -> m-term))
(define (sexp->m-term s)
  (m-constant-term 'x #f))


; Populate syn fields appropriately
; Syntax
; SExp
; Identifier = (Syntaxof Symbol)
(: sexp->m-formula (Datum -> m-formula))
(define (sexp->m-formula s)
  (match s
    ['true
     (m-true #f)]        
    ['false
     (m-false #f)]        
    
    [(list '= t1 t2)
     (m-equals (sexp->m-term t1) (sexp->m-term t1)  #f)]
    
    ;[`(and ,@(list args ...))               
    [(list 'and args ...)              
     ; getting (Listof Any) as type of arg
     (m-and (map sexp->m-formula args) #f)]    
    ;[(m-op-case or args ...)
    ; (andmap (lambda (f) (m-formula-is-well-sorted?/err voc f env)) args)]
    ;[(m-op-case implies arg1 arg2)
    ; (and (m-formula-is-well-sorted?/err voc arg1 env) (m-formula-is-well-sorted?/err voc arg2 env))]   
    ;[(m-op-case iff arg1 arg2)
    ; (and (m-formula-is-well-sorted?/err voc arg1 env) (m-formula-is-well-sorted?/err voc arg2 env))]   
    ;[(m-op-case not arg)
    ; (m-formula-is-well-sorted?/err voc arg env)]   
   ; 
   ; [(m-op-case forall vname sname subfmla)
   ;  (m-formula-is-well-sorted?/err voc subfmla (hash-set env vname sname))]     
   ; [(m-op-case exists vname sname subfmla)
   ;  (m-formula-is-well-sorted?/err voc subfmla (hash-set env vname sname))]  
   ; ; If (isa x A alpha) is sugar for (exists y A (and (= x y) alpha[x -> y]))
   ; ; the sort of x must be _replaced_, not augmented, by the new sort.
   ; [(m-op-case isa vname sname subfmla)
   ;  (m-formula-is-well-sorted?/err voc subfmla (hash-set env vname sname))]     
   ; 
   ; ; (idb term0 terms ...)    
   ; [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,@(list terms ...))
   ;  (internal-correct/idb terms pids idbname)] 
   ; 
   ; [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
   ;  (cond
   ;    [(and (valid-sort? dbname) 
   ;          (empty? terms))
   ;     (internal-correct/isa-sugar term0 (->string dbname))] ; sugar for (isa x A true)      
   ;    [else (internal-correct/edb (cons term0 terms) (->string dbname))])] ; (edb term0 terms ...)
   ;
    [else (raise "x")]))


; Fails. The checker can't solve undecidable problems. :-)
;(: test2 (Integer -> Integer))
;(define (test2 f) 
;  (cond [(< (random) 0.5) 1]
;        [(> (random) 0.5) 2]
;        [(= (random) 0.5) 3]
;  ))
;(test2 1)


