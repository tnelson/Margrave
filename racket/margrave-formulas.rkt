#lang typed/racket

#|

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

(struct: m-exists ([var : m-variable-term]
                   [sort : Symbol]
                   [subfmla : m-formula]
                   [syn : (Option Syntax)])  
  #:transparent)

(struct: m-forall ([var : m-variable-term]
                   [sort : Symbol]
                   [subfmla : m-formula]
                   [syn : (Option Syntax)])  
  #:transparent)

(struct: m-isa ([var : m-variable-term]
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


(test (m-exists (m-variable-term 'x #'zot) 'S (m-true #f) #f))
(test (m-forall (m-variable-term 'x #'zot) 'S (m-true #'test) #'foo) )
(test (m-and (list (m-true #'bar) 
                   (m-atomic-edb 'P (list (m-variable-term 'x #'zot)) #'baz)) #'test))
(test (m-or (list (m-false #'bar) 
                  (m-atomic-idb '(mypolicyname) 'permit (list (m-constant-term 'c #'zot1)
                                                              (m-variable-term 'y #'zot2)
                                                              (m-function-term 'f (list (m-variable-term 'y #'zot2)) #'zot3)) #'baz)) #'test))

(: sexp->m-term (Sexp -> m-term))
(define (sexp->m-term s)
  (m-constant-term 'x #f))



(: foo ((Listof Symbol) -> (Option Syntax)))
(define (foo lst)
  #f)

; Populate syn fields appropriately
; Syntax
; SExp
; Identifier = (Syntaxof Symbol)
(: sexp->m-formula (Sexp -> m-formula))
(define (sexp->m-formula s)
  (match s
    ['true
     (m-true #f)]        
    ['false
     (m-false #f)]        
    [(list '= t1 t2)
     (m-equals (sexp->m-term t1) (sexp->m-term t1)  #f)]
                    
    [(list 'and #{args : (Listof Sexp)} ...)
     (m-and (map sexp->m-formula args) #f)]        
    [(list 'or #{args : (Listof Sexp)} ...)
     (m-or (map sexp->m-formula args) #f)]    
    [(list 'implies f1 f2)
     (m-implies (sexp->m-formula f1) (sexp->m-formula f2) #f)]    
    [(list 'iff f1 f2)
     (m-iff (sexp->m-formula f1) (sexp->m-formula f2) #f)]
    [(list 'not subf)
     (m-not (sexp->m-formula subf) #f)]

    [(list 'exists (? symbol? var) (? symbol? sort) subf)
     (m-exists (m-variable-term var #f) sort (sexp->m-formula subf) #f)]    
    [(list 'forall (? symbol? var) (? symbol? sort) subf)
     (m-forall (m-variable-term var #f) sort (sexp->m-formula subf) #f)]    
    [(list 'isa (? symbol? var) (? symbol? sort) subf)
     (m-isa (m-variable-term var #f) sort (sexp->m-formula subf) #f)]

    ; correct pattern:
    ; (match '((1 2) 3) [(list (list a b) c) '(a b c)])
    
    [(list (? symbol? edbname) #{terms : (Listof Sexp)} ...)
     (m-atomic-edb edbname (map sexp->m-term terms) #f)]
    
    ; Looks like the annotation isn't working properly if the ? form is used inside it?
    ; #{} is a reader extension... 
    
   ; [ (list (list (? symbol? #{pids : (Listof Symbol)}) ... (? symbol? idbname))        
   ;         #{terms : (Listof Sexp)} ...)
    ;  (m-atomic-idb pids idbname (map sexp->m-term terms) #f)]
    
    ; NOT ok; simpler
     [ (list (? symbol? #{pids : (Listof Symbol)}) ... (? symbol? idbname)) 
      (m-atomic-idb pids 'idbname empty #f)]
    
    
    ;[`( ,@(list #{pids : (Listof Symbol)} ... (? symbol? idbname)) 
    ;    ,@(list #{terms : (Listof Sexp)} ...))
    ; (m-atomic-idb pids idbname (map sexp->m-term terms) #f)]

    [else (raise "bad m-formula sexpression")]))


; Fails. The checker can't solve undecidable problems. :-)
;(: test2 (Integer -> Integer))
;(define (test2 f) 
;  (cond [(< (random) 0.5) 1]
;        [(> (random) 0.5) 2]
;        [(= (random) 0.5) 3]
;  ))
;(test2 1)


|#