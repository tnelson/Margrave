;    Copyright (c) 2009-2010 Brown University and Worcester Polytechnic Institute.
;    
;    This file is part of Margrave.

;    Margrave is free software: you can redistribute it and/or modify
;    it under the terms of the GNU Lesser General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Margrave is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Lesser General Public License for more details.
;
;    You should have received a copy of the GNU Lesser General Public License
;    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.

#lang racket/base

(require ;margrave/margrave-xml
 (file "margrave-xml.rkt")
         margrave/helpers         
         racket/list
         racket/contract
         (for-syntax margrave/helpers
                     ;margrave/margrave-xml
                     (file "margrave-xml.rkt")
                     racket/list
                     racket/match
                     racket/string
                     racket/base
                     racket/contract))

(provide evaluate-policy
         
         ; for test cases
         Policy
        ; PolicySet
         Vocab)

; We use eval to load policies and vocabularies, and the call is in the definitions window.
; Thus we need to provide a namespace for eval, or it won't know what to do with the Policy
; and PolicyVocab syntax.
(define-namespace-anchor margrave-policy-vocab-namespace-anchor)
(define margrave-policy-vocab-namespace (namespace-anchor->namespace margrave-policy-vocab-namespace-anchor))

;****************************************************************
; Structs used to pass back info about a vocabulary in a structured fashion

(define-struct/contract m-vocabulary 
  ([name string?] [cmds list?] [types (listof string?)] [predicates (listof string?)]  [decisions (listof string?)]  [reqvars (listof string?)] [othvars (listof string?)]))
  

;****************************************************************

; Initialize policy file name.
; This is used by load-policy to assure we don't need to change
; the working directory.
; see normalize-url
(define local-policy-filename ".")

;****************************************************************

; policy-file-name -> list(pname, vname, list-of-commands-for-vocab, list-of-commands-for-policy, list(child-pname))
; The order of the children in their lists respects dependency: children-of-children appear before children, etc.
(define (evaluate-policy raw-fn 
                         #:syntax [src-syntax #f])
  
  ; If fn begins with *MARGRAVE*, replace with the path of the Margrave collections folder.
  (define fn (resolve-margrave-filename-keyword raw-fn))
      
  ;; Macro returns a func of two arguments:
  ; (1) the filename of the policy (for constructing the location of the vocab file)
  ; (2) the command syntax that started evaluation (for nice error highlighting)
  
  (parameterize ([read-case-sensitive #t])
    (define file-port (open-input-file/exists fn src-syntax (format "Could not find the policy file: ~a~n" fn)))
    (port-count-lines! file-port)
    
    (define the-policy-syntax (read-syntax fn file-port))    
    ; Don't convert to datum before evaluating, or the Policy macro loses location info
    (define the-policy-func (eval the-policy-syntax margrave-policy-vocab-namespace))    
    (define pol-result-list (the-policy-func fn src-syntax))
        
    (close-input-port file-port)        
    pol-result-list))



;****************************************************************

(define-for-syntax (syntax->string/safe x)
  (cond [(syntax? x) (symbol->string/safe (syntax->datum x))]
        [(symbol? x) (symbol->string x)]        
        [else x]))

(define-for-syntax (assert-one-clause stx clause-list descriptor)
  (match clause-list
    [(list) (raise-syntax-error #f (format "~a clause is missing" descriptor) stx)]
    [(list x y z ...) (raise-syntax-error 'Margrave (format "More than one ~a clause found" descriptor) #f #f (list* x y z))]
    [_ (void)]))         

(define-for-syntax (assert-lone-clause stx clause-list descriptor)
  (match clause-list
    [(list x y z ...) (raise-syntax-error 'Margrave (format "More than one ~a clause found" descriptor) #f #f (list* x y z))]
    [_ (void)]))  

;(define-for-syntax (none-colon-syn? syn-list)
;  (define lst (if (syntax? syn-list)
;                  (syntax->datum syn-list)
;                  syn-list))
;  (cond [(empty? lst) #t]
;        [(equal? (first lst) ':) #f] ; syntax->datum done above (recursively)
;        [else (none-colon-syn? (rest lst))]))

(define-for-syntax (id-syn? syn)
  (define dat (if (syntax? syn)
                  (syntax->datum syn)
                  syn))  
  (symbol? dat))


; Is this a capitalized id?
; (Sorts)
(define-for-syntax (capitalized-id-syn? syn)
  (define dat (if (syntax? syn)
                  (syntax->datum syn)
                  syn))  
  (and (symbol? dat)
       (char-upper-case? (string-ref (symbol->string dat) 0))))

; Is the first character lowercase? 
; (Variables, functions, predicates...)
(define-for-syntax (lower-id-syn? syn)
  (define dat (if (syntax? syn)
                  (syntax->datum syn)
                  syn))  
  (and (symbol? dat)
       (char-lower-case? (string-ref (symbol->string dat) 0))))

; Apply given pred to each
; (to be used on lists of identifiers)
(define-for-syntax (all-are-syn? syn-list fpred)
  (define proper-list (or (syntax->list syn-list)
                          (list (syntax->datum syn-list))))
  (andmap fpred proper-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define-syntax (PolicySet stx)
;  (syntax-case stx [Children PComb]
;    ([_ clauses ...]
;     (let ()
;       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Children
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;(define the-child-policies
       ;  (if (empty? the-children-clauses)
       ;      empty
       ;      (let ()    
       ;        (define the-children-clause (first the-children-clauses))
       ;        (define the-children (rest (syntax-e the-children-clause)))               
       ;        
       ;        ; Let the macro system expand these
       ;        the-children))) 

;       
;       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (Vocab stx)
  (syntax-case stx [Types Predicates Constants Functions Vocab]
    ([_ myvocabname clauses ...]     
                   
     (let ()
       (define vocab-name-syntax #'myvocabname)
       (define vocab-name (syntax->datum vocab-name-syntax))
       
       (unless (symbol? vocab-name)
         (raise-syntax-error 'PolicyVocab (format "Expected a name for the vocabulary, got: ~a" vocab-name) #f #f (list vocab-name-syntax)))
       
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Types Predicates Constants Functions]
                                                        [(Types atypedec ...) 'types]                                   
                                                        [(Predicates apreddec ...) 'predicates] 
                                                        [(Constants aconstdec ...) 'constants] 
                                                        [(Functions afuncdec ...) 'functions] 
                                                        [_ #f]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(types predicates constants functions)))
       
      ; (printf "Vocab syntax: ~a~n" stx)
      ; (printf "Clause list: ~a~n" (syntax->list #'(clauses ...)))
      ;(printf "Clause table: ~n~a~n" clause-table)
       
       ; from gmarceau
       ;(define-syntax (syntax-case-match? stx)
       ;  [(_ v lits pattern) #'(syntax-case v (Types Decisions Predicates ReqVariables OthVariables Constraints) [pattern #t] [_ #f])])       
       ;(define the-types-clauses (filter (lambda (v) (syntax-case-match? v (Types (t subt ...) ...)) clauses)))
       
       (define the-types-clauses (hash-ref clause-table 'types)) 
       (define the-predicates-clauses (hash-ref clause-table 'predicates))
       (define the-constants-clauses (hash-ref clause-table 'constants))
       (define the-functions-clauses (hash-ref clause-table 'functions))
                     
       (assert-one-clause stx the-types-clauses "Types")
       (assert-lone-clause stx the-predicates-clauses "Predicates")
       (assert-lone-clause stx the-constants-clauses "Constants")
       (assert-lone-clause stx the-functions-clauses "Functions")     
             
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Types
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define the-types-clause (first the-types-clauses))
       (define the-types (rest (syntax-e the-types-clause)))
       
       ; Returns a pair (name cmd)
       (define (handle-type a-type)         
         (syntax-case a-type [Type >]     
           
           ; (Type T)
           [(Type t) (capitalized-id-syn? #'t)
                     (make-type-command-syntax (syntax->datum #'t))]  
           
           [(Type t) (not (capitalized-id-syn? #'t))
                     (raise-syntax-error 'Vocab "Type declaration was not valid: Types must have capitalized names." #f #f (list a-type))] 
           ; (Type T > A B C)
           [(Type t > subt ...) (and (capitalized-id-syn? #'t)
                                (all-are-syn? #'(subt ...) capitalized-id-syn?))                                
                                (make-type-command-syntax (syntax->datum #'t)
                                                          (syntax-e #'(subt ...)))] 
           [(Type t > subt ...) (not 
                                 (and (capitalized-id-syn? #'t)
                                      (all-are-syn? #'(subt ...) capitalized-id-syn?)))
                                (raise-syntax-error
                                 'Vocab "Type declaration was not valid: Types must have capitalized names." #f #f (list a-type)) ] 
           
           [_ (raise-syntax-error 'Vocab "Type declaration was not valid." #f #f (list a-type))]))
                     
       (define (make-type-command-syntax typename [child-list empty])
         (define typenamestr (syntax->string/safe typename))
         (cond [(empty? child-list)            
                #`(#,typenamestr
                   #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name))
                                                (xml-make-sort (symbol->string typename)))))]
               [else                 
                (define childstrlist (map syntax->string/safe child-list))
                (define alltypeslist (append (list typenamestr) childstrlist))
                #`(#,alltypeslist
                   #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                   (xml-make-type-with-subs (symbol->string typename) childstrlist))))]))
              
       (define types-result (map handle-type the-types)) 
       (define types-cmds (map (compose second syntax->datum) types-result))
       (define types-names (flatten (map (compose first syntax->datum) types-result)))
       
       ; Used later to recognize a type name that wasn't declared.
       (define (is-valid-type? typename-syn)
         (define type-name-str (symbol->string/safe (syntax->datum typename-syn)))
         (unless (member type-name-str types-names)
           (raise-syntax-error 'Vocab 
                               (format "Invalid declaration. The type ~a was not declared." type-name-str)
                               #f #f (list typename-syn) ))
         #t)
       
       
       (define (each-type-in-list-is-valid? typelist-syntax)
         (define type-syn-list (syntax->list typelist-syntax))
         (andmap is-valid-type? type-syn-list))
             
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Predicates
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       ; Optional clause; allow to be empty.
       (define predicates-result
         (if (empty? the-predicates-clauses)
             empty
             (let ()
               (define the-predicates-clause (first the-predicates-clauses))
               (define the-predicates (rest (syntax-e the-predicates-clause)))
               
               (define (add-predicate vocab-syn predname-syn listrels-syn)
                 (xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum vocab-syn))) 
                                               (xml-make-predicate (symbol->string (syntax->datum predname-syn))) 
                                               (xml-make-relations-list (map symbol->string (syntax->datum listrels-syn))))))
                 
               
               (define (handle-predicate pred)
                 ; No colon. Just (Predicate predname A B C)
                 (syntax-case pred []                   
                   [(Predicate pname prel0 prel ...) (and (lower-id-syn? #'pname)
                                                (each-type-in-list-is-valid? #'(prel0 prel ...)))  
                                           #`( (pname
                                                #,(length (syntax->list #'(prel0 prel ...))))
                                               #,(add-predicate  #'myvocabname #'pname #'(prel0 prel ...)))] 
                   
                   [_ (raise-syntax-error 'Vocab "Invalid predicate declaration." #f #f (list pred) )]))
               
               (map handle-predicate the-predicates))))

       ; may be empty
       (define predicates-names-and-arities (if (empty? predicates-result)
                                            empty
                                            (map (compose (lambda (ppair) (list (symbol->string/safe (first ppair)) (second ppair)))
                                                          first 
                                                          syntax->datum) predicates-result)))
       
       (define predicates-cmds (if (empty? predicates-result)
                                           empty
                                           (map (compose second syntax->datum) predicates-result)))
       
      ; (printf "predicates-result: ~a~n" predicates-result)  
       ;(printf "~a ... ~a ~n" predicates-names predicates-cmds)   
              
       ; Used later to discover whether a pred is being used properly
       (define (is-valid-pred? predname-syn [desired-arity #f])
         (define pred-name-str (symbol->string/safe (syntax->datum predname-syn)))   
         ; todo: There's a better func than filter for this. what was it called? -TN
         (define found-pred-pair (filter (lambda (pair) (equal? (first pair) pred-name-str)) predicates-names-and-arities))
         
         (when (empty? found-pred-pair)
           (raise-syntax-error 'Vocab 
                               (format "Invalid declaration. The predicate ~a was not declared." pred-name-str)
                               #f #f (list predname-syn) ))    
                           
         (define the-arity (second (first found-pred-pair)))         
         (unless (or (not desired-arity) 
                     (equal? desired-arity the-arity))
                (raise-syntax-error 'Vocab 
                                    (format "Invalid declaration. The predicate ~a was declared, but had arity ~a which was not ~a." 
                                            pred-name-str the-arity desired-arity)
                                    #f #f (list predname-syn) ))               
         #t)


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Constants
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              
       
       ; Optional clause; allow to be empty.
       (define constants-result
         (if (empty? the-constants-clauses)
             empty
             (let ()
               (define the-constants-clause (first the-constants-clauses))
               (define the-constants (rest (syntax-e the-constants-clause)))
               
               (define (add-constant vocab-syn constname-syn type-syn)
                 (xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum vocab-syn))) 
                                               (xml-make-constant (symbol->string (syntax->datum constname-syn)) 
                                                                  (symbol->string (syntax->datum type-syn))))))
                 
               
               (define (handle-constant const)
                 ; No colon. Just (Constant a A)
                 (syntax-case const [Constant]                    
                   [(Constant cname crel) (and (lower-id-syn? #'cname)
                                      (is-valid-type? #'crel))  
                                           #`( cname
                                               #,(add-constant #'myvocabname #'cname #'crel))] 
                   [(Constant cname crel) (not (lower-id-syn? #'cname))
                                 (raise-syntax-error 'Vocab "Invalid constant declaration. Constant identifiers must begin with a lowercase letter." #f #f (list const) )]     
                   
                   [_ (raise-syntax-error 'Vocab "Invalid constant declaration." #f #f (list const) )]))
               
               (map handle-constant the-constants))))

       ; may be empty
       (define constants-names (if (empty? constants-result)
                                   empty
                                   (map (compose symbol->string/safe first syntax->datum) constants-result)))      
       
       (define constants-cmds (if (empty? constants-result)
                                           empty
                                           (map (compose second syntax->datum) constants-result)))
       
      ; (printf "constants-result: ~a~n" constants-result)  
       ;(printf "~a ... ~a ~n" constants-names constants-cmds)   
              
       ; Used later to discover whether a constant is being used properly
       (define (is-valid-const? constname-syn)
         (define const-name-str (symbol->string/safe (syntax->datum constname-syn)))   
         ; todo: There's a better func than filter for this. what was it called? -TN
         (define found-const (filter (lambda (c) (equal? c const-name-str)) constants-names))
         
         (when (empty? found-const)
           (raise-syntax-error 'Vocab 
                               (format "Invalid declaration. The constant ~a was not declared." const-name-str)
                               #f #f (list constname-syn) ))                                        
         #t)


       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Functions
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       ; Optional clause; allow to be empty.
       (define functions-result
         (if (empty? the-functions-clauses)
             empty
             (let ()
               (define the-functions-clause (first the-functions-clauses))
               (define the-functions (rest (syntax-e the-functions-clause)))
               
               (define (add-function vocab-syn funcname-syn type-syn)
                 (xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum vocab-syn))) 
                                               (xml-make-function (symbol->string (syntax->datum funcname-syn)) 
                                                                  (map symbol->string (syntax->datum type-syn))))))
                 
               
               (define (handle-function func)
                 ; No colon. Just (function a A B)
                 (syntax-case func [Function]                   
                   [(Function fname frel0 frel ...) (and (lower-id-syn? #'fname)
                                                         (each-type-in-list-is-valid? #'(frel0 frel ...)))  
                                                    #`( (fname
                                                         #,(length (syntax->list #'(frel0 frel ...))))
                                                        #,(add-function  #'myvocabname #'fname #'(frel0 frel ...)))] 
                   
                   [_ (raise-syntax-error 'Vocab "Invalid function declaration." #f #f (list func) )]))
               
               (map handle-function the-functions))))

       ; may be empty
       (define functions-names-and-arities (if (empty? functions-result)
                                               empty
                                               (map (compose (lambda (ppair) (list (symbol->string/safe (first ppair)) (second ppair)))
                                                             first 
                                                             syntax->datum) functions-result)))  
       
       (define functions-cmds (if (empty? functions-result)
                                           empty
                                           (map (compose second syntax->datum) functions-result)))
       
      ; (printf "functions-result: ~a~n" functions-result)  
       ;(printf "~a ... ~a ~n" functions-names functions-cmds)   
              
       ; Used later to discover whether a function is being used properly
       (define (is-valid-func? funcname-syn [desired-arity #f])
         (define func-name-str (symbol->string/safe (syntax->datum funcname-syn)))   
         ; todo: There's a better func than filter for this. what was it called? -TN
         (define found-func-pair (filter (lambda (pair) (equal? (first pair) func-name-str)) functions-names-and-arities))         

         (when (empty? found-func-pair)
           (raise-syntax-error 'Vocab 
                               (format "Invalid declaration. The function ~a was not declared." func-name-str)
                               #f #f (list funcname-syn) ))   
         
         (define the-arity (second (first found-func-pair)))         
         (unless (or (not desired-arity) 
                     (equal? desired-arity the-arity))
                (raise-syntax-error 'Vocab 
                                    (format "Invalid declaration. The function ~a was declared, but had arity ~a which was not ~a." 
                                            func-name-str the-arity desired-arity)
                                    #f #f (list funcname-syn) ))       
         #t)

       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
       ; Return a list containing the vocabname, and then a list of commands to be sent to java
       ; We have no idea whether the vocabulary has been created yet or not. 
       ; Java will handle creation of the object if the identifier hasn't been seen before.
       ; Also include info on what types are valid, etc. for error generation in the policy macro
       
       (with-syntax ([xml-list (append types-cmds                                        
                                       predicates-cmds
                                       constants-cmds
                                       functions-cmds)]
                     [vocab-name (symbol->string vocab-name)]                     
                     [types-names types-names]
                     [predicates-names-and-arities predicates-names-and-arities]
                     [constants-names constants-names]
                     [functions-names-and-arities functions-names-and-arities])                  
         
         ; Note: make sure to test with '(Vocab ...), not (Vocab ...) or this will cause an error.
         (syntax/loc stx '(vocab-name xml-list types-names predicates-names-and-arities constants-names functions-names-and-arities)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Policy: Parses a policy definition and creates an MPolicyLeaf OR MPolicySet object
; Policies are permitted to have child policies, so this may be recursively called
  
(define-syntax (Policy stx)
  (syntax-case stx [Target Rules = :- uses RComb Policy Variables]
    [(_ policyname uses vocabname clauses ... )

     (let ()
       (unless (symbol? (syntax->datum #'policyname))
         (raise-syntax-error 'Policy (format "Expected a name for the policy, got: ~a" 
                                             (syntax->datum #'policyname)) #f #f (list #'policyname)))
       (unless (symbol? (syntax->datum #'vocabname))
         (raise-syntax-error 'Policy (format "Expected a vocabulary name for the policy to use, got: ~a"
                                             (syntax->datum #'vocabname)) #f #f (list #'vocabname)))
       
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Target Rules = :- RComb PComb Children Variables]
                                                        [(Variables vardec ...) 'variables]
                                                        [(Target targ ...) 'target]                   
                                                        [(Rules rule ...) 'rules]
                                                        [(RComb comb ...) 'rcomb] 
                                                        [_ #f]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(variables target rules rcomb)))
       
       (printf "Policy Clause list: ~a~n" (syntax->list #'(clauses ...)))
       (printf "Policy Clause table: ~n~a~n" clause-table)

       (define the-variables-clauses (hash-ref clause-table 'variables))                            
       (define the-target-clauses (hash-ref clause-table 'target))                                 
       (define the-rules-clauses (hash-ref clause-table 'rules))
       (define the-rcomb-clauses (hash-ref clause-table 'rcomb))

       (assert-one-clause stx the-variables-clauses "Variables")
       (assert-lone-clause stx the-target-clauses "Target")
       (assert-one-clause stx the-rules-clauses "Rules")
       (assert-lone-clause stx the-rcomb-clauses "RComb")       
       
       ; target is no longer just conj of literal fmlas
       ; is an actual fmla
       
       ; Takes formula syntax and returns XML for the formula.
       ; todo: detect valid vars, sorts, etc.
       (define (handle-formula fmla)         
        (syntax-case fmla [and or not implies iff exists forall = true] 
          [true (xml-make-true-condition)]
          [(= v1 v2) (xml-make-equals-formula #'v1 #'v2)]
          [(and f0 f ...) (xml-make-and (map handle-formula #'(f0 f ...)))]
          [(or f0 f ...) (xml-make-or (map handle-formula #'(f0 f ...)))]
          [(implies f1 f2) (xml-make-implies (handle-formula #'f1) (handle-formula #'f2))]
          [(iff f1 f2) (xml-make-iff (handle-formula #'f1) (handle-formula #'f2))]
          [(not f) (xml-make-not (handle-formula #'f))]
          [(exists f var type) (xml-make-exists (handle-formula #'f) #'var #'type)]
          [(forall f var type) (xml-make-forall (handle-formula #'f) #'var #'type)]
          
          ; last, so it doesn't supercede keywords
          [(dottedpred t0 t ...) (xml-make-atomic-formula #'dottedpred #'(t0 t ...)) ]

          [else (raise-syntax-error 'Policy "Invalid formula type." #f #f (list fmla))]))
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Variables
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define variables-result
         (if (empty? the-variables-clauses)
             empty
             (let ()    
               (define the-variables-clause (first the-variables-clauses))
               (define the-variables (rest (syntax-e the-variables-clause)))
                                             
               (define (handle-variable a-var-dec)
                 (syntax-case a-var-dec [Variable]
                   [(Variable lowid capid)                                         
                    (and (capitalized-id-syn? #'capid)
                         (lower-id-syn? #'lowid))
                    (xml-make-command "ADD" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                  (xml-make-vardec (syntax->datum #'lowid)
                                                                   (syntax->datum #'capid))))]
                   [_ (raise-syntax-error 'Policy "Invalid rule" #f #f (list a-var-dec))]))
                                             
               (map handle-variable the-variables))))
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Target
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define target-result
         (if (empty? the-target-clauses)
             empty
             (let ()               
               (define the-target-clause (first the-target-clauses))
               (define the-targets (rest (syntax-e the-target-clause)))
               (when (> (length the-targets) 1)
                 (raise-syntax-error 'Policy "Only one target formula can be given, but had more than one." #f #f (list the-target-clause)))
               (when (< (length the-targets) 1)
                 (raise-syntax-error 'Policy "The Target clause must contain a formula if it is given." #f #f (list the-target-clause)))
                            
               (define the-target (first the-targets))                                             
               (list (xml-make-command "SET TARGET FOR POLICY" 
                                       (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname)))
                                             (handle-formula the-target)))))))    
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Rules
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define rules-result
         (if (empty? the-rules-clauses)
             empty
             (let ()    
               (define the-rules-clause (first the-rules-clauses))
               (define the-rules (rest (syntax-e the-rules-clause)))
                              
               ;(define (is-valid-conjunction conj)
               ;  (syntax-case conj []
               ;    [(pred v0 v ...) #t]
               ;    [x (equal? 'true (syntax->datum #'x))]
               ;    [(x) (equal? 'true (syntax->datum #'x))]
               ;    [_ #f]))
               
               (define (handle-rule a-rule)
                 (syntax-case a-rule [= :-]
                   [(rulename = (decision rvar ...) :- fmla)                     
                    ; 'true is dealt with in the back-end.                     
                    
                    (xml-make-command "ADD" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                  (xml-make-rule (syntax->datum #'rulename)
                                                                 (xml-make-decision-type (syntax->datum #'decision)
                                                                                         (syntax->datum #'(rvar ...)))
                                                                 (handle-formula #'fmla))))]
                   [_ (raise-syntax-error 'Policy "Invalid rule" #f #f (list a-rule))]))
                                             
               (map handle-rule the-rules))))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; RComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define (handle-combine comb)
         (syntax-case comb [fa over]         
          [(fa dec0 dec ...) (xml-make-fa #'(dec0 dec ...))]
          [(over dec odec0 odec ...) (xml-make-over #'dec #'(odec0 odec ...))]
          [else (raise-syntax-error 'Policy "Invalid combination type. Must be (fa ...) or (over ...)" #f #f (list comb))]))
       
       (define the-rcomb-clause (first the-rcomb-clauses))
       (define rcomb-result 
         (syntax-case the-rcomb-clause [RComb]
           [(RComb x0 x ...) (xml-make-command "SET RCOMBINE FOR POLICY" 
                                               (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                     (xml-make-comb-list (map handle-combine (syntax->list #'(x0 x ...))))))]
           [_ (raise-syntax-error 'Policy "Invalid rule-combination clause." #f #f (list the-rcomb-clause))]))
         
       ;(printf "rcomb res: ~a~n" rcomb-result)             
       
; !!! How to make fa, over case insensitive?
       
       
       
       
       
       
       ;(printf "compile time children: ~a~n" the-child-policies)
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Create
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
     ;  (define create-result        
     ;    (if (< (length the-child-policies) 1)
     ;        (list
     ;         (xml-make-command "CREATE POLICY LEAF" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
     ;                                                      (xml-make-vocab-identifier (symbol->string (syntax->datum #'vocabname)))))
     ;         rcomb-result)
     ;        (list
     ;         (xml-make-command "CREATE POLICY SET" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname)))
     ;                                                    (xml-make-vocab-identifier (symbol->string (syntax->datum #'vocabname)))))
     ;         pcomb-result)))
      
       (define create-result (xml-make-command "CREATE POLICY LEAF" 
                                               (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname)))))) 
     
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Prepare
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define prepare-result 
            (xml-make-command "PREPARE" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))))))       
                        
       
       ; Macro returns a lambda that takes a filename and a syntax object.   
       ; This is so we know where to find the vocabulary file. Only know that at runtime
       (with-syntax ([my-commands (append create-result ; create must come first
                                          variables-result
                                          target-result
                                          rcomb-result
                                          rules-result)]
                     [my-prepare-command (list prepare-result)]
                    ; [the-child-policies #`(list #,@the-child-policies)]
                     [vocabname #'vocabname]
                     [policyname #'policyname]
                     
                     ; can't include Policy here or else it gets macro-expanded (inf. loop)
                     ; smuggle in location info and re-form syntax if we need to throw an error
                     [orig-stx-line #`#,(syntax-line stx)]
                     [orig-stx-column #`#, (syntax-column stx)]
                     [orig-stx-source #`#, (syntax-source stx)]
                     [orig-stx-position #`#, (syntax-position stx)]
                     [orig-stx-span #`#, (syntax-span stx)])     
        ; (printf "before sloc:~a~n" #'orig-stx)
         
         (syntax/loc stx 
           ; Don't quote the lambda. Un-necessary (and would force evaluate-policy to double-eval)
           (lambda (local-policy-filename src-syntax)                                                                        
              (define vocab-path (build-path (path-only/same local-policy-filename) 
                                             (string-append (symbol->string 'vocabname) ".v")))
             
              ; Produce a friendly error if the vocab doesn't exist
              ; src-syntax here is the *command* that spawned the loading, not the Policy form.
              (file-exists?/error vocab-path src-syntax 
                                  (format "The policy's vocabulary did not exist. Expected: ~a" (path->string vocab-path)))   
              
              (define vocab-macro-return                 
                (call-with-input-file
                    vocab-path
                  (lambda (in-port) 
                    (port-count-lines! in-port)
                    (define the-vocab-syntax (read-syntax vocab-path in-port))               
                    ; Keep as syntax here, so the PolicyVocab macro gets the right location info
                    ; margrave-policy-vocab-namespace is provided to the module that evaluates the code we're generating here
                    (eval the-vocab-syntax margrave-policy-vocab-namespace))))  
              
             ; (vocab-name xml-list types-names predicates-names-and-arities constants-names functions-names-and-arities)
             (define vocab-name (first vocab-macro-return))
             (define vocab-commands (second vocab-macro-return))
              
              ; Get the 6-tuples for each child
              ;(define child-policy-macros the-child-policies) ; no '
             
              
              ; Each child gives us its own list (pname, vname, vlist, plist, (child-pol-pairs), (child-voc-pairs)
              ; Error out if there is a duplicate policy name (or the same name as this parent policy)
             ; Don't repeat vocabularies
             ;(define the-children (map (lambda (child)                                          
             ;                            ((eval child margrave-policy-vocab-namespace) 
             ;                             local-policy-filename
             ;                             src-syntax))
             ;                          child-policy-macros))
             
             (define (make-placeholder-syntax placeholder)
               (datum->syntax #f placeholder
                              (list 'orig-stx-source orig-stx-line orig-stx-column orig-stx-position orig-stx-span)))                                              
             
             ; take the child policy's tuple from evaluation
             ; get the list of pnames              
         ;     (define (get-child-pols tuple)
         ;       (when (or (< (length tuple) 5)
         ;                 (not (list? (fifth tuple))))
         ;         (raise-syntax-error #f (format "Internal error: result from policy child did not have expected form. Result was ~a~n" tuple) 
         ;                             (make-placeholder-syntax 'placeholder)))
         ;       (append (list (first tuple)) ; <-- child's pname
         ;               (fifth tuple)))      ; <-- list of child's children's pnames
         ;     
         ;    (define (get-add-child-xml tuple)
         ;      (xml-make-command "ADD" (list `(PARENT ,(xml-make-parent-identifier (symbol->string 'policyname))
         ;                                             ,(xml-make-child-identifier (first tuple))))))
             
          ;   (define my-commands-without-child 'my-commands)
             
             ; Forge the connection between parent and child policies
             ; and *prefix* this policy's commands with the child commands
             ; *FINALLY* prepare the policy.
          ;   (define child-commands (append* (map fourth the-children)))
          ;   (define my-commands-with-children (append child-commands
          ;                                             my-commands-without-child
          ;                                             (map get-add-child-xml the-children)
          ;                                             'my-prepare-command))                          
             
           ;  (define child-polnames (flatten (map get-child-pols the-children)))
             
             ; Throw error if duplicate policy name
         ;    (define (dup-pname-helper todo sofar)
         ;      (cond [(empty? todo) #f]
         ;            [(member (first todo) sofar)                      
         ;             (raise-syntax-error 'Policy 
         ;                                 "Policy name duplicated among children and parent. All policies in a hierarchy must have distinct names."; 
       ;                                   (make-placeholder-syntax (first todo)))]
       ;              [else (dup-pname-helper (rest todo) (cons (first todo) sofar))]))
             
        ;     (dup-pname-helper child-polnames (list (symbol->string 'policyname)))
              
             ; Don't double-load vocabs
             ; Even more, they must all be the same. Since we're in a fixed directory, check only the vname of each
     ;        (for-each (lambda (tuple)
     ;                    (unless (equal? (second tuple) (symbol->string 'vocabname))
     ;                      (raise-syntax-error 'Policy 
     ;                                          (format "Child policy used a vocabulary other than \"~a\". All children must have the same vocabulary as the parent." 'vocabname)                
     ;                                          (make-placeholder-syntax (second tuple)))))
     ;                  the-children)
                       
                          
              
              ; Return list(pname, vname, list-of-commands-for-vocab, list-of-commands-for-policy, list(child-pname, child-pcmds), list(child-vname, child-vcmds))
              `( ,(symbol->string 'policyname)
                 ,(symbol->string 'vocabname)
                 ,vocab-commands
                 ;,my-commands-with-children
                 ,my-commands
                 ;,child-polnames
                 )))))]

         
       
      
 
    
    [(_) (raise-syntax-error 'Policy "Empty policy specification not allowed." 
                            #f #f (list stx))]
    [(_ x) (raise-syntax-error 'Policy "Policy must supply both its name and the name of the vocabulary it uses." 
                               #f #f (list stx))]
    [(_ x y) (raise-syntax-error 'Policy "Policy must supply both its name and the name of the vocabulary it uses. (e.g. Policy mypolicy uses myvocabulary ...)" 
                                 #f #f (list stx))]
    [(_ x0 x1 x ...) (and (not (equal? (syntax->datum #'x0) 'uses))
                          (not (equal? (syntax->datum #'x1) 'uses)))
                     (raise-syntax-error 'Policy "Policy must supply both its name and the name of the vocabulary it uses. (The uses keyword may be missing between policy and vocabulary name.)" 
                                         #f #f (list stx))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(expand (Vocab myvoc (Types (Type X ) (Type Y) (Type Z > A B C)) (Constants (Constant c A) (Constant c2 X)) (Functions (Function f1 A B) (Function f2 X Y Z)) (Predicates (Predicate r X Y))))
;(expand-once '(Policy polname uses vocabname (Variables )(RComb (fa Permit Deny)) (Rules (Rule1 = (Permit x y z) :- true))))
; Why not case-insensitive in match?