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

#lang racket
(require margrave/margrave-xml
         margrave/helpers         
         (for-syntax margrave/helpers
                     margrave/margrave-xml
                     racket/list
                     racket/match))

(provide evaluate-policy
         
         ; for test cases
         Policy
         PolicyVocab)

; We use eval to load policies and vocabularies, and the call is in the definitions window.
; Thus we need to provide a namespace for eval, or it won't know what to do with the Policy
; and PolicyVocab syntax.
(define-namespace-anchor margrave-policy-vocab-namespace-anchor)
(define margrave-policy-vocab-namespace (namespace-anchor->namespace margrave-policy-vocab-namespace-anchor))

;****************************************************************

; Initialize policy file name.
; This is used by load-policy to assure we don't need to change
; the working directory.
; see normalize-url
(define local-policy-filename ".")

;****************************************************************

; policy-file-name -> list(pname, vname, list-of-commands-for-vocab, list-of-commands-for-policy)
(define (evaluate-policy raw-fn 
                         #:syntax [src-syntax #f])
  
  ; If fn begins with <MARGRAVE>, replace with the path of the Margrave collections folder.
  (define fn (resolve-margrave-filename-keyword raw-fn))
      
  ;; Macro returns a func of two arguments:
  ; (1) the filename of the policy (for constructing the location of the vocab file)
  ; (2) the command syntax that started evaluation (for nice error highlighting)
  
  (parameterize ([read-case-sensitive #t])
    (define file-port (open-input-file/exists fn src-syntax (format "Could not find the policy file: ~a~n" fn)))
    (port-count-lines! file-port)
    
    (define the-policy-syntax (read-syntax fn file-port))    
    (define the-policy-func (eval (syntax->datum the-policy-syntax) margrave-policy-vocab-namespace))    
    (define pol-result-list (the-policy-func fn src-syntax))
        
    (close-input-port file-port)    
    
    ; Return the script needed to create this policy
    pol-result-list))



;****************************************************************

(define-for-syntax (assert-one-clause stx clause-list descriptor)
  (match clause-list
    [(list) (raise-syntax-error #f (format "~a clause is missing" descriptor) stx)]
    [(list x y z ...) (raise-syntax-error 'Margrave (format "More than one ~a clause found" descriptor) #f #f (list* x y z))]
    [_ (void)]))         

(define-for-syntax (assert-lone-clause stx clause-list descriptor)
  (match clause-list
    [(list x y z ...) (raise-syntax-error 'Margrave (format "More than one ~a clause found" descriptor) #f #f (list* x y z))]
    [_ (void)]))  

(define-for-syntax (none-colon-syn? syn-list)
  (define lst (if (syntax? syn-list)
                  (syntax->datum syn-list)
                  syn-list))
  (cond [(empty? lst) #t]
        [(equal? (first lst) ':) #f] ; syntax->datum done above (recursively)
        [else (none-colon-syn? (rest lst))]))

(define-for-syntax (all-id-syn? syn-list)
  (define lst (if (syntax? syn-list)
                  (syntax->datum syn-list)
                  syn-list))
  (cond [(empty? lst) #t]
        [(not (symbol? (first lst))) #f] ; syntax->datum done above (recursively)
        [else (all-id-syn? (rest lst))]))

; PolicyVocab: Parses a vocabulary definition and creates an MVocab object
(define-syntax (PolicyVocab stx)
  (syntax-case stx [Types Decisions Constraints Predicates Variables : PolicyVocab]
    ([_ myvocabname clauses ...]     
         
     (let ()
       (unless (symbol? (syntax->datum #'myvocabname))
         (raise-syntax-error 'PolicyVocab (format "Expected a name for the vocabulary, got: ~a" (syntax->datum #'myvocabname)) #f #f (list #'myvocabname)))
       
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Types Decisions Predicates ReqVariables OthVariables Constraints Types:]
                                                        [(Types atype ...) 'types]                   
                                                        [(Types: atype ...) 'types]                   
                                                        [(Decisions r ...) 'decisions]
                                                        [(Predicates apreddec ...) 'predicates] 
                                                        [(ReqVariables avardec ...) 'reqvariables] 
                                                        [(OthVariables avardec ...) 'othvariables] 
                                                        [(Constraints acondec ...) 'constraints] 
                                                        [_ #f]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(types decisions predicates reqvariables othvariables constraints)))
       
      ; (printf "Clause list: ~a~n" (syntax->list #'(clauses ...)))
      ; (printf "Clause table: ~n~a~n" clause-table)
       
       ; from gmarceau
       ;(define-syntax (syntax-case-match? stx)
       ;  [(_ v lits pattern) #'(syntax-case v (Types Decisions Predicates ReqVariables OthVariables Constraints) [pattern #t] [_ #f])])       
       ;(define the-types-clauses (filter (lambda (v) (syntax-case-match? v (Types (t subt ...) ...)) clauses)))
       
       (define the-types-clauses (hash-ref clause-table 'types))                                 
       (define the-decisions-clauses (hash-ref clause-table 'decisions))
       (define the-predicates-clauses (hash-ref clause-table 'predicates))
       (define the-reqvariables-clauses (hash-ref clause-table 'reqvariables))
       (define the-othvariables-clauses (hash-ref clause-table 'othvariables))
       (define the-constraints-clauses (hash-ref clause-table 'constraints))
                     
       (assert-one-clause stx the-types-clauses "Types")
       (assert-one-clause stx the-decisions-clauses "Decisions")
       (assert-lone-clause stx the-predicates-clauses "Predicates")
       (assert-one-clause stx the-reqvariables-clauses "ReqVariables")
       (assert-lone-clause stx the-othvariables-clauses "OthVariables")
       (assert-lone-clause stx the-constraints-clauses "Constraints")
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Types
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define (handle-top-types types-list)
         ; Optional :
         ; must have at least one type
         ; (Types : atype0 atype ...)
         ; (Types atype0 atype ...)         
         (syntax-case types-list [Types : Types:]
           [(Types : x0 x ...) (and (not (equal? (syntax->datum #'x0) ':))
                                    (none-colon-syn? #'(x ...)))
                               (map (lambda (t) (handle-type t #f)) 
                                    (append (list #'x0) (syntax->list #'(x ...))))]
                      
           [(Types: x0 x ...) (and (not (equal? (syntax->datum #'x0) ':))
                                   (none-colon-syn? #'(x ...)))
                              (map (lambda (t) (handle-type t #f)) 
                                   (append (list #'x0) (syntax->list #'(x ...))))]
           
           [(Types x0 x ...) (and (not (equal? (syntax->datum #'x0) ':))
                                  (none-colon-syn? #'(x ...)))
                             (map (lambda (t) (handle-type t #f)) (append (list #'x0) (syntax->list #'(x ...))))]   
           
           [_ (raise-syntax-error 'PolicyVocab "Top-level type declaration was not valid." #f #f types-list)]))
       
       (define (handle-type a-type [parent #f])
         ; (id!=: : atype ...)
         ; (id!=: atype ...)
         ; id!=:
         (syntax-case a-type [:]     
           [(t subt ...) (and (not (equal? (syntax->datum #'t) ':))
                              (none-colon-syn? #'(subt ...)))
                         (list (make-type-command-syntax (syntax->datum #'t) parent)
                               (handle-sub-types (syntax->list #'(subt ...)) (syntax->datum #'t)))] 
           [(t : subt ...) (and (not (equal? (syntax->datum #'t) ':))
                                (none-colon-syn? #'(subt ...)))
                           (list (make-type-command-syntax (syntax->datum #'t) parent) 
                                 (handle-sub-types (syntax->list #'(subt ...)) (syntax->datum #'t)))]
           
           [t (not (equal? (syntax->datum #'t) ':))
              (make-type-command-syntax (syntax->datum #'t) parent)]
           
           [_ (raise-syntax-error 'PolicyVocab "Type declaration was not valid." #f #f #'(a-type))]))
              
       
       (define (handle-sub-types types-list [parent #f])
         (syntax-case types-list [:]
           [() (raise-syntax-error 'PolicyVocab "Type declaration was not valid. Expected nonempty set of types." #f #f types-list)]
           
           [(x0 x ...) (and (not (equal? (syntax->datum #'x0) ':))
                            (none-colon-syn? #'(x ...)))
                       (map (lambda (t) (handle-type t parent)) types-list)]
           
           [(x0 : x ...) (and (not (equal? (syntax->datum #'x0) ':))
                              (none-colon-syn? #'(x ...)))
                         (map (lambda (t) (handle-type t parent)) types-list)]
           
           [_ (raise-syntax-error 'PolicyVocab "Type declaration was not valid." #f #f types-list)]))
       
       (define (make-type-command-syntax typename parent)
         (if parent             
             #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname))) 
                                               (xml-make-subsort (symbol->string parent) (symbol->string typename))))
             #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname)))
                                               (xml-make-sort (symbol->string typename))))))
       
       ; Is each type valid?  
       (define types-result (flatten (handle-top-types (syntax-e (first the-types-clauses)))))
       ;(printf "types-result: ~a~n" types-result)        
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Decisions
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-decisions-clause (first the-decisions-clauses))
       
       (define my-decisions (match (syntax-e the-decisions-clause)
                              [(list _) (raise-syntax-error 'PolicyVocab "Must have at least one decision." #f #f (list (first the-decisions-clauses)) )]
                              [(list _ dec ...) dec]
                              [_ (raise-syntax-error 'PolicyVocab "Invalid Decisions clause." #f #f (list (first the-decisions-clauses)) )])) 
       
       (define decisions-result (map (lambda (dec-syn) 
                                       #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname))) 
                                                                         (xml-make-decision (symbol->string (syntax->datum dec-syn)))))) 
                                     my-decisions))
       
        ; (printf "decisions-result: ~a~n" decisions-result)  
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Predicates
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                 ; Optional : again
                 (syntax-case pred [ : ]
                   [(pname : prel0 prel ...) (and (not (equal? (syntax->datum #'prel0) ':))
                                                  (none-colon-syn? #'(prel ...)))                                             
                                             #`#,(add-predicate  #'myvocabname #'pname #'(prel0 prel ...))]
                   
                   
                   [(pname prel0 prel ...) (and (not (equal? (syntax->datum #'prel0) ':))
                                                (none-colon-syn? #'(prel ...)))  
                                           #`#,(add-predicate  #'myvocabname #'pname #'(prel0 prel ...))] 
                   
                   [_ (raise-syntax-error 'PolicyVocab "Invalid predicate declaration." #f #f (list pred) )]))
               
               (map handle-predicate the-predicates))))
         
      ; (printf "predicates-result: ~a~n" predicates-result)  
      
              
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; ReqVariables
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-reqvariables-clause (first the-reqvariables-clauses))
       (define the-reqvardecs (rest (syntax-e the-reqvariables-clause)))
       
       (when (empty? the-reqvardecs)
         (raise-syntax-error 'PolicyVocab "There must be at least one request field." #f #f (list the-reqvariables-clause) ))
       
       (define (handle-reqvardec vardec)
         ; Optional : again
         ; only one sort per variable
         (syntax-case vardec [ : ]
           [(varname : varsort) (and (not (equal? (syntax->datum #'varname) ':))
                                     (not (equal? (syntax->datum #'varsort) ':)))  
                                #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname))) 
                                                                  (xml-make-request-var (symbol->string (syntax->datum #'varname)) 
                                                                                        (symbol->string (syntax->datum #'varsort)))))]
           
           [(varname varsort) (and (not (equal? (syntax->datum #'varname) ':))
                                   (not (equal? (syntax->datum #'varsort) ':)))  
                              #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname))) 
                                                                (xml-make-request-var (symbol->string (syntax->datum #'varname)) 
                                                                                      (symbol->string (syntax->datum #'varsort)))))]
           
           [_ (raise-syntax-error 'PolicyVocab "Invalid request field declaration." #f #f (list vardec) )]) )
                           
       (define reqvariables-result (map handle-reqvardec the-reqvardecs))
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; OthVariables
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define othvariables-result 
         (if (empty? the-othvariables-clauses)
             empty
             (let ()
               (define the-othvariables-clause (first the-othvariables-clauses))
               (define the-othvardecs (rest (syntax-e the-othvariables-clause)))
               
               (define (handle-othvardec vardec)
                 ; Optional : again
                 ; only one sort per variable
                 (syntax-case vardec [ : ]
                   [(varname : varsort) (and (not (equal? (syntax->datum #'varname) ':))
                                             (not (equal? (syntax->datum #'varsort) ':)))  
                                        #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname))) 
                                                                          (xml-make-other-var (symbol->string (syntax->datum #'varname)) 
                                                                                              (symbol->string (syntax->datum #'varsort)))))]
                   
                   [(varname varsort) (and (not (equal? (syntax->datum #'varname) ':))
                                           (not (equal? (syntax->datum #'varsort) ':)))  
                                      #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname))) 
                                                                        (xml-make-other-var (symbol->string (syntax->datum #'varname)) 
                                                                                            (symbol->string (syntax->datum #'varsort)))))]

                   [_ (raise-syntax-error 'PolicyVocab "Invalid rule-scope variable declaration." #f #f (list vardec) )]) )
                           
               (map handle-othvardec the-othvardecs))))
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Constraints
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define constraints-result
         
         (if (empty? the-constraints-clauses)
             empty
             (let ()
               (define the-constraints-clause (first the-constraints-clauses))
               (define the-constraints (rest (syntax-e the-constraints-clause)))
               
               (define (make-constraint vocname-syn ctype-str list-of-args-syn)
                 #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum vocname-syn))) 
                                                   (xml-make-constraint ctype-str
                                                                        (map syntax->datum list-of-args-syn)))))              
               
               (define (handle-constraint constraint)
                 (syntax-case constraint [disjoint atmostone singleton abstract nonempty 
                                                   disjoint-all atmostone-all singleton-all abstract-all nonempty-all
                                                   total-function partial-function subset]
                   
                   [(disjoint s1 s2) (and (symbol? (syntax->datum #'s1))
                                          (symbol? (syntax->datum #'s2))) 
                                     (make-constraint #'myvocabname "DISJOINT" (list #'s1 #'s2))]
                   [(atmostone s) (symbol? (syntax->datum #'s))
                                  (make-constraint #'myvocabname "ATMOSTONE" (list #'s))]
                   [(singleton s) (symbol? (syntax->datum #'s))
                                  (make-constraint #'myvocabname "SINGLETON" (list #'s))]
                   [(abstract s) (symbol? (syntax->datum #'s))
                                 (make-constraint #'myvocabname "ABSTRACT" (list #'s))]
                   [(nonempty s) (symbol? (syntax->datum #'s))
                                 (make-constraint #'myvocabname "NONEMPTY" (list #'s))]
                                                         
                   [(disjoint-all s) (symbol? (syntax->datum #'s))
                                     (make-constraint #'myvocabname "DISJOINT-ALL" (list #'s))]
                   [(atmostone-all s) (symbol? (syntax->datum #'s))
                                      (make-constraint #'myvocabname "ATMOSTONE-ALL" (list #'s))]
                   [(singleton-all s) (symbol? (syntax->datum #'s))
                                      (make-constraint #'myvocabname "SINGLETON-ALL" (list #'s))]
                   [(abstract-all s) (symbol? (syntax->datum #'s))
                                     (make-constraint #'myvocabname "ABSTRACT-ALL" (list #'s))]
                   [(nonempty-all s) (symbol? (syntax->datum #'s))
                                     (make-constraint #'myvocabname "NONEMPTY-ALL" (list #'s))]
                   
                   [(partial-function s) (symbol? (syntax->datum #'s))
                                         (make-constraint #'myvocabname "PARTIAL-FUNCTION" (list #'s))]
                   [(total-function s) (symbol? (syntax->datum #'s))
                                       (make-constraint #'myvocabname "TOTAL-FUNCTION" (list #'s))]
                   
                   [(subset s1 s2) (symbol? (syntax->datum #'s))
                                   #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string (syntax->datum #'myvocabname))) 
                                                                     (xml-make-subset (xml-make-parent-identifier (symbol->string (syntax->datum #'s1))) 
                                                                                      (xml-make-child-identifier (symbol->string (syntax->datum #'s2))))))]                   
                   
                   [_ (raise-syntax-error 'PolicyVocab "Invalid constraint declaration." #f #f (list constraint) )]))
               
               (map handle-constraint the-constraints))))

       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       ; TODO Add check for lexically valid type names 
       ; TODO Check that each sort reference is to a declared sort

       ; Return a list containing the vocabname, and then a list of commands to be sent to java
       ; We have no idea whether the vocabulary has been created yet or not. 
       ; Java will handle creation of the object if the identifier hasn't been seen before.
       
       (with-syntax ([xml-list (append types-result 
                                       decisions-result
                                       predicates-result
                                       reqvariables-result
                                       othvariables-result
                                       constraints-result)])
         (syntax/loc stx `( ,(symbol->string (syntax->datum #'myvocabname)) xml-list)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Policy: Parses a policy definition and creates an MPolicyLeaf OR MPolicySet object
; Policies are permitted to have child policies, so this may be recursively called

(define-syntax (Policy stx)
  ;(syntax-case stx [Target Rules = :- uses RComb PComb Children Policy]
  ;  ([_ policyname uses vocabname
  ;      (Target tconj ...) ; back end expects a list of conjuncts
  ;      (Rules (rulename = (dtype v ...) :- conj ...) ; for each variable in the IDB dec; for each conjunct in the rule itself                    
  ;             ...); for each rule
  ;      (RComb rcstr ...) ; rule combination alg?
  ;      (PComb pcstr ...) ; policy combination alg?
  ;      (Children child ...)] ; child policies? 
  
  (syntax-case stx [Target Rules = :- uses RComb PComb Children Policy]
    [(_ policyname uses vocabname clauses ... )

     (let ()
       (unless (symbol? (syntax->datum #'policyname))
         (raise-syntax-error 'Policy (format "Expected a name for the policy, got: ~a" (syntax->datum #'policyname)) #f #f (list #'policyname)))
       (unless (symbol? (syntax->datum #'vocabname))
         (raise-syntax-error 'Policy (format "Expected a vocabulary name for the policy to use, got: ~a" (syntax->datum #'vocabname)) #f #f (list #'vocabname)))
       
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Target Rules = :- RComb PComb Children]
                                                        [(Target targ ...) 'target]                   
                                                        [(Rules rule ...) 'rules]
                                                        [(RComb comb ...) 'rcomb] 
                                                        [(PComb comb ...) 'pcomb] 
                                                        [(Children child ...) 'children] 
                                                        [_ #f]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(target rules rcomb pcomb children)))
       
       (printf "Policy Clause list: ~a~n" (syntax->list #'(clauses ...)))
       (printf "Policy Clause table: ~n~a~n" clause-table)
              
       (define the-target-clauses (hash-ref clause-table 'target))                                 
       (define the-rules-clauses (hash-ref clause-table 'rules))
       (define the-rcomb-clauses (hash-ref clause-table 'rcomb))
       (define the-pcomb-clauses (hash-ref clause-table 'pcomb))
       (define the-children-clauses (hash-ref clause-table 'children))
                     
       (assert-lone-clause stx the-target-clauses "Target")
       (assert-lone-clause stx the-rules-clauses "Rules")
       (assert-one-clause stx the-rcomb-clauses "RComb")
       (assert-one-clause stx the-pcomb-clauses "PComb")
       (assert-lone-clause stx the-children-clauses "Children")
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Target
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define target-result
         (if (empty? the-target-clauses)
             empty
             (let ()               
               (define the-target-clause (first the-target-clauses))
               (define the-target (rest (syntax-e the-target-clause)))
                              
               (define (validate-target a-conjunct)
                 (syntax-case a-conjunct []
                   ; ! is part of the pred name token if present
                   [(predname x0 x ...) (all-id-syn? #'(predname x0 x ...)) 
                                        (syntax->datum #'(predname x0 x ...))]
                   [_ (raise-syntax-error 'Policy "Invalid target conjunct" #f #f (list a-conjunct))]))
               
               (if (> (length the-target) 0)
                   (list (xml-make-command "SET TARGET FOR POLICY" 
                                           (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname)))
                                                 (xml-make-conjunct-chain (map validate-target the-target)))))
                   empty))))
    
       (printf "~a~n" target-result)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Rules
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define rules-result
         (if (empty? the-rules-clauses)
             empty
             (let ()    
               (define the-rules-clause (first the-rules-clauses))
               (define the-rules (rest (syntax-e the-rules-clause)))
                              
               
               empty)))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; RComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-rcomb-clause (first the-rcomb-clauses))
       (define rcomb-result 
         (syntax-case the-rcomb-clause [RComb]
           [(RComb x ...) (xml-make-command "SET RCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                                           (xml-make-identifiers-list (map symbol->string (syntax->datum #'(x ...))))))]
           [_ (raise-syntax-error 'Policy "Invalid rule-combination algorithm" #f #f (list #'the-rcomb-clause))]))
         
       ;(printf "~a~n" rcomb-result)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; PComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-pcomb-clause (first the-pcomb-clauses))
       (define pcomb-result 
         (syntax-case the-pcomb-clause [PComb]
           [(PComb x ...) (xml-make-command "SET PCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                                            (xml-make-identifiers-list (map symbol->string (syntax->datum #'(x ...))))))]
           [_ (raise-syntax-error 'Policy "Invalid policy-combination algorithm" #f #f (list #'the-pcomb-clause))]))
       
       ;(printf "~a~n" pcomb-result)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Children
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define children-result
         (if (empty? the-children-clauses)
             empty
             (let ()    
               (define the-children-clause (first the-children-clauses))
               (define the-children (rest (syntax-e the-children-clause)))
               
               empty)))
       
       (define prepare-result 
            (xml-make-command "PREPARE" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))))))
       
       
       ; Macro returns a lambda that takes a filename and a syntax object.        
       (with-syntax ([xml-list #`(list #,@(append target-result
                                                 rules-result
                                                 rcomb-result
                                                 pcomb-result
                                                 children-result))]
                     [vocabname #'vocabname]
                     [policyname #'policyname])         
         (syntax/loc stx 
           `(lambda (local-policy-filename src-syntax) 
             (define vocab-path (build-path (path-only/same local-policy-filename) 
                                            (string-append (symbol->string vocabname) ".v")))
             
             ; Produce a friendly error if the vocab doesn't exist
             (file-exists?/error vocab-path src-syntax (format "The policy's vocabulary did not exist. Expected: ~a" (path->string vocab-path)))       
                          
             (define vocab-macro-return                 
               (call-with-input-file
                   vocab-path
                 (lambda (in-port) 
                   (port-count-lines! in-port)
                   (define the-vocab-syntax (read-syntax vocab-path in-port))               
                   (eval (syntax->datum the-vocab-syntax)   )))) ;  margrave-policy-vocab-namespace
             
             (define vocab-name (first vocab-macro-return))
             (define vocab-commands (second vocab-macro-return))
             
             ; Return a 4-tuple: policy name, vocab name, command list for vocab, and command list for policy                          
             ( ,(symbol->string 'policyname)
               ,(symbol->string 'vocabname)
               vocab-commands
               xml-list)))
         
         
       
       
     ; Return a function of one argument that can be called in the context of some local directory.
     ; This is so we know where to find the vocabulary file.
     #;(syntax/loc stx
       (lambda (local-policy-filename src-syntax) 
         
            ,(if (< (length mychildren) 1)
                 (xml-make-command "CREATE POLICY LEAF" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-vocab-identifier vocab-name)))
                 (xml-make-command "CREATE POLICY SET" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-vocab-identifier vocab-name))))
            
       
            
            ; Add the rules to the policy. 'true is dealt with in the back-end.         
            ,(add-rule (symbol->string 'policyname)
                      ;myvarorder 
                      (symbol->string 'rulename) (symbol->string 'dtype) (list (symbol->string 'v) ...) (list 'conj ...))
            ...
            
            
            ;; !!! TODO: confirm this works. are we loading the sub-policy properly?
            
            ; Each child is a Policy
            ,(let ((cpol child))
              (xml-make-command "ADD" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-policy-identifier cpol)))
              )
            ...
            
        
           ; close paren for above GET REQUEST VECTOR commented out )
           ))))]
    
    [(_) (raise-syntax-error 'Policy "Empty policy specification not allowed." 
                            #f #f (list stx))]
    [(_ x) (raise-syntax-error 'Policy "Policy must supply both its name and the name of the vocabulary it uses." 
                               #f #f (list stx))]
    [(_ x y) (raise-syntax-error 'Policy "Policy must supply both its name and the name of the vocabulary it uses." 
                                 #f #f (list stx))]
    [(_ x0 x1 x ...) (and (not (equal? (syntax->datum #'x0) 'uses))
                          (not (equal? (syntax->datum #'x1) 'uses)))
                     (raise-syntax-error 'Policy "Policy must supply both its name and the name of the vocabulary it uses. (The uses keyword may be missing between policy and vocabulary name.)" 
                                         #f #f (list stx))]))

; ********************************************************************
; Helper functions   


; Add a rule of the form rulename = (dtype reqvars) :- conjlist
(define (add-rule mypolicy rulename dtype reqvars conjlist)  
  (xml-make-command "ADD" (list (xml-make-policy-identifier mypolicy) (xml-make-rule rulename (xml-make-decision-type dtype) (xml-make-rule-list conjlist))))) 
