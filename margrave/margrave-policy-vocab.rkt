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

(require margrave/margrave-xml
         margrave/helpers         
         racket/list
         racket/contract
         (for-syntax margrave/helpers
                     margrave/margrave-xml
                     racket/list
                     racket/match
                     racket/string
                     racket/base
                     racket/contract))

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
  
  ; If fn begins with <MARGRAVE>, replace with the path of the Margrave collections folder.
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

(define-for-syntax (id-syn? syn)
  (define dat (if (syntax? syn)
                  (syntax->datum syn)
                  syn))  
  (symbol? dat))


(define-for-syntax (all-id-syn? syn-list)
  (define proper-list (or (syntax->list syn-list)
                          (list (syntax->datum syn-list))))
  (andmap id-syn? proper-list))


;****************************************************************

; PolicyVocab: Parses a vocabulary definition and creates an MVocab object
(define-syntax (PolicyVocab stx)
  (syntax-case stx [Types Decisions Constraints Predicates Variables : PolicyVocab]
    ([_ myvocabname clauses ...]     
                   
     (let ()
       (define vocab-name-syntax #'myvocabname)
       (define vocab-name (syntax->datum vocab-name-syntax))
       
       (unless (symbol? vocab-name)
         (raise-syntax-error 'PolicyVocab (format "Expected a name for the vocabulary, got: ~a" vocab-name) #f #f (list vocab-name-syntax)))
       
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
       
      ; (printf "Vocab syntax: ~a~n" stx)
      ; (printf "Clause list: ~a~n" (syntax->list #'(clauses ...)))
      ;(printf "Clause table: ~n~a~n" clause-table)
       
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
           ; (Length : )   <--- old syntax for subtype with no children
           [(t : ) (and (not (equal? (syntax->datum #'t) ':))
                        (id-syn? #'t))
                   (list (make-type-command-syntax (syntax->datum #'t) parent))]

           [(t) (and (not (equal? (syntax->datum #'t) ':))
                     (id-syn? #'t))
                (list (make-type-command-syntax (syntax->datum #'t) parent))]  
           
           [(t subt ...) (and (not (equal? (syntax->datum #'t) ':))
                              (none-colon-syn? #'(subt ...))
                              (id-syn? #'t))
                         (list (make-type-command-syntax (syntax->datum #'t) parent)
                               (handle-sub-types (syntax->list #'(subt ...)) (syntax->datum #'t)))] 
           
           [(t : subt ...) (and (not (equal? (syntax->datum #'t) ':))
                                (id-syn? #'t)
                                (none-colon-syn? #'(subt ...)))
                           (list (make-type-command-syntax (syntax->datum #'t) parent) 
                                 (handle-sub-types (syntax->list #'(subt ...)) (syntax->datum #'t)))]
           
           [t (and (not (equal? (syntax->datum #'t) ':))
                   (id-syn? #'t))
              (make-type-command-syntax (syntax->datum #'t) parent)]
           
           [_ (raise-syntax-error 'PolicyVocab "Type declaration was not valid." #f #f (list a-type))]))
              
       
       (define (handle-sub-types types-list [parent #f])
         (syntax-case types-list [:]           
           [(x0 x ...) (and (not (equal? (syntax->datum #'x0) ':))
                            (none-colon-syn? #'(x ...)))
                       (map (lambda (t) (handle-type t parent)) types-list)]
                      
           [_ (raise-syntax-error 'PolicyVocab "Type declaration was not valid." #f #f types-list)]))
       
       (define (make-type-command-syntax typename parent)
         (if parent             
             #`(#,(symbol->string typename)
                #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                (xml-make-subsort (symbol->string parent) (symbol->string typename)))))
             #`(#,(symbol->string typename)
                #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name))
                                                (xml-make-sort (symbol->string typename)))))))
       
       ; Is each type valid?  
       (define types-result (flatten (handle-top-types (syntax-e (first the-types-clauses)))))       
       (define types-cmds (map (compose second syntax->datum) types-result))
       (define types-names (map (compose first syntax->datum) types-result))
       
       (define (is-valid-type? typename-syn)
         (define type-name-str (symbol->string/safe (syntax->datum typename-syn)))
         (unless (member type-name-str types-names)
           (raise-syntax-error 'PolicyVocab 
                               (format "Invalid declaration. The type ~a was not declared." type-name-str)
                               #f #f (list typename-syn) ))
         #t)
       
       
       (define (each-type-in-list-is-valid typelist-syntax)
         (define type-syn-list (syntax->list typelist-syntax))
         (andmap is-valid-type? type-syn-list))
       
;       (printf "typen: ~a~n" types-names)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Decisions
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-decisions-clause (first the-decisions-clauses))
       
       (define my-decisions (match (syntax-e the-decisions-clause)
                              [(list _) (raise-syntax-error 'PolicyVocab "Must have at least one decision." #f #f (list (first the-decisions-clauses)) )]
                              [(list _ dec ...) (for-each (lambda (d) 
                                                            (when (equal? (syntax->datum d) ':)                                                              
                                                              (raise-syntax-error 'PolicyVocab "Invalid decisions clause. Colon is not a valid decision." #f #f (list (first the-decisions-clauses))))
                                                            (unless (symbol? (syntax->datum d))
                                                              (raise-syntax-error 'PolicyVocab "Invalid decisions clause." #f #f (list (first the-decisions-clauses)))))
                                                          dec)
                                                dec]
                              [_ (raise-syntax-error 'PolicyVocab "Invalid Decisions clause." #f #f (list (first the-decisions-clauses)) )])) 
       
       (define decisions-cmds (map (lambda (dec-syn) 
                                     #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                                       (xml-make-decision (symbol->string (syntax->datum dec-syn)))))) 
                                   my-decisions))

       (define decisions-names (map (compose symbol->string syntax->datum) my-decisions))
       
      ; (printf "dn: ~a~n" decisions-names)
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
                                                  (none-colon-syn? #'(prel ...))
                                                  (each-type-in-list-is-valid #'(prel0 prel ...)))                                             
                                             #`( (pname 
                                                  #,(length (syntax->list #'(prel0 prel ...))))
                                                 #,(add-predicate  #'myvocabname #'pname #'(prel0 prel ...))) ]
                   
                   
                   [(pname prel0 prel ...) (and (not (equal? (syntax->datum #'prel0) ':))
                                                (none-colon-syn? #'(prel ...))
                                                (each-type-in-list-is-valid #'(prel0 prel ...)))  
                                           #`( (pname
                                                #,(length (syntax->list #'(prel0 prel ...))))
                                               #,(add-predicate  #'myvocabname #'pname #'(prel0 prel ...)))] 
                   
                   [_ (raise-syntax-error 'PolicyVocab "Invalid predicate declaration." #f #f (list pred) )]))
               
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
              
       (define (is-valid-pred? predname-syn [desired-arity-lowest #f] [desired-arity-highest #f])
         (define pred-name-str (symbol->string/safe (syntax->datum predname-syn)))
         
         ;; there is a better func than filter, right? - todo
         (define found-pred-pair (filter (lambda (pair) (equal? (first pair) pred-name-str)) predicates-names-and-arities))
         
         (when (empty? found-pred-pair)
           (raise-syntax-error 'PolicyVocab 
                               (format "Invalid declaration. The predicate ~a was not declared." pred-name-str)
                               #f #f (list predname-syn) ))    
                  
         ; Check arity bounded on both sides
         (define the-arity (second (first found-pred-pair)))
         
         (cond [(and desired-arity-lowest 
                     desired-arity-highest
                     (or (> the-arity desired-arity-highest)
                         (< the-arity desired-arity-lowest)))
                (raise-syntax-error 'PolicyVocab 
                                    (format "Invalid declaration. The predicate ~a was declared, but had arity ~a which was not between ~a and ~a." 
                                            pred-name-str the-arity desired-arity-lowest desired-arity-highest)
                                    #f #f (list predname-syn) )]
               [(and desired-arity-lowest
                     (< the-arity desired-arity-lowest))
                (raise-syntax-error 'PolicyVocab 
                                    (format "Invalid declaration. The predicate ~a was declared, but had arity ~a which was lower than ~a." pred-name-str the-arity desired-arity-lowest)
                                    #f #f (list predname-syn) )]
               [(and desired-arity-highest
                     (> the-arity desired-arity-highest))
                (raise-syntax-error 'PolicyVocab 
                                    (format "Invalid declaration. The predicate ~a was declared, but had arity ~a which was higher than ~a." pred-name-str the-arity desired-arity-highest)
                                      #f #f (list predname-syn) )])  
         #t)

       
       
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
                                     (not (equal? (syntax->datum #'varsort) ':))
                                     (is-valid-type? #'varsort))  
                                #`(varname
                                   #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                                   (xml-make-request-var (symbol->string (syntax->datum #'varname)) 
                                                                                         (symbol->string (syntax->datum #'varsort))))))]
           
           [(varname varsort) (and (not (equal? (syntax->datum #'varname) ':))
                                   (not (equal? (syntax->datum #'varsort) ':))
                                   (is-valid-type? #'varsort))  
                              #`(varname 
                                 #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                                (xml-make-request-var (symbol->string (syntax->datum #'varname)) 
                                                                                      (symbol->string (syntax->datum #'varsort))))))]
           
           [_ (raise-syntax-error 'PolicyVocab "Invalid request field declaration." #f #f (list vardec) )]) )
                           
       (define reqvariables-result (map handle-reqvardec the-reqvardecs))
       (define reqvariables-cmds (map (compose second syntax->datum) reqvariables-result))
       (define reqvariables-names (map (compose first syntax->datum) reqvariables-result))
       
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
                                             (not (equal? (syntax->datum #'varsort) ':))
                                             (is-valid-type? #'varsort))  
                                        #`(varname
                                           #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                                          (xml-make-other-var (symbol->string (syntax->datum #'varname)) 
                                                                                              (symbol->string (syntax->datum #'varsort))))))]
                   
                   [(varname varsort) (and (not (equal? (syntax->datum #'varname) ':))
                                           (not (equal? (syntax->datum #'varsort) ':))
                                           (is-valid-type? #'varsort))  
                                      #`(varname
                                         #,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                                        (xml-make-other-var (symbol->string (syntax->datum #'varname)) 
                                                                                            (symbol->string (syntax->datum #'varsort))))))]

                   [_ (raise-syntax-error 'PolicyVocab "Invalid rule-scope variable declaration." #f #f (list vardec) )]) )
                           
               (map handle-othvardec the-othvardecs))))
       
       (define othvariables-names (if (empty? othvariables-result)
                                      empty
                                      (map (compose first syntax->datum) othvariables-result)))         
       (define othvariables-cmds (if (empty? othvariables-result)
                                     empty
                                     (map (compose second syntax->datum) othvariables-result)))
       
       
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
                                                   total-function partial-function total-relation subset]
                   
                   [(disjoint s1 s2) (and (is-valid-type? #'s1)
                                          (is-valid-type? #'s2)) 
                                     (make-constraint #'myvocabname "DISJOINT" (list #'s1 #'s2))]
                   [(atmostone s) (is-valid-type? #'s)
                                  (make-constraint #'myvocabname "ATMOSTONE" (list #'s))]
                   [(singleton s) (is-valid-type? #'s)
                                  (make-constraint #'myvocabname "SINGLETON" (list #'s))]
                   [(abstract s) (is-valid-type? #'s)
                                 (make-constraint #'myvocabname "ABSTRACT" (list #'s))]
                   [(nonempty s) (is-valid-type? #'s)
                                 (make-constraint #'myvocabname "NONEMPTY" (list #'s))]
                                                         
                   [(disjoint-all s) (is-valid-type? #'s)
                                     (make-constraint #'myvocabname "DISJOINT-ALL" (list #'s))]
                   [(atmostone-all s) (is-valid-type? #'s)
                                      (make-constraint #'myvocabname "ATMOSTONE-ALL" (list #'s))]
                   [(singleton-all s) (is-valid-type? #'s)
                                      (make-constraint #'myvocabname "SINGLETON-ALL" (list #'s))]
                   [(abstract-all s) (is-valid-type? #'s)
                                     (make-constraint #'myvocabname "ABSTRACT-ALL" (list #'s))]
                   [(nonempty-all s) (is-valid-type? #'s)
                                     (make-constraint #'myvocabname "NONEMPTY-ALL" (list #'s))]
                   
                   [(subset s1 s2) (and (is-valid-type? #'s2)
                                        (is-valid-type? #'s1))
                                   #`#,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string vocab-name)) 
                                                                     (xml-make-subset (xml-make-parent-identifier (symbol->string (syntax->datum #'s1))) 
                                                                                      (xml-make-child-identifier (symbol->string (syntax->datum #'s2))))))] 
                                      
                   
                   ; These are over BINARY PREDICATES instead of types. 
                   [(partial-function s) (is-valid-pred? #'s 2 #f)
                                         (make-constraint #'myvocabname "PARTIAL-FUNCTION" (list #'s))]
                   [(total-function s) (is-valid-pred? #'s 2 #f)
                                       (make-constraint #'myvocabname "TOTAL-FUNCTION" (list #'s))]
                   [(total-relation s) (is-valid-pred? #'s 2 #f)
                                       (make-constraint #'myvocabname "TOTAL-RELATION" (list #'s))]
                   
                   
                   [_ (raise-syntax-error 'PolicyVocab "Invalid constraint declaration." #f #f (list constraint) )]))
               
               (map handle-constraint the-constraints))))

       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
       ; Return a list containing the vocabname, and then a list of commands to be sent to java
       ; We have no idea whether the vocabulary has been created yet or not. 
       ; Java will handle creation of the object if the identifier hasn't been seen before.
       ; Also include info on what types are valid, etc. for error generation in the policy macro
                    
       (with-syntax ([xml-list (append types-cmds 
                                       decisions-cmds
                                       predicates-cmds
                                       reqvariables-cmds
                                       othvariables-cmds
                                       constraints-result)]
                     [vocab-name (symbol->string vocab-name)]                     
                     [types-names types-names]
                     [decisions-names decisions-names]
                     [predicates-names-and-arities predicates-names-and-arities]
                     [reqvariables-names reqvariables-names]
                     [othvariables-names othvariables-names])         
         (syntax/loc stx '(vocab-name xml-list types-names decisions-names predicates-names-and-arities reqvariables-names othvariables-names)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Policy: Parses a policy definition and creates an MPolicyLeaf OR MPolicySet object
; Policies are permitted to have child policies, so this may be recursively called
  
(define-syntax (Policy stx)
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
       
       ;(printf "Policy Clause list: ~a~n" (syntax->list #'(clauses ...)))
       ;(printf "Policy Clause table: ~n~a~n" clause-table)
              
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
                              
               (define (validate-target-to-str a-conjunct)
                 (syntax-case a-conjunct []
                   ; ! is part of the pred name token if present
                   [(predname x0 x ...) (all-id-syn? #'(predname x0 x ...)) 
                                        (string-join (map symbol->string (syntax->datum #'(predname x0 x ...))) " ")]
                   [_ (raise-syntax-error 'Policy "Invalid target conjunct" #f #f (list a-conjunct))]))
               
               (if (> (length the-target) 0)
                   (list (xml-make-command "SET TARGET FOR POLICY" 
                                           (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname)))
                                                 (xml-make-conjunct-chain (map validate-target-to-str the-target)))))
                   empty))))
    
       ;(printf "~a~n" target-result)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Rules
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define rules-result
         (if (empty? the-rules-clauses)
             empty
             (let ()    
               (define the-rules-clause (first the-rules-clauses))
               (define the-rules (rest (syntax-e the-rules-clause)))
               
               ; Why is this failing on one of my test cases?
               ; No time to look into it now, so re-implemented
               ; -tn 
               ;(define (is-valid-conjunction conj)
               ;  (syntax-case conj [true]
               ;    [(pred v0 v ...) #t]
               ;    [true #t]
               ;    [(true) #t]
               ;    [_ #f]))
               
               (define (is-valid-conjunction conj)
                 (syntax-case conj []
                   [(pred v0 v ...) #t]
                   [x (equal? 'true (syntax->datum #'x))]
                   [(x) (equal? 'true (syntax->datum #'x))]
                   [_ #f]))
               
               (define (handle-rule a-rule)
                 (syntax-case a-rule [= :-]
                   [(rulename = (decision rvar ...) :- conj0 conj ...)                     
                    ; 'true is dealt with in the back-end. 
                    ; require each conj to be valid
                    (andmap is-valid-conjunction (syntax->list #'(conj0 conj ...)))               
                    
                    (xml-make-command "ADD" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                  (xml-make-rule (syntax->datum #'rulename)
                                                                 (xml-make-decision-type (syntax->datum #'decision))
                                                                 (xml-make-rule-list (syntax->datum #'(conj0 conj ...))))))]
                   [_ (raise-syntax-error 'Policy "Invalid rule" #f #f (list a-rule))]))
                                             
               (map handle-rule the-rules))))

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; RComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-rcomb-clause (first the-rcomb-clauses))
       (define rcomb-result 
         (syntax-case the-rcomb-clause [RComb]
           [(RComb x0 x ...) (xml-make-command "SET RCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                                           (xml-make-identifiers-list (map symbol->string (syntax->datum #'(x0 x ...))))))]
           [_ (raise-syntax-error 'Policy "Invalid rule-combination algorithm" #f #f (list the-rcomb-clause))]))
         
       ;(printf "rcomb res: ~a~n" rcomb-result)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; PComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-pcomb-clause (first the-pcomb-clauses))
       (define pcomb-result 
         (syntax-case the-pcomb-clause [PComb]
           [(PComb x0 x ...) (xml-make-command "SET PCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                                            (xml-make-identifiers-list (map symbol->string (syntax->datum #'(x0 x ...))))))]
           [_ (raise-syntax-error 'Policy "Invalid policy-combination algorithm" #f #f (list the-pcomb-clause))]))
       
       ;(printf "~a~n" pcomb-result)
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Children
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-child-policies
         (if (empty? the-children-clauses)
             empty
             (let ()    
               (define the-children-clause (first the-children-clauses))
               (define the-children (rest (syntax-e the-children-clause)))               
               
               ; Let the macro system expand these
               the-children))) 
       
       ;(printf "compile time children: ~a~n" the-child-policies)
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Create
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define create-result        
         (if (< (length the-child-policies) 1)
             (list
              (xml-make-command "CREATE POLICY LEAF" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))) 
                                                           (xml-make-vocab-identifier (symbol->string (syntax->datum #'vocabname)))))
              rcomb-result)
             (list
              (xml-make-command "CREATE POLICY SET" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname)))
                                                         (xml-make-vocab-identifier (symbol->string (syntax->datum #'vocabname)))))
              pcomb-result)))
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Prepare
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define prepare-result 
            (xml-make-command "PREPARE" (list (xml-make-policy-identifier (symbol->string (syntax->datum #'policyname))))))       
                  
       
       ; Macro returns a lambda that takes a filename and a syntax object.   
       ; This is so we know where to find the vocabulary file. Only know that at runtime
       (with-syntax ([my-commands (append create-result ; create must come first
                                          target-result
                                          rules-result)]
                     [my-prepare-command (list prepare-result)]
                     [the-child-policies #`(list #,@the-child-policies)]
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
              (define vocab-path (build-path/file-ci (path-only/same local-policy-filename) 
                                                     (string-append (symbol->string 'vocabname) ".v")))
             
              ; Produce a friendly error if the vocab doesn't exist
              ; src-syntax here is the *command* that spawned the loading, not the Policy form.
              (file-exists?/error vocab-path src-syntax (format "The policy's vocabulary did not exist. Expected: ~a" (path->string vocab-path)))       
              
              (define vocab-macro-return                 
                (call-with-input-file
                    vocab-path
                  (lambda (in-port) 
                    (port-count-lines! in-port)
                    (define the-vocab-syntax (read-syntax vocab-path in-port))               
                    ; Keep as syntax here, so the PolicyVocab macro gets the right location info
                    ; margrave-policy-vocab-namespace is provided to the module that evaluates the code we're generating here
                    (eval the-vocab-syntax margrave-policy-vocab-namespace))))  
              
             ; (vocab-name xml-list types-names decisions-names predicates-names reqvariables-names othvariables-names)
             (define vocab-name (first vocab-macro-return))
             (define vocab-commands (second vocab-macro-return))
              
              ; Get the 6-tuples for each child
              (define child-policy-macros the-child-policies) ; no '
             
              
              ; Each child gives us its own list (pname, vname, vlist, plist, (child-pol-pairs), (child-voc-pairs)
              ; Error out if there is a duplicate policy name (or the same name as this parent policy)
             ; Don't repeat vocabularies
             (define the-children (map (lambda (child)                                          
                                         ((eval child margrave-policy-vocab-namespace) 
                                          local-policy-filename
                                          src-syntax))
                                       child-policy-macros))
             
             (define (make-placeholder-syntax placeholder)
               (datum->syntax #f placeholder (list 'orig-stx-source orig-stx-line orig-stx-column orig-stx-position orig-stx-span)))                                              
             
             ; take the child policy's tuple from evaluation
             ; get the list of pnames              
              (define (get-child-pols tuple)
                (when (or (< (length tuple) 5)
                          (not (list? (fifth tuple))))
                  (raise-syntax-error #f (format "Internal error: result from policy child did not have expected form. Result was ~a~n" tuple) 
                                      (make-placeholder-syntax 'placeholder)))
                (append (list (first tuple)) ; <-- child's pname
                        (fifth tuple)))      ; <-- list of child's children's pnames
              
             (define (get-add-child-xml tuple)
               (xml-make-command "ADD" (list `(PARENT ,(xml-make-parent-identifier (symbol->string 'policyname)) ,(xml-make-child-identifier (first tuple))))))
             
             (define my-commands-without-child 'my-commands)
             
             ; Forge the connection between parent and child policies
             ; and *prefix* this policy's commands with the child commands
             ; *FINALLY* prepare the policy.
             (define child-commands (append* (map fourth the-children)))
             (define my-commands-with-children (append child-commands
                                                       my-commands-without-child
                                                       (map get-add-child-xml the-children)
                                                       'my-prepare-command))                          
             
             (define child-polnames (flatten (map get-child-pols the-children)))
             
             ; Throw error if duplicate policy name
             (define (dup-pname-helper todo sofar)
               (cond [(empty? todo) #f]
                     [(member (first todo) sofar)                      
                      (raise-syntax-error 'Policy "Policy name duplicated among children and parent. All policies in a hierarchy must have distinct names." 
                                          (make-placeholder-syntax (first todo)))]
                     [else (dup-pname-helper (rest todo) (cons (first todo) sofar))]))
             
             (dup-pname-helper child-polnames (list (symbol->string 'policyname)))
              
             ; Don't double-load vocabs
             ; Even more, they must all be the same. Since we're in a fixed directory, check only the vname of each
             (for-each (lambda (tuple)
                         (unless (equal? (second tuple) (symbol->string 'vocabname))
                           (raise-syntax-error 'Policy 
                                               (format "Child policy used a vocabulary other than \"~a\". All children must have the same vocabulary as the parent." 'vocabname)                
                                               (make-placeholder-syntax (second tuple)))))
                       the-children)
                       
                          
              
              ; Return list(pname, vname, list-of-commands-for-vocab, list-of-commands-for-policy, list(child-pname, child-pcmds), list(child-vname, child-vcmds))
              `( ,(symbol->string 'policyname)
                 ,(symbol->string 'vocabname)
                 ,vocab-commands
                 ,my-commands-with-children
                 ,child-polnames)))))]

         
       
      
 
    
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

