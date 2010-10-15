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

(provide evaluate-policy)

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
        
    ; don't keep the handle open! call-with-input-file would be better here.  
    (close-input-port file-port)    
    
    ; Return the script needed to create this policy
    pol-result-list))



;****************************************************************

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
       
       (define (assert-one-clause clause-list descriptor)
         (match clause-list
           [(list) (raise-syntax-error #f (format "~a clause is missing" descriptor) stx)]
           [(list x y z ...) (raise-syntax-error 'PolicyVocab (format "More than one ~a clause found" descriptor) #f #f (list* x y z))]
           [_ (void)]))         
       (define (assert-lone-clause clause-list descriptor)
         (match clause-list
           [(list x y z ...) (raise-syntax-error 'PolicyVocab (format "More than one ~a clause found" descriptor) #f #f (list* x y z))]
           [_ (void)]))         
       
       (assert-one-clause the-types-clauses "Types")
       (assert-one-clause the-decisions-clauses "Decisions")
       (assert-lone-clause the-predicates-clauses "Predicates")
       (assert-one-clause the-reqvariables-clauses "ReqVariables")
       (assert-lone-clause the-othvariables-clauses "OthVariables")
       (assert-lone-clause the-constraints-clauses "Constraints")
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Types
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define (none-colon-syn? syn-list)
         (define lst (if (syntax? syn-list)
                         (syntax->datum syn-list)
                         syn-list))
         (cond [(empty? lst) #t]
               [(equal? (first lst) ':) #f] ; syntax->datum done above (recursively)
               [else (none-colon-syn? (rest lst))]))
       
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



; Policy: Parses a policy definition and creates an MPolicyLeaf OR MPolicySet object
; Policies are permitted to have child policies, so this may be recursively called
; (but is guaranteed to terminate.)
(define-syntax (Policy stx)
  (syntax-case stx [Target Rules = :- uses RComb PComb Children Policy]
    ([_ policyname uses vocabname
        (Target tconj ...) ; back end expects a list of conjuncts
        (Rules (rulename = (dtype v ...) :- conj ...) ; for each variable in the IDB dec; for each conjunct in the rule itself                    
               ...); for each rule
        (RComb rcstr ...) ; rule combination alg?
        (PComb pcstr ...) ; policy combination alg?
        (Children child ...)] ; child policies? 
     
     ; Return a function of one argument that can be called in the context of some local directory.
     ; This is so we know where to find the vocabulary file.
     (syntax/loc stx
       (lambda (local-policy-filename src-syntax) 
         (define vocab-path (build-path (path-only/same local-policy-filename) 
                                      (string-append (symbol->string 'vocabname) ".v")))
       ; Produce a friendly error if the vocab doesn't exist
       (file-exists?/error vocab-path src-syntax (format "The policy's vocabulary did not exist. Expected: ~a" (path->string vocab-path)))       
       
       (let* ([mychildren (list child ...)]
                           
             ; !!! TODO: Only using eval here because we had to in SISC;
              ; stop using it when switch to language-level
             [vocab-macro-return                 
              (call-with-input-file
               vocab-path
               (lambda (in-port) 
                 (port-count-lines! in-port)
                 (define the-vocab-syntax (read-syntax vocab-path in-port))
                 
                 (eval (syntax->datum the-vocab-syntax) margrave-policy-vocab-namespace)))]
             
             [vocab-name (first vocab-macro-return)]
             [vocab-commands (second vocab-macro-return)])
         
         ; In SISC, the above was: 
         ;                       (eval (read (open-input-file
         ;                           (normalize-url 
         ;                            ; Make sure we look in the correct directory!
         ;                            local-policy-filename 
         ;                            (string-append (symbol->string 'vocabname) ".v")))))))
                 
         
         ;Return (Policyname vocabName vocabcommands policycommands
         `(,(symbol->string 'policyname)
           ,vocab-name
           ,vocab-commands
           (
            ,(if (< (length mychildren) 1)
                 (xml-make-command "CREATE POLICY LEAF" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-vocab-identifier vocab-name)))
                 (xml-make-command "CREATE POLICY SET" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-vocab-identifier vocab-name))))
            
            
            ;; !!! TODO This was an ugly hack to get around a problem with the .p language.
            ; Either fix the language, or fix the hack.
            ;(let ((myvarorder (m (string-append "GET REQUEST VECTOR " myvocab))))
            
            
            ; Set the policy target (if any)
            ;Wrap the value in a list, and then unquote-splice, so that we can pass back the empty list if need be
            ,@(let ((the-target (list (symbol->string 'tconj) ...)))
              (if (> (length the-target) 0)
                (list (set-target (symbol->string 'policyname) the-target))
                (list)))
            
            ; Add the rules to the policy. 'true is dealt with in the back-end.         
            ,(add-rule (symbol->string 'policyname)
                      ;myvarorder 
                      (symbol->string 'rulename) (symbol->string 'dtype) (list (symbol->string 'v) ...) (list 'conj ...))
            ...
            
            ; Set the rule and policy combinator (depending on type)
            ,(if (< (length mychildren) 1)
                (xml-make-command "SET RCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-identifiers-list (list 'rcstr ...))))
                (xml-make-command "SET PCOMBINE FOR POLICY" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-identifiers-list (list 'pcstr ...)))))
            
            ;; !!! TODO: confirm this works. are we loading the sub-policy properly?
            
            ; Each child is a Policy
            ,(let ((cpol child))
              (xml-make-command "ADD" (list (xml-make-policy-identifier (symbol->string 'policyname)) (xml-make-policy-identifier cpol)))
              )
            ...
            
            ; Trigger IDB calculation
            ,(xml-make-command "PREPARE" (list (xml-make-policy-identifier (symbol->string 'policyname)))))
           ; close paren for above GET REQUEST VECTOR commented out )
           )))))))

; ********************************************************************
; Helper functions 

; listsubs contains a list of the subsorts for this sort. 
; However, it may be nested: subsorts may themselves have subsorts.
; Returns a list of xml commands
; TN: Changed to ignore ': in subsort lists. 
; (Kludge to make ":" optional deprecated syntax)

;(define (add-subtypes-of vocab parent listsubs-dirty)  
;  (let ([listsubs-cleaned (filter (lambda (x) (not (equal? x ':))) listsubs-dirty)])
;    ; listsubs may be empty -- if so, do nothing (we already added parent)
;    (if (> (length listsubs-cleaned) 0)            
;        (foldr (lambda (s rest) 
;                 ; Is this a sort with subsorts itself?
;                 (append 
;                  (if (list? s)                       
;                      (begin
;                        ; (display parent) ; debug
;                        ; Add subtype relationship between parent and s
;                        (cons (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-subsort parent (symbol->string (car s)))))
;                              
;                              ; Is this a nested subtype? If so, we must
;                              ; deal with s's subtypes.
;                              
;                              ; Check for list size;
;                              ; someone may have used parens without meaning to.
;                              (if (> (length s) 1)
;                                  (add-subtypes-of vocab (symbol->string (car s)) (cdr s))
;                                  empty)))
;                      
;                      ; Bottom of sort tree. 
;                      (begin
;                        ;(printf "~a ~a ~a ~n" s listsubs-cleaned parent)
;                        (list (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-subsort parent (symbol->string s)))))))
;                  rest))
;               empty
;               listsubs-cleaned)
;        empty)))

; Sets the target property of a policy object
(define (set-target mypolicy conjlist)
  (xml-make-command "SET TARGET FOR POLICY" (list (xml-make-policy-identifier mypolicy) (xml-make-conjunct-chain conjlist))))


; Add a rule of the form rulename = (dtype reqvars) :- conjlist
(define (add-rule mypolicy rulename dtype reqvars conjlist)  
  (xml-make-command "ADD" (list (xml-make-policy-identifier mypolicy) (xml-make-rule rulename (xml-make-decision-type dtype) (xml-make-rule-list conjlist))))) 
