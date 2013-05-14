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

(require
 racket/match
 "margrave-xml.rkt"
 "helpers.rkt"
 "polvochelpers.rkt"
 racket/list
 racket/contract
 racket/set
 rackunit
 xml
 
 (for-syntax (only-in srfi/13 string-contains)             
             "helpers.rkt"
             "polvochelpers.rkt"
             "margrave-xml.rkt"
             xml
             racket/list
             racket/match
             racket/string
             racket/base
             racket/contract))

(provide evaluate-policy 
                  
         Policy
         PolicySet
         Vocab
         Theory        
                  
         (all-from-out "polvochelpers.rkt"))


; We use eval to load policies and vocabularies, and the call is in the definitions window.
; Thus we need to provide a namespace for eval, or it won't know what to do with the Policy
; and PolicyVocab syntax.
(define-namespace-anchor margrave-policy-vocab-namespace-anchor)
(define margrave-policy-vocab-namespace (namespace-anchor->namespace margrave-policy-vocab-namespace-anchor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Error messages

(define-for-syntax err-invalid-constant-case "Invalid constant name. Constant names must begin with a lowercase letter.")
(define-for-syntax err-invalid-variable-case "Invalid variable name. Variable names must begin with a lowercase letter.")
(define-for-syntax err-invalid-function-case "Invalid function name. Function names must begin with a lowercase letter.")
(define-for-syntax err-invalid-constant "Invalid constant name.")
(define-for-syntax err-invalid-variable "Invalid variable name.")
(define-for-syntax err-invalid-function "Invalid function name.")

(define-for-syntax err-invalid-type-decl-case "Type declaration was not valid: Types must have capitalized names.")
(define-for-syntax err-invalid-type-decl "Type declaration was not valid.")

;****************************************************************

; Initialize policy file name.
; This is used by load-policy to assure we don't need to change
; the working directory.
(define local-policy-filename ".")

;****************************************************************

; policy-file-name policy-id [optional syntax] -> list
; The order of the children in their lists respects dependency: children-of-children appear before children, etc.
(define (evaluate-policy raw-fn 
                         policy-id
                         #:syntax [src-syntax #f])
  
  ; If fn begins with *MARGRAVE*, replace with the path of the Margrave collections folder.
  (define fn (resolve-margrave-filename-keyword raw-fn))
  
  ;; Macro returns a func of two arguments:
  ; (1) the filename of the policy (for constructing the location of the vocab file)
  ; (2) The policy ID to be bound
  ; (3) the command syntax that started evaluation (for nice error highlighting)
  
  ; Policy ID needs to be known at compile time, not runtime, since it's used by the command XML
  (parameterize ([read-case-sensitive #t])            
    (define file-port (open-input-file/exists fn src-syntax (format "Could not find the policy file: ~a~n" fn)))
    (port-count-lines! file-port)    
    (define the-policy-syntax (read-syntax fn file-port))   
        
    ; Don't convert to datum before evaluating, or the Policy macro loses location info
    (define the-policy-func (eval the-policy-syntax margrave-policy-vocab-namespace))           
    
    (define the-policy-instance (the-policy-func fn policy-id src-syntax))        
    (close-input-port file-port)        
    the-policy-instance))


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

; Used so that rules can be written nicely, e.g.
; ... :- (p x) (r y)
; instead of
; ... :- (and (p x) (r y))
;(define-for-syntax (handle-formula-list syn)
;  (define fmla-list (syntax->list syn))
;  (cond
;    [(> (length fmla-list) 1)
;     (quasisyntax/loc syn (and #,@fmla-list))]
;    [else syn]))

;(define-for-syntax (handle-combine comb)
;  (syntax-case comb [fa over]         
;    [(fa dec0 dec ...) (xml-make-fa (map symbol->string (syntax->datum #'(dec0 dec ...))))]
;    [(over dec odec0 odec ...) (xml-make-over (symbol->string (syntax->datum #'dec))
;                                              (map symbol->string (syntax->datum #'(odec0 odec ...))))]
;    [else (raise-syntax-error 'Policy "Invalid combination type. Must be (fa ...) or (over ...)" #f #f (list comb))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (PolicySet stx)
  (syntax-case stx [Children Target PComb uses]
    ([_ uses vocabname clauses ...]
     (let ()
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Target PComb Children]
                                                        [(Target targ ...) 'target]                   
                                                        [(PComb comb ...) 'pcomb] 
                                                        [(Children pol ...) 'children] 
                                                        [_ #f]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(target children pcomb)))
       
       ;(printf "PolicySet Clause list: ~a~n" (syntax->list #'(clauses ...)))
       ;(printf "PolicySet Clause table: ~n~a~n" clause-table)
       
       (define the-target-clauses (hash-ref clause-table 'target))                                 
       (define the-children-clauses (hash-ref clause-table 'children))
       (define the-pcomb-clauses (hash-ref clause-table 'pcomb))
       
       (assert-one-clause stx the-children-clauses "Children")
       (assert-lone-clause stx the-target-clauses "Target")
       (assert-lone-clause stx the-pcomb-clauses "PComb")       
       
      ; (printf "In policyset macro phase +1. done getting clauses. vocab was: ~v~n" (syntax->datum #'vocabname))
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Children
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Set of syntax objects. Will be consumed in the func we create.
       (define the-child-policies
         (let ()    
           (define the-children-clause (first the-children-clauses))
           (rest (syntax-e the-children-clause)))) 
       
       ;(printf "In policyset macro phase +1. number of children gotten:~v~n" (length the-child-policies))
       
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
                 (raise-syntax-error 'PolicySet "Only one target formula can be given, but had more than one." #f #f (list the-target-clause)))
               (when (< (length the-targets) 1)
                 (raise-syntax-error 'PolicySet "The Target clause must contain a formula if it is given." #f #f (list the-target-clause)))
               
               (define the-target (first the-targets))                                             
               the-target)))    
              
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; PComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define pcomb-result 
         (if (empty? the-pcomb-clauses)
             empty
             (let ()
               (define the-pcomb-clause (first the-pcomb-clauses)) 
               (syntax-case the-pcomb-clause [PComb]
                 [(PComb x0 x ...) (map comb->sexpr (syntax->list #'(x0 x ...)))]
                 [_ (raise-syntax-error 'PolicySet "Invalid policy-combination clause." #f #f (list the-pcomb-clause))]))))
              
       ;(printf "Children are: ~v~n" the-child-policies)
       ; Macro returns a lambda that takes a filename and a syntax object.   
       (with-syntax (;[the-child-policies #`(list #,@the-child-policies)]
                     [the-child-policies `',the-child-policies]
                     [vocabname #'vocabname]
                     [target-result target-result]
                     [my-comb-list pcomb-result]
                     
                     ; can't include Policy here or else it gets macro-expanded (inf. loop)
                     ; smuggle in location info and re-form syntax if we need to throw an error
                     [orig-stx-line #`#,(syntax-line stx)]
                     [orig-stx-column #`#, (syntax-column stx)]
                     [orig-stx-source #`#, (syntax-source stx)]
                     [orig-stx-position #`#, (syntax-position stx)]
                     [orig-stx-span #`#, (syntax-span stx)])     
         
         (syntax/loc stx 
           ; Don't quote the lambda. Un-necessary (and would force evaluate-policy to double-eval)
           (lambda (local-policy-filename local-policy-id src-syntax [thy/maybe #f])                     
             (define vocab-path (build-path/file-ci (path-only/same local-policy-filename) 
                                               (string-append (symbol->string 'vocabname) ".v")))             
             (define my-theory
               (cond [(m-theory? thy/maybe)
                      thy/maybe]
                     [else
                      ; Produce a friendly error if the vocab doesn't exist
                      ; src-syntax here is the *command* that spawned the loading, not the Policy form.
                      (file-exists?/error vocab-path src-syntax 
                                          (format "The policyset's vocabulary did not exist. Expected: ~a" (path->string vocab-path)))                         
                      (define my-t-or-v 
                        (call-with-input-file
                            vocab-path
                          (lambda (in-port) 
                            (port-count-lines! in-port)
                            (define the-vocab-syntax (read-syntax vocab-path in-port))               
                            ; Keep as syntax here, so the PolicyVocab macro gets the right location info
                            ; margrave-policy-vocab-namespace is provided to the module that evaluates the code we're generating here
                            (eval the-vocab-syntax margrave-policy-vocab-namespace))))
                      (if (m-vocabulary? my-t-or-v)
                          (m-theory (m-vocabulary-name my-t-or-v) vocab-path my-t-or-v empty)
                          my-t-or-v)]))
                                       
             ; Error out if there is a duplicate policy name (or the same name as this parent policy)
             ; Don't repeat vocabularies             
             (define the-children-list (map (lambda (childproc)
                                             ; (printf "Handling: ~v~n" childproc)
                                              ;(childproc
                                               ((eval childproc margrave-policy-vocab-namespace) 
                                               local-policy-filename
                                               (string-append local-policy-id (->string (gensym)))
                                               src-syntax
                                               thy/maybe))
                                            the-child-policies))
             (define (children-aggregator child sofar)
               (define id
                 (cond [(m-policy? child) (m-policy-id child)] 
                       [else (m-policy-id child)]))
               (hash-set sofar id child))
             
             (define the-children (foldl children-aggregator (make-immutable-hash '()) the-children-list))                    
             
             (define (make-placeholder-syntax placeholder)
               (datum->syntax #f placeholder
                              (list 'orig-stx-source orig-stx-line orig-stx-column orig-stx-position orig-stx-span)))                                              
             
             ; Vocabs must all be the same. Since we're in a fixed directory, check only the vname of each
             (for-each (lambda (child)
                         (define childthy
                           (cond [(m-policy? child) (m-policy-theory child)] 
                                 [else (m-policy-theory child)]))
                         (unless (equal? (m-theory-name my-theory) (m-theory-name childthy))
                           (raise-syntax-error 
                            'Policy 
                            (format "Child policy used a vocabulary other than ~a. Child vocabulary must match parent vocabulary, but used ~a." 
                                    'vocabname 
                                    (m-theory-name childthy))                
                            (make-placeholder-syntax child))))
                       (hash-values the-children))
             
             
             ;[id string?]   
             ;[theory m-theory?]   
             ;;[vardecs (hash/c string? m-vardec?)]
             ;[children (hash/c string? m-policy?)]
             ;[pcomb string?]
             ;[target m-formula?]
             ;[idbs (hash/c string? (listof string?))])                          
             
             ;(printf "~v~n" local-policy-id)
             ;(printf "~v~n" the-children)
             ;(printf "~v~n" 'my-comb-list)
             ;(printf "~v~n" 'target-result)
             
             ; id theory vardecs rule-names rules comb target idbs children
             (m-policy-set local-policy-id 
                           my-theory 
                           (hash) ; vardecs
                           empty
                           (hash)
                           'my-comb-list
                           'target-result
                           (hash) ; EXTRA idbs beyond what is in children. empty!
                           the-children))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A Margrave theory contains a vocabulary and axioms constraining the 
; models of the vocabulary.
; Syntax (Theory (Vocabulary ... ) (Axioms ...))


(define-syntax (Theory stx)
  (syntax-case stx [Axioms ]
    ([_ theoryname myvocab (Axioms myaxioms ...) ]
     
     (let ()       
       ; (Vocab ...)
       ; will expand to (m-vocabulary ...)
       (define the-vocab-syntax #'myvocab)                           
       (define the-axioms-clauses (syntax-e #'(myaxioms ...)))                     
       
       (with-syntax ([theory-name (->string #'theoryname)]                     
                     [the-vocab-syntax the-vocab-syntax]                     
                     [the-axioms-list the-axioms-clauses])                                               
         (syntax/loc stx (m-theory theory-name (string->path ".") the-vocab-syntax 'the-axioms-list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (Vocab stx)
  (syntax-case stx [Types Predicates Constants Functions Vocab]
    ([_ myvocabname clauses ...]     
     
     (let ()
       (define vocab-name-syntax #'myvocabname)
       (define vocab-name (syntax->datum vocab-name-syntax))
       (define vocab-name-string (symbol->string vocab-name))
       
       (unless (symbol? vocab-name)
         (raise-syntax-error 'PolicyVocab (format "Expected a name for the vocabulary, got: ~a" vocab-name) #f #f (list vocab-name-syntax)))
       
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Types Predicates Constants Functions]
                                                        [(Types atypedec ...) 'types]                                   
                                                        [(Predicates apreddec ...) 'predicates] 
                                                        [(Constants aconstdec ...) 'constants] 
                                                        [(Functions afuncdec ...) 'functions] 
                                                        [_ 'other]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(types predicates constants functions)))
       
       ; from gmarceau
       ;(define-syntax (syntax-case-match? stx)
       ;  [(_ v lits pattern) #'(syntax-case v (Types Decisions Predicates ReqVariables OthVariables Constraints) [pattern #t] [_ #f])])       
       ;(define the-types-clauses (filter (lambda (v) (syntax-case-match? v (Types (t subt ...) ...)) clauses)))
       
       (define the-types-clauses (hash-ref clause-table 'types)) 
       (define the-predicates-clauses (hash-ref clause-table 'predicates))
       (define the-constants-clauses (hash-ref clause-table 'constants))
       (define the-functions-clauses (hash-ref clause-table 'functions))
              
       (when (hash-has-key? clause-table 'other)
         (margrave-error "Invalid clauses in vocabulary declaration"  (hash-ref clause-table 'other)))
       
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
         (syntax-case a-type [>]                           
           ; (T > A B C)
           [(t > subt ...) (and (capitalized-id-syn? #'t)
                                (all-are-syn? #'(subt ...) capitalized-id-syn?))                                
                           (m-type (symbol->string (syntax->datum #'t))
                                   (map (compose symbol->string syntax->datum) (syntax-e #'(subt ...))))] 
           [(t > subt ...) (not 
                            (and (capitalized-id-syn? #'t)
                                 (all-are-syn? #'(subt ...) capitalized-id-syn?)))
                           (raise-syntax-error
                            'Vocab err-invalid-type-decl-case #f #f (list a-type)) ] 
           
           ; T (check for this AFTER the subtype declarations above, or the final "t" will match them.)
           [t (capitalized-id-syn? #'t)
              (m-type (symbol->string (syntax->datum #'t)) empty)]             
           
           [_ (raise-syntax-error 'Vocab err-invalid-type-decl #f #f (list a-type))]))
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Remove duplicate types, but combine child names.
       ; Child names who don't have their own constructor are given one.
       (define/contract (resolve-m-types types)
         [(listof m-type?) . -> . (listof m-type?)]
         (define the-buckets (partition* m-type-name types))
         (define the-sort-names (hash-keys the-buckets))  
         
         (define (combine-same-types name)
           (define duplicate-types (hash-ref the-buckets name))
           (define the-children (sort (flatten (map m-type-child-names duplicate-types)) string<?))
           (define unmentioned-children (filter (lambda (c) (not (member c the-sort-names))) the-children))
           (define unmentioned-mtypes (map (lambda (c) (m-type c empty)) unmentioned-children))    
           (append unmentioned-mtypes (list (m-type name the-children))))
         
         (flatten (map combine-same-types the-sort-names)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define types-result-list (resolve-m-types (map handle-type the-types)))
       
       (define (type-aggregator ele sofar)
         (hash-set sofar (m-type-name ele) ele))
       (define types-result (foldl type-aggregator (make-immutable-hash '()) types-result-list))      
       
       ; potential duplicates here (and in above xml); s/b a set maintained throughout the process?
       (define types-names (hash-keys types-result))
       
       ; Used by predicate/function/constant sections to recognize a type name that wasn't declared.
       (define (is-valid-type? typename-syn)
         (define type-name-str (->string typename-syn))
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
       (define predicates-result-list
         (if (empty? the-predicates-clauses)
             empty
             (let ()
               (define the-predicates-clause (first the-predicates-clauses))
               (define the-predicates (rest (syntax-e the-predicates-clause)))                                        
               
               (define (handle-predicate pred)
                 ; No colon. Just (predname A B C)
                 (syntax-case pred []                   
                   [(pname prel0 prel ...) (and (lower-id-syn? #'pname)
                                                          (each-type-in-list-is-valid? #'(prel0 prel ...)))  
                                                     (m-predicate (symbol->string (syntax->datum #'pname))
                                                                  (map syntax->string (syntax->list #'(prel0 prel ...))))] 
                   
                   [_ (raise-syntax-error 'Vocab "Invalid predicate declaration." #f #f (list pred) )]))
               
               (map handle-predicate the-predicates))))
       
       (define (predicate-aggregator ele sofar)
         (hash-set sofar (m-predicate-name ele) ele))
       (define predicates-result (foldl predicate-aggregator (make-immutable-hash '()) predicates-result-list))
                    
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Constants
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;              
       
       ; Optional clause; allow to be empty.
       (define constants-result-list
         (if (empty? the-constants-clauses)
             empty
             (let ()
               (define the-constants-clause (first the-constants-clauses))
               (define the-constants (rest (syntax-e the-constants-clause)))                                
               
               (define (handle-constant const)
                 ; No colon. Just ('a A)
                 (syntax-case const []                    
                   [('cname crel) (and (lower-id-syn? (syntax cname))
                                                (is-valid-type? (syntax crel)))  
                                           (m-constant (syntax->string #'cname)
                                                       (syntax->string #'crel))] 
                   
                   [('cname crel) (not (lower-id-syn? (syntax cname)))
                                           (raise-syntax-error `Vocab err-invalid-constant #f #f (list const) )]
                   [(cname crel) (raise-syntax-error `Vocab "Invalid constant name. Constant names must be preceded with a single quote. For instance: (Constant 'c A)" #f #f (list const) )]
                   
                   [_ (raise-syntax-error 'Vocab err-invalid-constant #f #f (list const) )]))
               
               (map handle-constant the-constants))))
       
       (define (constants-aggregator ele sofar)
         (hash-set sofar (m-constant-name ele) ele))
       (define constants-result (foldl constants-aggregator (make-immutable-hash '()) constants-result-list))
             
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Functions
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       ; Optional clause; allow to be empty.
       (define functions-result-list
         (if (empty? the-functions-clauses)
             empty
             (let ()
               (define the-functions-clause (first the-functions-clauses))
               (define the-functions (rest (syntax-e the-functions-clause)))                                
               
               (define (handle-function func)
                 ; No colon. Just (f A B)
                 (syntax-case func []                   
                   [(fname frel0 frel ...) (and (lower-id-syn? #'fname)
                                                (each-type-in-list-is-valid? #'(frel0 frel ...))) 
                                           (let ()
                                             (define args (map symbol->string (syntax->datum #'(frel0 frel ...))))
                                             (define arity (take args (- (length args) 1)))
                                             (define result (last args))
                                             (define funcname (syntax->string #'fname))
                                             (m-function funcname arity result))] 
                   
                   [_ (raise-syntax-error 'Vocab err-invalid-function #f #f (list func) )]))
               
               (map handle-function the-functions))))
       
       (define (functions-aggregator ele sofar)
         (hash-set sofar (m-function-name ele) ele))
       (define functions-result (foldl functions-aggregator (make-immutable-hash '()) functions-result-list))
       
              
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
       ; We have no idea whether the vocabulary has been created yet or not. 
       ; Java will handle creation of the object if the identifier hasn't been seen before.                            
              
       ;;;;;;;;; Final Syntax ;;;;;;;;;
       (with-syntax ([vocab-name-string vocab-name-string]                     
                     [types-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-type-name ele) `',(disassemble-transparent-struct ele))) (hash-values types-result))))]
                     [predicates-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-predicate-name ele) `',(disassemble-transparent-struct ele))) (hash-values predicates-result))))]
                     [constants-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-constant-name ele) `',(disassemble-transparent-struct ele))) (hash-values constants-result))))]
                     [functions-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-function-name ele) `',(disassemble-transparent-struct ele))) (hash-values functions-result))))])     
         
         (syntax/loc stx (m-vocabulary vocab-name-string                                       
                                       (assemble-struct-hash types-disassembled-hash)
                                       (assemble-struct-hash predicates-disassembled-hash)
                                       (assemble-struct-hash constants-disassembled-hash)
                                       (assemble-struct-hash functions-disassembled-hash))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Policy: Parses a policy definition and creates an MPolicyLeaf OR MPolicySet object
; Policies are permitted to have child policies, so this may be recursively called

(define-for-syntax (comb->sexpr comb)         
  (syntax-case comb [fa over]         
    [(fa dec0 dec ...) (syntax->datum comb)]
    [(over dec odec0 odec ...)(syntax->datum comb)]
    [else (raise-syntax-error 'Policy "Invalid combination type. Must be (fa ...) or (over ...)" #f #f (list comb))]))

(define-syntax (Policy stx)
  (syntax-case stx [Target Rules = :- uses RComb Policy Variables]
    [(_ uses a-vocab-or-thy-name clauses ... )
     
     (let ()
       (unless (identifier? #'a-vocab-or-thy-name)
         (raise-syntax-error 'Policy (format "Expected a theory or vocabulary name for the policy to use, got: ~a"
                                             (syntax->datum #'a-vocab-or-thy-name)) #f #f (list #'a-vocab-or-thy-name)))                          
       
       ;(printf "In POLICY macro phase +1. done getting clauses. vocab was: ~v~n" (syntax->datum #'a-vocab-or-thy-name))              
       
       (define my-theory-name-sym (syntax->datum #'a-vocab-or-thy-name))     
       (define my-theory-name-str (->string my-theory-name-sym)) 
       
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Target Rules = :- RComb PComb Children Variables]
                                                        [(Variables vardec ...) 'variables]
                                                        [(Target targ ...) 'target]                   
                                                        [(Rules rule ...) 'rules]
                                                        [(RComb comb ...) 'rcomb] 
                                                        [_ 'all-other]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(variables target rules rcomb all-other)))
       
       ;(printf "Policy Clause list: ~a~n" (syntax->list #'(clauses ...)))
       ;(printf "Policy Clause table: ~n~a~n" clause-table)
       
       (define the-variables-clauses (hash-ref clause-table 'variables))                            
       (define the-target-clauses (hash-ref clause-table 'target))                                 
       (define the-rules-clauses (hash-ref clause-table 'rules))
       (define the-rcomb-clauses (hash-ref clause-table 'rcomb))
       (define other-clauses (hash-ref clause-table 'all-other))
       
       (assert-one-clause stx the-variables-clauses "Variables")
       (assert-lone-clause stx the-target-clauses "Target")
       (assert-one-clause stx the-rules-clauses "Rules")
       (assert-lone-clause stx the-rcomb-clauses "RComb")       
       
       (unless (empty? other-clauses)
         (raise-syntax-error 'Policy "Invalid policy clause(s)" #f #f other-clauses))
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Variables
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define variables-result-list
         (if (empty? the-variables-clauses)
             empty
             (let ()    
               (define the-variables-clause (first the-variables-clauses))
               (define the-variables (rest (syntax-e the-variables-clause)))
               
               (define (handle-variable a-var-dec)
                 (syntax-case a-var-dec []
                   [(lowid capid)                                         
                    (and (capitalized-id-syn? #'capid)
                         (lower-id-syn? #'lowid))
                    (m-vardec (syntax->datum #'lowid) (syntax->datum #'capid))]
                   [(lowid capid)                                         
                    (not (capitalized-id-syn? #'capid))
                    (raise-syntax-error 'Policy "Invalid variable declaration; the variable's type must be capitalized." #f #f (list a-var-dec))]
                   
                   [(lowid capid)                                         
                    (not (lower-id-syn? #'lowid))
                    (raise-syntax-error 'Policy "Invalid variable declaration; the variable's name must begin with a lowercase letter." #f #f (list a-var-dec))]
                   
                   [_ (raise-syntax-error 'Policy "Invalid variable declaration. Expected (Variable varname Typename)" #f #f (list a-var-dec))]))
               
               (map handle-variable the-variables))))
              
       
       ; vardec-hash
       (define (vars-aggregator ele sofar)
         (hash-set sofar (m-vardec-name ele) ele))
       (define vardec-hash (foldl vars-aggregator (make-immutable-hash '()) variables-result-list))                    

       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Target
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define target-result
         (if (empty? the-target-clauses)
             'true
             (let ()               
               (define the-target-clause (first the-target-clauses))
               (define the-targets (rest (syntax-e the-target-clause)))
               (when (> (length the-targets) 1)
                 (raise-syntax-error 'Policy "Only one target formula can be given, but had more than one." #f #f (list the-target-clause)))
               (when (< (length the-targets) 1)
                 (raise-syntax-error 'Policy "The Target clause must contain a formula if it is given." #f #f (list the-target-clause)))
               (when (not (m-formula? (first the-targets)))
                 (raise-syntax-error 'Policy "Target was not a valid formula." #f #f (list (first the-targets))))
               
               (first the-targets))))
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Rules
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define rules-result-list
         (if (empty? the-rules-clauses)
             empty
             (let ()    
               (define the-rules-clause (first the-rules-clauses))
               (define the-rules (rest (syntax-e the-rules-clause)))
               
               ;(printf "the-rules: ~v~n" the-rules)
               
               (define (handle-rule a-rule)
                 (syntax-case a-rule [= :-]
                   [(rulename = (decision rvar ...) :- fmla0 fmla ...)
                    (begin
                     ; (printf "Creating Rule: ~v with conditions: ~v~n" #'rulename (syntax->list #'(fmla0 fmla ...)))
                      ;(printf "Head variables: ~v~n" (map syntax->datum (syntax->list #'(rvar ...))))
                      ; Add implicit 'and if needed before desugaring:
                      (define bodyfmlas (syntax->list #'(fmla0 fmla ...)))
                      (define bodyfmla (if (> (length bodyfmlas) 1) 
                                           `(and ,@bodyfmlas)
                                           (first bodyfmlas)))
                      (m-rule (->string #'rulename)
                              (->string #'decision)
                              (map syntax->datum (syntax->list #'(rvar ...)))
                               bodyfmla))]
                   [_ (raise-syntax-error 'Policy "Rule form did not have the expected shape." #f #f (list a-rule))]))
               
               (map handle-rule the-rules))))                                  
       
       (define (rules-aggregator ele sofar)
         (when (hash-has-key? sofar (m-rule-name ele))
           (raise-syntax-error 'Policy (format "There were multiple rules named ~v. Rule names must be unique." (m-rule-name ele))))
         (hash-set sofar (m-rule-name ele) ele))
       (define rules-hash (foldl rules-aggregator (make-immutable-hash '()) rules-result-list))

       
       ; Order matters! Make sure the order is preserved when passing XML to java. 
       ; (The hash table has no concept of ordering on the entries, so keep the keys in order.)
       (define rule-names (map m-rule-name rules-result-list))
       
       (define (internal-get-variable-sort varid)
         (unless (hash-has-key? vardec-hash varid)
           (margrave-error "Variable was not declared" varid))
         (m-vardec-type (hash-ref vardec-hash varid)))
       
       (define/contract (idb-aggregator ele sofar) 
         [m-rule? hash? . -> . hash?]
         (define idbarity (map internal-get-variable-sort (m-rule-headvars ele)))
         (when (and (hash-has-key? sofar (m-rule-decision ele))
                    (not (equal? idbarity (hash-ref sofar (m-rule-decision ele)))))
           (margrave-error "The rule used a decision that was used previously, but with a different free variable arity" ele))                         
         (define result1 (hash-set sofar (m-rule-decision ele) idbarity))
         (define result2 (hash-set result1 (string-append (m-rule-name ele) "_applies") idbarity))
         (define result3 (hash-set result2 (string-append (m-rule-name ele) "_matches") idbarity))
         result3)        
       (define idbs-hash (foldl idb-aggregator (make-immutable-hash '()) rules-result-list))              
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; RComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                        
       
       (define rcomb-result 
         (if (empty? the-rcomb-clauses)
             empty
             (let ()
               (define the-rcomb-clause (first the-rcomb-clauses))
               (syntax-case the-rcomb-clause [RComb]
                 [(RComb x0 x ...) (map comb->sexpr (syntax->list #'(x0 x ...)))]
                 [_ (raise-syntax-error 'Policy "Invalid rule-combination clause." #f #f (list the-rcomb-clause))]))))
       
       ; !!! How to make fa, over case insensitive?
      
       
       ; Only know filename and policy id at *runtime*                                         
       
       ; need ,(list (xml-make-command ...
       ; so do `(list ,@( ...))
       ; (The vocab command list doesn't have unquoting since everything is known about the vocab at compile time)
       ; (create must come first; prepare last)              
       
       ;(printf "~v~n" #`(hash #,@(apply append (map (lambda (ele) (list (m-rule-name ele) `',(disassemble-transparent-struct ele))) (hash-values rules-hash)))))
       ;(printf "~v~n" #`(hash #,@(apply append (map (lambda (ele) (list (m-vardec-name ele) `',(disassemble-transparent-struct ele))) (hash-values vardec-hash)))))
       
       ; Need `', in front of re-assembled symbols. Strings are ok.
       
       (with-syntax ([my-theory-name my-theory-name-sym]
                     [my-comb-list rcomb-result]
                     [my-target target-result]
                     [disassembled-rules-hash #`(hash #,@(apply append (map (lambda (ele) (list (m-rule-name ele) `',(disassemble-transparent-struct ele))) (hash-values rules-hash))))]
                     [disassembled-vardec-hash #`(hash #,@(apply append (map (lambda (ele) (list `',(m-vardec-name ele) `',(disassemble-transparent-struct ele))) (hash-values vardec-hash))))]
                     [rule-names rule-names]
                     [idbs-hash idbs-hash]
                     
                     ; can't include Policy here or else it gets macro-expanded (inf. loop)
                     ; smuggle in location info and re-form syntax if we need to throw an error
                     [orig-stx-line #`#,(syntax-line stx)]
                     [orig-stx-column #`#, (syntax-column stx)]
                     [orig-stx-source #`#, (syntax-source stx)]
                     [orig-stx-position #`#, (syntax-position stx)]
                     [orig-stx-span #`#, (syntax-span stx)])     
         
         (syntax/loc stx 
           ; Don't quote the lambda. Un-necessary (and would force evaluate-policy to double-eval)
           (lambda (local-policy-filename local-policy-id src-syntax [thy/maybe #f])                                                                         
             (define s-VOCAB-EXTENSION ".v")                          
             
             (define vocab-file-name 
               (string-append (string-downcase (symbol->string 'my-theory-name)) s-VOCAB-EXTENSION))
             
             ; vocab names are all lower-cased before searching for file
             (define vocab-path (build-path/file-ci (path-only/same local-policy-filename) 
                                                    vocab-file-name))
                          
             ; If vocab-path is #f here, the file doesn't exist (in any capitalization)
             
             (define my-thy
               (cond [(m-theory? thy/maybe)
                      thy/maybe]                    
                     [else 
                      ; Produce a friendly error if the vocab doesn't exist
                      ; src-syntax here is the *command* that spawned the loading, not the Policy form.                      
                      (file-exists?/error vocab-path src-syntax 
                                          (format "Unable to find the vocabulary named in policy file ~a.~nExpected a file ~a in directory ~a~nCurrent-directory is: ~a" 
                                                  (->string local-policy-filename)
                                                  (->string vocab-file-name)
                                                  (->string (path-only/same local-policy-filename))
                                                  (->string (current-directory))))                         
                      (define my-t-or-v 
                        (call-with-input-file
                            vocab-path
                          (lambda (in-port) 
                            (port-count-lines! in-port)
                            (define the-vocab-syntax (read-syntax vocab-path in-port))  ; (Vocab ...)                    
                            ; Keep as syntax here, so the PolicyVocab macro gets the right location info
                            ; margrave-policy-vocab-namespace is provided to the module that evaluates the code we're generating here
                            (eval the-vocab-syntax margrave-policy-vocab-namespace))))
                      (if (m-vocabulary? my-t-or-v)
                          (m-theory (m-vocabulary-name my-t-or-v) (or vocab-path (string->path ".")) my-t-or-v empty)
                          my-t-or-v)]))
             
             
             (define (make-placeholder-syntax placeholder)
               (datum->syntax #f placeholder
                              (list 'orig-stx-source orig-stx-line orig-stx-column orig-stx-position orig-stx-span))) 
             
             (define vardec-hash (assemble-struct-hash disassembled-vardec-hash))             
             (define rules-hash (assemble-struct-hash disassembled-rules-hash))             
               
             (define (grant-theory-path t vp)
               (m-theory (m-theory-name t) (or vp (string->path ".")) (m-theory-vocab t) (m-theory-axioms t)))                          
             
             (m-policy local-policy-id ; [id string?]                                             
                       (grant-theory-path my-thy vocab-path) ; [theory-path path?]; [theory m-theory?]                     
                       vardec-hash ; [vardecs (hash/c string? m-vardec?)]
                       'rule-names ; [rule-names (list string?)]
                       rules-hash ; [rules (hash/c string? m-rule?)]
                       'my-comb-list ; [rcomb any/c]
                       'my-target ; [target m-formula?]
                       idbs-hash ;  [idbs (hash/c string? (listof string?))]
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

