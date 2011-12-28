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
 (only-in srfi/1 zip)
 
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
         ; for test cases
         Policy
         PolicySet
         Vocab
         Theory
         
         ; for reflection, errors, etc
         cached-policies         
         cached-theories
         
         ; used by compiler and margrave modules
         cached-prior-queries
         
         m-formula-is-well-sorted?
         m-formula-is-well-sorted?/err
         get-uber-vocab-for-formula 
         combine-vocabs
         
         (all-from-out "polvochelpers.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cached-policies (make-hash))
(define cached-vocabularies (make-hash))
(define cached-theories (make-hash))

(define cached-prior-queries (make-hash))

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
  ; (2) the command syntax that started evaluation (for nice error highlighting)
  
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

(define-for-syntax (handle-combine comb)
  (syntax-case comb [fa over]         
    [(fa dec0 dec ...) (xml-make-fa (map symbol->string (syntax->datum #'(dec0 dec ...))))]
    [(over dec odec0 odec ...) (xml-make-over (symbol->string (syntax->datum #'dec))
                                              (map symbol->string (syntax->datum #'(odec0 odec ...))))]
    [else (raise-syntax-error 'Policy "Invalid combination type. Must be (fa ...) or (over ...)" #f #f (list comb))]))




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
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Children
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define the-child-policies
         (let ()    
           (define the-children-clause (first the-children-clauses))
           (define the-children (rest (syntax-e the-children-clause)))               
           
           ; Let the macro system expand these
           the-children)) 
       
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
               (list `(xml-make-command "SET TARGET FOR POLICY" 
                                        (list (xml-make-policy-identifier local-policy-id)
                                              ,(m-formula->xexpr the-target)))))))    
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; PComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define pcomb-result 
         (if (empty? the-pcomb-clauses)
             empty
             (let ()
               (define the-pcomb-clause (first the-pcomb-clauses)) 
               (syntax-case the-pcomb-clause [PComb]
                 [(PComb x0 x ...) (list `(xml-make-command "SET PCOMBINE FOR POLICY" 
                                                            (list (xml-make-policy-identifier local-policy-id) 
                                                                  ,(xml-make-comb-list (map handle-combine (syntax->list #'(x0 x ...)))))))]
                 [_ (raise-syntax-error 'PolicySet "Invalid policy-combination clause." #f #f (list the-pcomb-clause))]))))
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Create
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define create-result           
         (list
          `(xml-make-command "CREATE POLICY SET" (list (xml-make-policy-identifier local-policy-id)
                                                       ,(xml-make-vocab-identifier (symbol->string (syntax->datum #'vocabname)))))))
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Prepare
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define prepare-result 
         `(xml-make-command "PREPARE" (list (xml-make-policy-identifier local-policy-id))))   
       
       ; Macro returns a lambda that takes a filename and a syntax object.   
       ; This is so we know where to find the vocabulary file. Only know that at runtime
       (with-syntax ([my-commands (append create-result ; create must come first                                                                                   
                                          target-result
                                          pcomb-result)]
                     [my-prepare-command (list prepare-result)]
                     [the-child-policies #`(list #,@the-child-policies)]
                     [vocabname #'vocabname]
                     
                     ; can't include Policy here or else it gets macro-expanded (inf. loop)
                     ; smuggle in location info and re-form syntax if we need to throw an error
                     [orig-stx-line #`#,(syntax-line stx)]
                     [orig-stx-column #`#, (syntax-column stx)]
                     [orig-stx-source #`#, (syntax-source stx)]
                     [orig-stx-position #`#, (syntax-position stx)]
                     [orig-stx-span #`#, (syntax-span stx)])     
         
         (syntax/loc stx 
           ; Don't quote the lambda. Un-necessary (and would force evaluate-policy to double-eval)
           (lambda (local-policy-filename local-policy-id src-syntax)                                                                        
             (define vocab-path (build-path (path-only/same local-policy-filename) 
                                            (string-append (symbol->string 'vocabname) ".v")))
             
             ; Produce a friendly error if the vocab doesn't exist
             ; src-syntax here is the *command* that spawned the loading, not the Policy form.
             (file-exists?/error vocab-path src-syntax 
                                 (format "The policyset's vocabulary did not exist. Expected: ~a" (path->string vocab-path)))   
             
             (define my-vocab                 
               (call-with-input-file
                   vocab-path
                 (lambda (in-port) 
                   (port-count-lines! in-port)
                   (define the-vocab-syntax (read-syntax vocab-path in-port))               
                   ; Keep as syntax here, so the PolicyVocab macro gets the right location info
                   ; margrave-policy-vocab-namespace is provided to the module that evaluates the code we're generating here
                   (eval the-vocab-syntax margrave-policy-vocab-namespace))))  
             
             ; (vocab-name xml-list types-names predicates-names-and-arities constants-names functions-names-and-arities)
             (define vocab-name (first my-vocab))
             (define vocab-commands (second my-vocab))
             
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
               (datum->syntax #f placeholder
                              (list 'orig-stx-source orig-stx-line orig-stx-column orig-stx-position orig-stx-span)))                                              
             
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
               (xml-make-command "ADD" (list `(PARENT local-policy-id
                                                      ,(xml-make-child-identifier (first tuple))))))
             
             (define my-commands-without-child 'my-commands)
             
             ; Forge the connection between parent and child policies
             ; and *prefix* this policy's commands with the child commands
             ; *FINALLY* prepare the policy.
             (define child-commands (append* (map fourth the-children)))
             (define my-commands-with-children (append child-commands
                                                       my-commands-without-child
                                                       (map get-add-child-xml the-children)
                                                       'my-prepare-command))                          
             
             ;    (define child-polnames (flatten (map get-child-pols the-children)))
             
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
             (for-each (lambda (tuple)
                         (unless (equal? (second tuple) (symbol->string 'vocabname))
                           (raise-syntax-error 
                            'Policy 
                            (format "Child policy used a vocabulary other than \"~a\". Child vocabulary must match parent vocabulary." 'vocabname)                
                            (make-placeholder-syntax (second tuple)))))
                       the-children)
             
             
             
             ; Return list(pname, vname, list-of-commands-for-vocab, list-of-commands-for-policy, list(child-pname, child-pcmds), list(child-vname, child-vcmds))
             `( ,local-policy-id
                ,(symbol->string 'vocabname)
                ,vocab-commands
                ,my-commands-with-children
                ,my-commands
                ,child-polnames
                ))))))))

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
       (define axioms-xml (map (lambda (axiom) (make-axiom-command (->string #'theoryname) axiom)) the-axioms-clauses))
       
       (with-syntax ([axioms-xml axioms-xml]
                     [theory-name (->string #'theoryname)]                     
                     [the-vocab-syntax the-vocab-syntax]                     
                     [the-axioms-list the-axioms-clauses])                                               
         (syntax/loc stx (m-theory theory-name 'axioms-xml the-vocab-syntax 'the-axioms-list)))))))

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
                                                        [_ #f]))                                        
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
           [t (not (capitalized-id-syn? #'t))
              (raise-syntax-error 'Vocab err-invalid-type-decl-case #f #f (list a-type))] 

           
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
           (define the-children (flatten (map m-type-child-names duplicate-types)))
           (define unmentioned-children (filter (lambda (c) (not (member c the-sort-names))) the-children))
           (define unmentioned-mtypes (map (lambda (c) (m-type c empty)) unmentioned-children))    
           (append unmentioned-mtypes (list (m-type name the-children))))
         
         (flatten (map combine-same-types the-sort-names)))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define types-result-list (resolve-m-types (map handle-type the-types)))
       
       (define (type-aggregator ele sofar)
         (hash-set sofar (m-type-name ele) ele))
       (define types-result (foldl type-aggregator (make-immutable-hash '()) types-result-list))
       
       (define types-cmds (map (lambda (x) 
                                 (m-type->cmd vocab-name-string x))
                               types-result-list))
       
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
       
       
       (define predicates-cmds (if (empty? predicates-result)
                                   empty
                                   (map (lambda (x) 
                                          (m-predicate->cmd vocab-name-string x)) predicates-result-list)))       
       
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
       
       (define constants-cmds (if (empty? constants-result)
                                  empty
                                  (map (lambda (x) (m-constant->cmd vocab-name-string x)) constants-result-list)))              
       
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
       
       (define functions-cmds (if (empty? functions-result)
                                  empty
                                  (map (lambda (x) (m-function->cmd vocab-name-string x)) functions-result-list)))       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
       ; We have no idea whether the vocabulary has been created yet or not. 
       ; Java will handle creation of the object if the identifier hasn't been seen before.                            
              
       ;;;;;;;;; Final Syntax ;;;;;;;;;
       (with-syntax ([vocab-name-string vocab-name-string]
                     [xml-list (append types-cmds predicates-cmds constants-cmds functions-cmds)]
                     [types-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-type-name ele) `',(disassemble-transparent-struct ele))) (hash-values types-result))))]
                     [predicates-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-predicate-name ele) `',(disassemble-transparent-struct ele))) (hash-values predicates-result))))]
                     [constants-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-constant-name ele) `',(disassemble-transparent-struct ele))) (hash-values constants-result))))]
                     [functions-disassembled-hash
                      #`(hash #,@(apply append (map (lambda (ele) (list (m-function-name ele) `',(disassemble-transparent-struct ele))) (hash-values functions-result))))])     
         
         (syntax/loc stx (m-vocabulary vocab-name-string 
                                       'xml-list
                                       (assemble-struct-hash types-disassembled-hash)
                                       (assemble-struct-hash predicates-disassembled-hash)
                                       (assemble-struct-hash constants-disassembled-hash)
                                       (assemble-struct-hash functions-disassembled-hash))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Policy: Parses a policy definition and creates an MPolicyLeaf OR MPolicySet object
; Policies are permitted to have child policies, so this may be recursively called

(define-syntax (Policy stx)
  (syntax-case stx [Target Rules = :- uses RComb Policy Variables]
    [(_ uses a-vocab-or-thy-name clauses ... )
     
     (let ()
       (unless (identifier? #'a-vocab-or-thy-name)
         (raise-syntax-error 'Policy (format "Expected a theory or vocabulary name for the policy to use, got: ~a"
                                             (syntax->datum #'a-vocab-or-thy-name)) #f #f (list #'a-vocab-or-thy-name)))                          
       
       (define my-theory-name-sym (syntax->datum #'a-vocab-or-thy-name))     
       (define my-theory-name-str (->string my-theory-name-sym)) 
       
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Target Rules = :- RComb PComb Children Variables]
                                                        [(Variables vardec ...) 'variables]
                                                        [(Target targ ...) 'target]                   
                                                        [(Rules rule ...) 'rules]
                                                        [(RComb comb ...) 'rcomb] 
                                                        [_ #f]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(variables target rules rcomb)))
       
       ;(printf "Policy Clause list: ~a~n" (syntax->list #'(clauses ...)))
       ;(printf "Policy Clause table: ~n~a~n" clause-table)
       
       (define the-variables-clauses (hash-ref clause-table 'variables))                            
       (define the-target-clauses (hash-ref clause-table 'target))                                 
       (define the-rules-clauses (hash-ref clause-table 'rules))
       (define the-rcomb-clauses (hash-ref clause-table 'rcomb))
       
       (assert-one-clause stx the-variables-clauses "Variables")
       (assert-lone-clause stx the-target-clauses "Target")
       (assert-one-clause stx the-rules-clauses "Rules")
       (assert-lone-clause stx the-rcomb-clauses "RComb")       
       
       
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
                    (m-vardec (->string #'lowid) (->string #'capid))]
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
             empty
             (let ()               
               (define the-target-clause (first the-target-clauses))
               (define the-targets (rest (syntax-e the-target-clause)))
               (when (> (length the-targets) 1)
                 (raise-syntax-error 'Policy "Only one target formula can be given, but had more than one." #f #f (list the-target-clause)))
               (when (< (length the-targets) 1)
                 (raise-syntax-error 'Policy "The Target clause must contain a formula if it is given." #f #f (list the-target-clause)))
               
               (define the-target (first the-targets))                                             
               (list `(xml-make-command "SET TARGET FOR POLICY" 
                                        (list (xml-make-policy-identifier local-policy-id)
                                              ',(m-formula->xexpr the-target)))))))    
       
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Rules
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define rules-result-list
         (if (empty? the-rules-clauses)
             empty
             (let ()    
               (define the-rules-clause (first the-rules-clauses))
               (define the-rules (rest (syntax-e the-rules-clause)))
               
               (define (handle-rule a-rule)
                 (syntax-case a-rule [= :-]
                   [(rulename = (decision rvar ...) :- fmla0 fmla ...)                                         
                            
                    (m-rule (->string #'rulename)
                            (->string #'decision)
                            (map ->string (syntax->list #'(rvar ...)))
                            (syntax->list #'(fmla0 fmla ...)))]
                   [_ (raise-syntax-error 'Policy "Rule form did not have the expected shape." #f #f (list a-rule))]))
               
               (map handle-rule the-rules))))
       
       (define (rules-aggregator ele sofar)
         (hash-set sofar (m-rule-name ele) ele))
       (define rules-hash (foldl rules-aggregator (make-immutable-hash '()) rules-result-list))

       (define (internal-get-variable-sort varid)
         (unless (hash-has-key? vardec-hash varid)
           (margrave-error "Variable was not declared" varid))
         (m-vardec-type (hash-ref vardec-hash varid)))
       
       (define (idb-aggregator ele sofar) 
         (define idbarity (map internal-get-variable-sort (m-rule-headvars ele)))
         (when (and (hash-has-key? sofar (m-rule-decision ele))
                    (not (equal? idbarity (hash-ref sofar (m-rule-decision ele)))))
           (margrave-error "The rule used a decision that was used previously, but with a different free variable arity" ele))                         
         (hash-set sofar (m-rule-decision ele) idbarity))        
       (define idbs-hash (foldl idb-aggregator (make-immutable-hash '()) rules-result-list))              
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; RComb
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define (handle-combine comb)         
         (syntax-case comb [fa over]         
           [(fa dec0 dec ...) (xml-make-fa (map symbol->string (syntax->datum #'(dec0 dec ...))))]
           [(over dec odec0 odec ...) (xml-make-over (symbol->string (syntax->datum #'dec))
                                                     (map symbol->string (syntax->datum #'(odec0 odec ...))))]
           [else (raise-syntax-error 'Policy "Invalid combination type. Must be (fa ...) or (over ...)" #f #f (list comb))]))
       
       (define rcomb-result 
         (if (empty? the-rcomb-clauses)
             empty
             (let ()
               (define the-rcomb-clause (first the-rcomb-clauses))
               (syntax-case the-rcomb-clause [RComb]
                 [(RComb x0 x ...) (list `(xml-make-command "SET RCOMBINE FOR POLICY" 
                                                            (list (xml-make-policy-identifier local-policy-id) 
                                                                  ',(xml-make-comb-list (map handle-combine (syntax->list #'(x0 x ...)))))))]
                 [_ (raise-syntax-error 'Policy "Invalid rule-combination clause." #f #f (list the-rcomb-clause))]))))
       
       ; !!! How to make fa, over case insensitive?
       
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ; Create and Prepare
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (define create-xexpr `(xml-make-command "CREATE POLICY LEAF" 
                                                (list (xml-make-policy-identifier local-policy-id)
                                                      (xml-make-vocab-identifier ,my-theory-name-str))))
       
       (define prepare-xexpr 
         `(xml-make-command "PREPARE" (list (xml-make-policy-identifier local-policy-id))))
       
       ; Only know filename and policy id at *runtime*                    
       
       ; need ,(list (xml-make-command ...
       ; so do `(list ,@( ...))
       ; (The vocab command list doesn't have unquoting since everything is known about the vocab at compile time)
       ; (create must come first; prepare last)
       (with-syntax ([start-commands `(list ,create-xexpr)]
                     [end-commands `(list ,@(append target-result
                                                    rcomb-result                                                   
                                                    (list prepare-xexpr)))]
                     [my-theory-name my-theory-name-sym]
                     [disassembled-rules-hash #`(hash #,@(apply append (map (lambda (ele) (list (m-rule-name ele) `',(disassemble-transparent-struct ele))) (hash-values rules-hash))))]
                     [disassembled-vardec-hash #`(hash #,@(apply append (map (lambda (ele) (list (m-vardec-name ele) `',(disassemble-transparent-struct ele))) (hash-values vardec-hash))))]                     
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
           (lambda (local-policy-filename local-policy-id src-syntax)                                                                        
             (define s-VOCAB-EXTENSION ".v")
             
             (define vocab-path (build-path (path-only/same local-policy-filename) 
                                            (string-append (symbol->string 'my-theory-name) s-VOCAB-EXTENSION)))           
             
             ; Produce a friendly error if the vocab doesn't exist
             ; src-syntax here is the *command* that spawned the loading, not the Policy form.
             (file-exists?/error vocab-path src-syntax 
                                 (format "Unable to find the policy's vocabulary. Expected a file at: ~a" (path->string vocab-path)))   
             
             (define my-t-or-v                 
               (call-with-input-file
                   vocab-path
                 (lambda (in-port) 
                   (port-count-lines! in-port)
                   (define the-vocab-syntax (read-syntax vocab-path in-port))  ; (Vocab ...)                    
                   ; Keep as syntax here, so the PolicyVocab macro gets the right location info
                   ; margrave-policy-vocab-namespace is provided to the module that evaluates the code we're generating here
                   (eval the-vocab-syntax margrave-policy-vocab-namespace))))
             
             ; Allow users to provide just a vocab with no theory.
             (define my-thy
               (if (m-vocabulary? my-t-or-v)
                   (m-theory (m-vocabulary-name my-t-or-v) empty my-t-or-v empty)
                   my-t-or-v))
             
             (define (make-placeholder-syntax placeholder)
               (datum->syntax #f placeholder
                              (list 'orig-stx-source orig-stx-line orig-stx-column orig-stx-position orig-stx-span))) 
             
             ; !!! TODO: these fields of the m-policy structure need to be populated still             
             (define rcomb-desc "")
             (define target-fmla 'true)   
                         
             
             (define vardec-hash (assemble-struct-hash disassembled-vardec-hash))             
             (define rules-hash (assemble-struct-hash disassembled-rules-hash))             
             
             (m-policy local-policy-id ; [id string?]                      
                       vocab-path ; [theory-path path?]
                       ; [xml (listof xexpr?)]
                       (append start-commands
                               (map (lambda (ele) (m-vardec->cmd local-policy-id ele)) (hash-values vardec-hash))
                               (map (lambda (ele) (m-rule->cmd local-policy-id ele)) (hash-values rules-hash))
                               end-commands)
                       my-thy ; [theory m-theory?]  
                       vardec-hash ; [vardecs (hash/c string? m-vardec?)]
                       rules-hash ; [rules (hash/c string? m-rule?)]
                       rcomb-desc ; [rcomb string?]
                       target-fmla ; [target m-formula?]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Inefficient, but works -- compute transitive closure of sort hierarchy.
(define/contract (m-type-child-names/rtrans voc sname)
  [m-vocabulary? (or/c symbol? string?) . -> . (listof string?)]
  (unless (hash-has-key? (m-vocabulary-types voc) (->string sname))
    (margrave-error (format "Unknown type ~v in vocabulary ~v." (->string sname) voc) sname))
  (define the-type (hash-ref (m-vocabulary-types voc) (->string sname)))
  (define child-names (m-type-child-names the-type)) 
  ;(printf "child-names: ~v~n" child-names)
  (define result (append (list sname) ; *reflexive* transitive closure
                         ; do not include child-names explicitly; the map below will add them
                         (flatten 
                          (map (lambda (childname) (m-type-child-names/rtrans voc childname)) child-names))))
  ;(printf "result: ~v~n" result)
  (remove-duplicates result))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What is the sort of this term? If the term is not well-sorted, throw a suitable error.
(define/contract (m-term->sort/err voc term env)
  [m-vocabulary? any/c hash? . -> . (or/c symbol? boolean?)]
  (match term
    [(or `(,(? valid-function? funcid) ,@(list (? m-term->xexpr terms) ...))
         (syntax-list-quasi ,(? valid-function? funcid) ,@(list (? m-term->xexpr terms) ...)))
     (unless (hash-has-key? (m-vocabulary-functions voc) funcid)
       (margrave-error "The function symbol was not declared in the vocabulary context" term))   
     (define thefunc (hash-ref (m-vocabulary-functions voc) funcid))     
     (define pairs-to-check (zip terms (m-function-arity thefunc)))
     (for-each (lambda (p) 
                 (define-values (p-term p-sort) (values (first p) (second p)))
                 (unless (member? (m-term->sort/err voc p-term env)
                                  (m-type-child-names/rtrans voc p-sort))
                   (margrave-error (format "The subterm ~v did not have the correct sort to be used in ~v" p-term term ) term)))
               pairs-to-check)                    
     (string->symbol (m-function-result thefunc))]
    [(? valid-constant? cid) 
     (define unquoted-id-str (->string (extract-constant-id cid)))     
     (unless (hash-has-key? (m-vocabulary-constants voc) unquoted-id-str)
       (margrave-error "The constant symbol was not declared in the query's vocabulary context" term))     
     (string->symbol (m-constant-type (hash-ref (m-vocabulary-constants voc) unquoted-id-str)))]
    [(? valid-variable? vid) 
     (unless (hash-has-key? env term)
       (margrave-error (format "The variable was not declared in the environment (~v)" env) term))     
     (hash-ref env term)]
    [else (margrave-error (format "The term ~v was not well-sorted. Environment was: ~v." term env) term)]))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gather the set of policy references used in this formula
(define/contract (gather-policy-references fmla)
  [m-formula? . -> . set?]
  (match fmla
    [(maybe-identifier true)
     (set)]        
    [(maybe-identifier false)
     (set)]            
    [(m-op-case = t1 t2)
     (set)]    
    [(m-op-case and args ...)
     (apply set-union (map gather-policy-references args))]    
    [(m-op-case or args ...)
     (apply set-union (map gather-policy-references args))]
    [(m-op-case implies arg1 arg2)
     (apply set-union (gather-policy-references arg1) (gather-policy-references arg2))]  
    [(m-op-case iff arg1 arg2)
     (apply set-union (gather-policy-references arg1) (gather-policy-references arg2))]   
    [(m-op-case not arg)
     (gather-policy-references arg)]       
    [(m-op-case forall vname sname subfmla)
     (gather-policy-references subfmla)]     
    [(m-op-case exists vname sname subfmla)
     (gather-policy-references subfmla)]               
    [(m-op-case isa vname sname subfmla)
     (gather-policy-references subfmla)]     
    
    ; IDB: gather policy ids used!
    ;[(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,term0 ,@(list terms ...))
    ; (set (list pids))]     
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,term0 ,@(list terms ...))
     (list->set pids)] ; will be empty set if saved-query IDB  
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (set)]         
    [else (margrave-error "Invalid formula given to gather-policy-references" fmla)]))

(check-true (set-empty? (gather-policy-references '(and (= x y) (r x y z)))))
(check-false (set-empty? (gather-policy-references '(or false ((MyPolicy permit) x y z)))))
(check-true (equal? 2 (set-count (gather-policy-references '(or ((MyOtherPolicy deny) z y x) ((MyPolicy permit) x y z))))))

; Gather the set of prior query references used in this formula
(define/contract (gather-query-references fmla)
  [m-formula? . -> . set?]
  (match fmla
    [(maybe-identifier true)
     (set)]        
    [(maybe-identifier false)
     (set)]            
    [(m-op-case = t1 t2)
     (set)]    
    [(m-op-case and args ...)
     (apply set-union (map gather-query-references args))]    
    [(m-op-case or args ...)
     (apply set-union (map gather-query-references args))]
    [(m-op-case implies arg1 arg2)
     (apply set-union (gather-query-references arg1) (gather-query-references arg2))]  
    [(m-op-case iff arg1 arg2)
     (apply set-union (gather-query-references arg1) (gather-query-references arg2))]   
    [(m-op-case not arg)
     (gather-query-references arg)]       
    [(m-op-case forall vname sname subfmla)
     (gather-query-references subfmla)]     
    [(m-op-case exists vname sname subfmla)
     (gather-query-references subfmla)]               
    [(m-op-case isa vname sname subfmla)
     (gather-query-references subfmla)]     
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,term0 ,@(list terms ...))
     (if (empty? pids)
         (set idbname)
         (set))]     
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (set)]         
    [else (margrave-error "Invalid formula given to gather-policy-references" fmla)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get a cached policy by ID. If no such policy exists, throw a suitable error.
(define (get-cached-policy/err pid)  
  (unless (hash-has-key? cached-policies (->string pid))
    (margrave-error "No such policy" pid))
  (hash-ref cached-policies (->string pid)))
; same for cached prior query
(define (get-prior-query/err qid)  
  (unless (hash-has-key? cached-prior-queries (->string qid))
    (margrave-error "No prior query saved under that name" qid))
  (hash-ref cached-prior-queries (->string qid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Avoid duplicate code. Defer to m-formula-is-well-sorted/err
(define (m-formula-is-well-sorted? voc sexpr env)
  (with-handlers ([(lambda (e) (exn:fail:syntax? e))
                   (lambda (e) #f)]
                  [(lambda (e) (exn:fail:user? e))
                   (lambda (e) #f)])
    (m-formula-is-well-sorted?/err sexpr)))

; If the formula is not well-sorted, throw a suitable error.
(define/contract (m-formula-is-well-sorted?/err voc fmla env)
  [m-vocabulary? any/c hash? . -> . boolean?]      
    
  ; Check to see if term <tname> can "fit" as sort <sname>
  (define/contract (internal-correct tname sname)
    [any/c string? . -> . boolean?]        
    (define valid-sort-names (m-type-child-names/rtrans voc sname))
    (define term-sort-str (->string (m-term->sort/err voc tname env)))
    ;(printf "internal-correct checking: ~v ~v ~v ~v ~v~n" tname sname valid-sort-names term-sort-str (member? term-sort-str valid-sort-names))
    (unless (member? term-sort-str valid-sort-names)
      (margrave-error (format "This formula was not well-sorted. The term was of type ~v, but expected to be of type ~v" term-sort-str sname) tname))
    #t)
  
  ; Handle case: (isa x A true)
  (define/contract (internal-correct/isa-sugar vname sname)
    [any/c string? . -> . boolean?]  
    ; sugary isa formulas are always well-sorted.
    ;(internal-correct vname sname)
    #t)
  
  ; Handle case: (edbname x y z)
  (define (internal-correct/edb list-of-vars edbname)
    (unless (hash-has-key? (m-vocabulary-predicates voc) edbname)
      (margrave-error "The predicate was not defined in the vocabulary context" fmla))
    (define mypred (hash-ref (m-vocabulary-predicates voc) edbname))    
    (define pairs-to-check (zip list-of-vars (m-predicate-arity mypred)))
    (andmap (lambda (p) (internal-correct (first p) (second p))) pairs-to-check))
  
  ; Handle case: ( (polname idbname) x y z)
  ; todo for now: only support single element in polidlist
  ; need to access cached policies
  (define (internal-correct/idb list-of-vars pol-id-list idbname-pre)
    (define idbname (->string idbname-pre))
    ;(printf "internal-correct/idb: ~v ~v ~v~n" list-of-vars pol-id-list idbname-pre)
    (define the-idbs 
      (cond 
      [(empty? pol-id-list) 
       ; saved-query IDB
       (define this-prior-query (get-prior-query/err idbname))
       (unless (hash-has-key? (m-prior-query-idbs this-prior-query) idbname)
         (margrave-error (format "Saved query ~v did not contain the IDB ~v. It contained: ~v" idbname idbname (m-prior-query-idbs this-prior-query)) idbname))
       (m-prior-query-idbs this-prior-query)]
      [else
       ; policy IDB
       (define this-policy (get-cached-policy/err (first pol-id-list)))
       (unless (hash-has-key? (m-policy-idbs this-policy) idbname)
         (margrave-error (format "Policy ~v did not contain the IDB ~v. It contained: ~v" pol-id-list idbname (m-policy-idbs this-policy)) idbname))
       (m-policy-idbs this-policy)]))        
    ;(printf "the idbs ~v~n" the-idbs)
    
    (define my-arity (hash-ref the-idbs idbname)) ; these will be strings
    (define pairs-to-check (zip list-of-vars my-arity)) ; symbol . string            
    (andmap (lambda (p)                  
              (internal-correct (first p) (second p))) pairs-to-check))
    
    
  ; Checking starts here:
  (match fmla
    [(maybe-identifier true)
     #t]        
    [(maybe-identifier false)
     #t]        
    
    [(m-op-case = t1 t2)
     (m-term->sort/err voc t1 env) 
     (m-term->sort/err voc t2 env)
     #t]
    
    [(m-op-case and args ...)
     (andmap (lambda (f) (m-formula-is-well-sorted?/err voc f env)) args)]    
    [(m-op-case or args ...)
     (andmap (lambda (f) (m-formula-is-well-sorted?/err voc f env)) args)]
    [(m-op-case implies arg1 arg2)
     (and (m-formula-is-well-sorted?/err voc arg1 env) (m-formula-is-well-sorted?/err voc arg2 env))]   
    [(m-op-case iff arg1 arg2)
     (and (m-formula-is-well-sorted?/err voc arg1 env) (m-formula-is-well-sorted?/err voc arg2 env))]   
    [(m-op-case not arg)
     (m-formula-is-well-sorted?/err voc arg env)]   
    
    [(m-op-case forall vname sname subfmla)
     (m-formula-is-well-sorted?/err voc subfmla (hash-set env vname sname))]     
    [(m-op-case exists vname sname subfmla)
     (m-formula-is-well-sorted?/err voc subfmla (hash-set env vname sname))]  
    ; If (isa x A alpha) is sugar for (exists y A (and (= x y) alpha[x -> y]))
    ; the sort of x must be _replaced_, not augmented, by the new sort.
    [(m-op-case isa vname sname subfmla)
     (m-formula-is-well-sorted?/err voc subfmla (hash-set env vname sname))]     
    
    ; (idb term0 terms ...)    
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,term0 ,@(list terms ...))
     (internal-correct/idb (cons term0 terms) pids idbname)] 
    
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (cond
       [(and (valid-sort? dbname) 
             (empty? terms)
             (valid-variable? term0))
        (internal-correct/isa-sugar term0 (->string dbname))] ; sugar for (isa x A true)      
       [else (internal-correct/edb (cons term0 terms) (->string dbname))])] ; (edb term0 terms ...)
    
    [else (margrave-error "This formula was not well-sorted" fmla) ]))

(define/contract (union-types-hashes h1 h2)
  [hash? hash? . -> . (and/c hash? (not/c immutable?))]
  (define result (hash-copy h1))
  (for ([key (hash-keys h2)])
    (cond
      [(not (hash-has-key? result key)) (hash-set! result key (hash-ref h2 key))]
      [else 
       (define type1 (hash-ref h1 key))
       (define type2 (hash-ref h2 key))
       (hash-set! result key (m-type key (remove-duplicates (append (m-type-child-names type1)
                                                                    (m-type-child-names type2)))))]))   
  result)

(define/contract (combine-vocabs v1 v2)
  [m-vocabulary? m-vocabulary? . -> . m-vocabulary?]
  (define new-name (string-append (m-vocabulary-name v1) "+" (m-vocabulary-name v2)))
  
  ; Should never be used...
  (define new-xml empty)
  
  ; can't use hash-union since we want to allow (and check) overlaps
  (define new-types (union-types-hashes (m-vocabulary-types v1) (m-vocabulary-types v2)))
  (define new-predicates (hash-union/overlap (m-vocabulary-predicates v1) (m-vocabulary-predicates v2) "Predicates did not match"))
  (define new-constants (hash-union/overlap (m-vocabulary-constants v1) (m-vocabulary-constants v2) "Constants did not match"))
  (define new-functions (hash-union/overlap (m-vocabulary-functions v1) (m-vocabulary-functions v2) "Functions did not match"))
  
  (m-vocabulary new-name new-xml new-types new-predicates new-constants new-functions))

(check-true (equal? (combine-vocabs (m-vocabulary "v1" empty (hash) (hash) (hash) (hash)) 
                                    (m-vocabulary "v2" empty (hash) (hash) (hash) (hash)))
                    (m-vocabulary "v1+v2" empty (make-hash) (make-hash) (make-hash) (make-hash))))
(check-true (equal? (combine-vocabs (m-vocabulary "v1" empty (hash "A" (m-type "A" empty)) (hash) (hash) (hash)) 
                                    (m-vocabulary "v2" empty (hash "A" (m-type "A" empty)) (hash) (hash) (hash)))
                    (m-vocabulary "v1+v2" '() (make-hash `(("A" ,@(m-type "A" '())))) (make-hash) (make-hash) (make-hash))))
(check-true (equal? (combine-vocabs (m-vocabulary "v1" empty (hash "A" (m-type "A" '("B"))) (hash) (hash) (hash)) 
                                    (m-vocabulary "v2" empty (hash "A" (m-type "A" '("C"))) (hash) (hash) (hash)))
                    (m-vocabulary "v1+v2" '() (make-hash `(("A" ,@(m-type "A" '("B" "C"))))) (make-hash) (make-hash) (make-hash))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that grab the appropriate m-vocabulary from the cache
; One accepts saved query identifiers, the other accepts policy identifiers.
(define/contract (policy-name->m-vocab pname)
  [(or/c symbol? string?) . -> . m-vocabulary?]
  (define pname-str (->string pname))
  (unless (hash-has-key? cached-policies pname-str)
    (margrave-error "Unknown policy identifier" pname))
  (define the-policy (hash-ref cached-policies pname-str))
  (m-theory-vocab (m-policy-theory the-policy)))

(define/contract (prior-query-name->m-vocab qname)
  [(or/c symbol? string?) . -> . m-vocabulary?]
  (define qname-str (->string qname))
  (unless (hash-has-key? cached-prior-queries qname-str)
    (margrave-error "Unknown prior query identifier" qname))
  (m-prior-query-vocab (hash-ref cached-prior-queries qname-str)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Given a formula, compute its vocabulary context
; take policy references _and_ references to prior queries into account.
; This function makes 2 separate passes, for simplicity
(define/contract (get-uber-vocab-for-formula fmla #:under [under-list empty])
  [->* (m-formula?)
       (#:under list?)
       m-vocabulary?]
  (define used-policy-ids (gather-policy-references fmla))                                                     
  (define used-query-ids (gather-query-references fmla))
  
  ;(printf "used: ~v ~v~n" used-policy-ids used-query-ids)
  (define voc-list (remove-duplicates
                    (append (set-map used-policy-ids policy-name->m-vocab)
                            (set-map used-query-ids prior-query-name->m-vocab)
                            (map policy-name->m-vocab under-list))))
  (when (empty? voc-list)
    (margrave-error "The formula contained no vocabulary references. Please use the #:under clause." fmla))
  
  (define f (first voc-list))
  (define r (rest voc-list))
  (foldl combine-vocabs f r))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(expand '(Vocab myvoc (Types (Type X ) (Type Y) (Type Z > A B C)) (Constants (Constant 'c A) (Constant 'c2 X)) (Functions (Function f1 A B) (Function f2 X Y Z)) (Predicates (Predicate r X Y))))
;(expand-once '(Policy uses vocabname (Variables )(RComb (fa Permit Deny)) (Rules (Rule1 = (Permit x y z) :- true))))
; Why not case-insensitive in match?
; (expand-once '(Policy polname uses vocabname (Variables )(RComb (fa Permit Deny)) (Rules (Rule1 = (Permit x y z) :- true) (Rule2 = (Permit y x z) :- (rel x y (f x 'c))))))
;(expand-once '(Policy polname uses vocabname (Variables (Variable x A) (Variable y B) (Variable z C)) (RComb (fa Permit Deny) (over Permit CallPolice) (over Deny CallPolice)) (Rules (Rule1 = (Permit x y z) :- true) (Rule2 = (Permit y x z) :- (rel x y (f x 'c))))))


; ((eval '(Policy uses Conference
;        (Variables 
;         (Variable s Subject)
;         (Variable a Action)
;         (Variable r Resource))
;        (Rules 
;  	  (PaperNoConflict = (Permit s a r) :- (and (not (conflicted s r)) (readPaper a) (paper r)))
;	  (PaperAssigned = (Permit s a r) :- (and (assigned s r) (readPaper a) (paper r)));
;	  (PaperConflict = (Deny s a r) :- (and (conflicted s r) (readPaper a) (paper r))))
;        (RComb (fa permit deny)))) "F:\\msysgit\\git\\Margrave\\margrave\\examples\\conference.v" "MYPOLICYID" #'foo)

; Theory mythy (Vocab myvoc (Types (Type X ) (Type Y) (Type Z > A B C)) (Constants (Constant 'c A) (Constant 'c2 X)) (Functions (Function f1 A B) (Function f2 X Y Z)) (Predicates (Predicate r X Y))) (Axioms (partial-function f1)))

;(Vocab myvoc (Types (Type X ) (Type Y) (Type Z > A B C)) (Constants (Constant 'c A) (Constant 'c2 X)) (Functions (Function f1 A B) (Function f2 X Y Z)) (Predicates (Predicate r X Y)))
