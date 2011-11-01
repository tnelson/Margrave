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
 (file "margrave-xml.rkt")
 (file "helpers.rkt")
 (file "polvochelpers.rkt") 
 racket/list
 racket/contract
 xml
 (only-in srfi/1 zip)
 (for-syntax (only-in srfi/13 string-contains)             
             (file "helpers.rkt")                     
             (file "polvochelpers.rkt")                     
             (file "margrave-xml.rkt")
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
         cached-theories)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cached-policies (make-hash))
(define cached-vocabularies (make-hash))
(define cached-theories (make-hash))

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
    
    ;(printf "evaluate-policy: ~a~n" the-policy-syntax)
    
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
       (define axioms-xml (map m-axiom->xexpr the-axioms-clauses))
       
       (with-syntax ([axioms-xml axioms-xml]
                     [theory-name (->string 'theoryname)]                     
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
         (syntax-case a-type [Type >]     
           
           ; (Type T)
           [(Type t) (capitalized-id-syn? #'t)
                     (m-type (symbol->string (syntax->datum #'t)) empty)]  
           
           [(Type t) (not (capitalized-id-syn? #'t))
                     (raise-syntax-error 'Vocab err-invalid-type-decl-case #f #f (list a-type))] 
           ; (Type T > A B C)
           [(Type t > subt ...) (and (capitalized-id-syn? #'t)
                                     (all-are-syn? #'(subt ...) capitalized-id-syn?))                                
                                (m-type (symbol->string (syntax->datum #'t))
                                        (map (compose symbol->string syntax->datum) (syntax-e #'(subt ...))))] 
           [(Type t > subt ...) (not 
                                 (and (capitalized-id-syn? #'t)
                                      (all-are-syn? #'(subt ...) capitalized-id-syn?)))
                                (raise-syntax-error
                                 'Vocab err-invalid-type-decl-case #f #f (list a-type)) ] 
           
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
                 ; No colon. Just (Predicate predname A B C)
                 (syntax-case pred []                   
                   [(Predicate pname prel0 prel ...) (and (lower-id-syn? #'pname)
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
                 ; No colon. Just (Constant 'a A)
                 (syntax-case const [Constant]                    
                   [(Constant 'cname crel) (and (lower-id-syn? (syntax cname))
                                                (is-valid-type? (syntax crel)))  
                                           (m-constant (syntax->string #'cname)
                                                       (syntax->string #'crel))] 
                   
                   [(Constant 'cname crel) (not (lower-id-syn? (syntax cname)))
                                           (raise-syntax-error `Vocab err-invalid-constant #f #f (list const) )]
                   [(constant cname crel) (raise-syntax-error `Vocab "Invalid constant name. Constant names must be preceded with a single quote. For instance: (Constant 'c A)" #f #f (list const) )]
                   
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
                 ; No colon. Just (function a A B)
                 (syntax-case func [Function]                   
                   [(Function fname frel0 frel ...) (and (lower-id-syn? #'fname)
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
                     [types-result #`(hash #,@(flatten (map (lambda (ele) (list (m-type-name ele) (repackage-transparent-struct ele))) (hash-values types-result))))]
                     [predicates-result #`(hash #,@(flatten (map(lambda (ele) (list (m-predicate-name ele) (repackage-transparent-struct ele))) (hash-values predicates-result))))]
                     [constants-result #`(hash #,@(flatten (map (lambda (ele) (list (m-constant-name ele) (repackage-transparent-struct ele))) (hash-values constants-result))))]
                     [functions-result #`(hash #,@(flatten (map (lambda (ele) (list (m-function-name ele) (repackage-transparent-struct ele))) (hash-values functions-result))))])     
         
         (syntax/loc stx (m-vocabulary vocab-name-string 
                                       'xml-list
                                       types-result
                                       predicates-result
                                       constants-result
                                       functions-result)))))))


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
                 (syntax-case a-var-dec [Variable]
                   [(Variable lowid capid)                                         
                    (and (capitalized-id-syn? #'capid)
                         (lower-id-syn? #'lowid))
                    (m-vardec (->string #'lowid) (->string #'capid))]
                   [(Variable lowid capid)                                         
                    (not (capitalized-id-syn? #'capid))
                    (raise-syntax-error 'Policy "Invalid variable declaration; the variable's type must be capitalized." #f #f (list a-var-dec))]
                   
                   [(Variable lowid capid)                                         
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
       
       (define create-result (list `(xml-make-command "CREATE POLICY LEAF" 
                                                      (list (xml-make-policy-identifier local-policy-id)
                                                            (xml-make-vocab-identifier ,my-theory-name-str)))))   
       
       
       (define prepare-result 
         (list `(xml-make-command "PREPARE" (list (xml-make-policy-identifier local-policy-id)))))
       
       ; Only know filename and policy id at *runtime*                    
       
       ; need ,(list (xml-make-command ...
       ; so do `(list ,@( ...))
       ; (The vocab command list doesn't have unquoting since everything is known about the vocab at compile time)
       ; (create must come first; prepare last)
       (with-syntax ([some-commands `(list ,@(append create-result                                                    
                                                   target-result
                                                   rcomb-result                                                   
                                                   prepare-result))]
                     [my-theory-name my-theory-name-sym]
                     [rules-hash #`(hash #,@(flatten (map (lambda (ele) (list (m-rule-name ele) (repackage-transparent-struct ele))) (hash-values rules-hash))))]
                     [vardec-hash #`(hash #,@(flatten (map (lambda (ele) (list (m-vardec-name ele) (repackage-transparent-struct ele))) (hash-values vardec-hash))))]
                     
                     [idbs-hash #`(hash #,@(apply append (map (lambda (key) (list key (hash-ref idbs-hash key))) (hash-keys idbs-hash))))]
                     
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
                                
             (m-policy local-policy-id ; [id string?]                      
                       vocab-path ; [theory-path path?]
                       ; [xml (listof xexpr?)]
                       (append (map (lambda (ele) (m-vardec->cmd local-policy-id ele)) (hash-values vardec-hash))
                               (map (lambda (ele) (m-rule->cmd local-policy-id ele)) (hash-values rules-hash))
                        some-commands)
                       my-thy ; [theory m-theory?]  
                       vardec-hash ; [vardecs (hash/c string? m-vardec?)]
                       rules-hash ; [rules (hash/c string? m-rule?)]
                       rcomb-desc ; [rcomb string?]
                       target-fmla ; [target m-formula?]
                       idbs-hash ;  [idbs (hash/c string? m-predicate?)]
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
(define/contract (m-type-child-names/trans voc sname)
  [m-vocabulary? string? . -> . (listof string?)]
  (define child-names (m-type-child-names sname))
  (define child-types (map (lambda (n) (hash-ref (m-vocabulary-types voc) n))
                           child-names))
  (append child-names 
          (map m-type-child-names/trans child-types)))

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
                                  (m-type-child-names/trans voc p-sort))
                   (margrave-error (format "The subterm ~v did not have the correct sort to be used in ~v" p-term term ) term)))
               pairs-to-check)                    
     (m-function-result thefunc)]
    [(? valid-constant? cid) 
     (unless (hash-has-key? (m-vocabulary-constants voc) cid)
       (margrave-error "The constant symbol was not declared in the vocabulary context" term))     
     (m-constant-type (hash-ref (m-vocabulary-constants voc) cid))]
    [(? valid-variable? vid) 
     (unless (hash-has-key? env term)
       (margrave-error "The variable was not declared in the vocabulary context" term))
     (hash-ref env term)]
    [else (margrave-error "This term was not well-sorted" term)]))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get a cached policy by ID. If no such policy exists, throw a suitable error.
(define (get-cached-policy/err pid)
  (unless (hash-has-key? cached-policies pid)
    (margrave-error "No such policy" pid))
  (hash-ref cached-policies pid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If the formula is not well-sorted, throw a suitable error.
(define/contract (m-formula-is-well-sorted/err voc fmla env)
  [m-vocabulary? any/c hash? . -> . boolean?]      
  
  ; Check to see if term <tname> can "fit" as sort <sname>
  (define/contract (internal-correct tname sname)
    [string? string? . -> . boolean?]
    (member? (m-term->sort/err tname) (m-type-child-names/trans voc sname)))
  
  ; Handle case: (isa x A true)
  (define (internal-correct/isa vname sname)
    (internal-correct vname sname))
  
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
  (define (internal-correct/idb list-of-vars pol-id-list idbname)
    (when (empty? pol-id-list) #f)
    (define this-policy (get-cached-policy/err (first pol-id-list)))
    ;(define my-arity (m-policy-idbname->arity this-policy idbname))
    (define my-arity '()) ; placeholder TODO
    (define pairs-to-check (zip list-of-vars my-arity))
    (andmap (lambda (p) (internal-correct (first p) (second p))) pairs-to-check))
  
  (match fmla
    [(or 'true
         (? (make-keyword-predicate/nobind #'true)))
     #t]        
    [(or 'false
         (? (make-keyword-predicate/nobind #'false)))
     #t]        
    
    [(or `(= ,t1 ,t2)
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'=)) ,t1 ,t2))
     (and (m-term->sort/err t1) (m-term->sort/err t2))]
    
    [(or `(and ,@(list args ...))
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'and)) ,@(list args ...)))
     (andmap m-formula-is-well-sorted/err args)]    
    [(or `(or ,@(list args ...))
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'or)) ,@(list args ...)))
     (andmap m-formula-is-well-sorted/err args)]    
    [(or `(implies ,arg1 ,arg2) 
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'implies)) ,arg1 ,arg2))
     (and (m-formula-is-well-sorted/err arg1) (m-formula-is-well-sorted/err arg2))]   
    [(or `(iff ,arg1 ,arg2) 
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'iff)) ,arg1 ,arg2))
     (and (m-formula-is-well-sorted/err arg1) (m-formula-is-well-sorted/err arg2))]   
    [(or `(not ,arg)
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'not)) ,arg))
     (m-formula-is-well-sorted/err arg)]   
    
    [(or `(forall ,vname ,sname ,subfmla) 
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'forall)) ,vname ,sname ,subfmla))
     (m-formula-is-well-sorted/err (hash-set env vname sname) subfmla)] 
    
    [(or `(exists ,vname ,sname ,subfmla) 
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'exists)) ,vname ,sname ,subfmla))
     (m-formula-is-well-sorted/err (hash-set env vname sname) subfmla)]           
    
    ; If (isa x A alpha) is sugar for (exists y A (and (= x y) alpha[x -> y]))
    ; the sort of x must be _replaced_, not augmented, by the new sort.
    [(or `(isa ,vname ,sname ,subfmla) 
         (syntax-list-quasi ,(? (make-keyword-predicate/nobind #'isa)) ,vname ,sname ,subfmla))
     (m-formula-is-well-sorted/err (hash-set env vname sname) subfmla)]     
    
    ; (idb term0 terms ...)
    [(or `(,(list pids ... idbname) ,term0 ,@(list terms ...))
         (syntax-list-quasi ,(list pids ... idbname) ,term0 ,@(list terms ...)))       
     (internal-correct/idb (cons term0 terms) pids idbname)] 
    
    [(or `(,dbname ,term0 ,@(list terms ...)) 
         (syntax-list-quasi ,dbname ,term0 ,@(list terms ...)))
     (cond
       [(and (valid-sort? dbname) 
             (empty? terms)
             (valid-variable? term0))
        (internal-correct/isa term0 dbname)] ; sugar for (isa x A true)      
       [else (internal-correct/edb (cons term0 terms) dbname)])] ; (edb term0 terms ...)
    
    [else (margrave-error "This formula was not well-sorted" fmla) ]))



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

; (eval '(Theory mythy (Vocab myvoc (Types (Type X ) (Type Y) (Type Z > A B C)) (Constants (Constant 'c A) (Constant 'c2 X)) (Functions (Function f1 A B) (Function f2 X Y Z)) (Predicates (Predicate r X Y))) (Axioms (partial-function func))))
