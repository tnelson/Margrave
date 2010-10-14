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
    (printf "Policy Syntax: ~n~a~n" the-policy-syntax)
    
   ; (define the-policy-func (eval-syntax the-policy-syntax margrave-policy-vocab-namespace))
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
     
     ; TN: Removed explicit colon after root types. Changed add-subtypes-of to remove ': 
     ; Makes the syntax cleaner not to require the colon.
    
     (let ()
       (define clause-table (partition* (lambda (stx) (syntax-case stx [Types Decisions Predicates ReqVariables OthVariables Constraints]
                                                        [(Types atype ...) 'types]                         ; (Types (t subt ...) ...)                                
                                                        [(Decisions r ...) 'decisions]
                                                        [(Predicates apreddec ...) 'predicates] ; (pname : prel ...)
                                                        [(ReqVariables avardec ...) 'reqvariables]  ; (rvname : rvsort)
                                                        [(OthVariables avardec ...) 'othvariables] ;(ovname : ovsort)
                                                        [(Constraints acondec ...) 'constraints] ; (ctype crel ...)
                                                        [_ #f]))                                        
                                        (syntax->list #'(clauses ...))
                                        #:init-keys '(types decisions predicates reqvariables othvariables constraints)))
       
       (printf "Clause table: ~n~a~n" clause-table)
       
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
       
       (match the-types-clauses
         [(list) (raise-syntax-error #f "Types clause is missing" stx)]
         [(list x y z ...) (raise-syntax-error #f "More than one Types clause found" #f #f (list* x y z))]
         [_ (void)])
            
       
     #'()
     
     
     ;Return a list containing the vocabname, and then a list of commands to be sent to java
     #;(syntax/loc stx
         `(
           ; Return the object for use by the policy macro
           ,(symbol->string 'myvocabname)
           (
        ; Here, we have no idea whether the vocabulary has been created yet or not. 
        ; Java will handle creation of the object if the identifier hasn't been seen before.
        
        ; These sections must be in order.                     
        ; Types
        ,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-sort (symbol->string 't))))
        ...
        
        ,@(add-subtypes-of (symbol->string 'myvocabname) (symbol->string 't) (list 'subt ...))         
        
        ... ; for each type/subtype set
        
        ; Decisions               
        ,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-decision (symbol->string 'r))))
        ...
        
        ; Predicates
        ,(add-predicate (symbol->string 'myvocabname) (symbol->string 'pname) (list (symbol->string 'prel) ...))
        ... ; for each custom predicate
        
        ; Request Variables
        ,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-request-var (symbol->string 'rvname) (symbol->string 'rvsort))))
        ... ; for each req var
        
        ; Other Variables
        ,(xml-make-command "ADD" (list (xml-make-vocab-identifier (symbol->string 'myvocabname)) (xml-make-other-var (symbol->string 'ovname) (symbol->string 'ovsort))))
        ... ; for each oth var
        
        ; Constraints
        ,(add-constraint (symbol->string 'myvocabname) 'ctype (list (symbol->string 'crel) ...))       
        ... ; for each constraint
        )))))))



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
                 (eval-syntax (read-syntax vocab-path in-port) margrave-policy-vocab-namespace)))]
             
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

(define (add-subtypes-of vocab parent listsubs-dirty)  
  (let ([listsubs-cleaned (filter (lambda (x) (not (equal? x ':))) listsubs-dirty)])
    ; listsubs may be empty -- if so, do nothing (we already added parent)
    (if (> (length listsubs-cleaned) 0)            
        (foldr (lambda (s rest) 
                 ; Is this a sort with subsorts itself?
                 (append 
                  (if (list? s)                       
                      (begin
                        ; (display parent) ; debug
                        ; Add subtype relationship between parent and s
                        (cons (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-subsort parent (symbol->string (car s)))))
                              
                              ; Is this a nested subtype? If so, we must
                              ; deal with s's subtypes.
                              
                              ; Check for list size;
                              ; someone may have used parens without meaning to.
                              (if (> (length s) 1)
                                  (add-subtypes-of vocab (symbol->string (car s)) (cdr s))
                                  empty)))
                      
                      ; Bottom of sort tree. 
                      (begin
                        ;(printf "~a ~a ~a ~n" s listsubs-cleaned parent)
                        (list (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-subsort parent (symbol->string s)))))))
                  rest))
               empty
               listsubs-cleaned)
        empty)))


(define (add-constraint vocab typename listrels)
  ; Switch by typename:
  ; (Assume user is passing appropriate number of arguments)
  
  (cond 
    ; typename is a symbol at this point, not a string    
    ;TODO: make disjoint like subset. Can't rely on ordering
    ((eqv? typename 'disjoint) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "DISJOINT" (list (car listrels) (car (cdr listrels)))))))
    ((eqv? typename 'disjoint-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "DISJOINT-ALL" (list (car listrels) )))))
    ((eqv? typename 'nonempty) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "NONEMPTY" (list (car listrels) )))))
    ((eqv? typename 'nonempty-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "NONEMPTY-ALL" (list (car listrels) )))))
    ((eqv? typename 'singleton) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "SINGLETON" (list (car listrels) )))))
    ((eqv? typename 'singleton-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "SINGLETON-ALL" (list (car listrels) )))))
    ((eqv? typename 'atmostone) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ATMOSTONE" (list (car listrels) )))))
    ((eqv? typename 'atmostone-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ATMOSTONE-ALL" (list (car listrels) )))))
    ((eqv? typename 'partial-function) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "PARTIAL-FUNCTION" (list (car listrels) )))))
    ((eqv? typename 'total-function) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "TOTAL-FUNCTION" (list (car listrels) )))))
    ((eqv? typename 'abstract) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ABSTRACT" (list (car listrels) )))))
    ((eqv? typename 'abstract-all) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-constraint "ABSTRACT-ALL" (list (car listrels) )))))
    ((eqv? typename 'subset) (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-subset (xml-make-parent-identifier (car listrels)) 
                                                                                                              (xml-make-child-identifier (car (cdr listrels)))))))
    (else (printf " Error! Unsupported constraint type~n"))))

; Add a custom relation of type (car listrels) X (car (cdr listrels)) X ...
; Java expects an (unneeded!) arity value
(define (add-predicate vocab predname listrels)
  (xml-make-command "ADD" (list (xml-make-vocab-identifier vocab) (xml-make-predicate predname) (xml-make-relations-list listrels))))

; Sets the target property of a policy object
(define (set-target mypolicy conjlist)
  (xml-make-command "SET TARGET FOR POLICY" (list (xml-make-policy-identifier mypolicy) (xml-make-conjunct-chain conjlist))))


; Add a rule of the form rulename = (dtype reqvars) :- conjlist
(define (add-rule mypolicy rulename dtype reqvars conjlist)  
  (xml-make-command "ADD" (list (xml-make-policy-identifier mypolicy) (xml-make-rule rulename (xml-make-decision-type dtype) (xml-make-rule-list conjlist))))) 
