;    Copyright (c) 2009-2011 Brown University and Worcester Polytechnic Institute.
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

(require
 xml
 "margrave-xml.rkt"
 "helpers.rkt"
 rackunit)

(provide
 (all-defined-out))


;****************************************************************
; Structs used to store information about policies, theories, etc.
; This data is also stored on the Java side, but we duplicate it
; here in order to produce helpful error messages and facilitate
; reflection. (E.g. "What sorts are available in theory X?")

(define-struct/contract m-vocabulary  
  ([name string?]  
   [types (hash/c string? m-type?)] 
   [predicates (hash/c string? m-predicate?)] 
   [constants (hash/c string? m-constant?)] 
   [functions (hash/c string? m-function?)])
  #:transparent)

(define-struct/contract m-prior-query
  ([id string?]   
   [vocab m-vocabulary?]   
   [idbs (hash/c string? (listof string?))])
  #:transparent)

(define-struct/contract m-theory
  ([name string?]   
   [vocab m-vocabulary?]   
   [axioms (listof m-axiom?)])
  #:transparent)

(define-struct/contract m-vardec
  ([name string?]
   [type string?])
  #:transparent)

(define-struct/contract m-rule
  ([name string?]
   [decision string?]
   [headvars (listof string?)]
   [rbody (listof m-formula?)])
  #:transparent)

; Validate the m-policy fields. Check for consistency, etc.
(define/contract (prevalidate-m-policy id theory-path theory vardecs rule-names rules rcomb target idbs type-name)
  [-> string? path? m-theory? (hash/c string? m-vardec?) (listof string?) (hash/c string? m-rule?) any/c m-formula? (hash/c string? (listof string?)) any/c
      (values string? path? m-theory? (hash/c string? m-vardec?) (listof string?) (hash/c string? m-rule?) any/c m-formula? (hash/c string? (listof string?)))]    
  ;(when ...
  ;  (error ...))
  
  ; All is well:
  (values id theory-path theory vardecs rule-names rules rcomb target idbs))

(struct m-policy (id theory-path theory vardecs rule-names rules rcomb target idbs)
  #:transparent
  #:guard prevalidate-m-policy)

;(define-struct/contract m-policy
;  ([id string?]
;   [theory-path path?]   
;   [theory m-theory?]   
;   [vardecs (hash/c string? m-vardec?)]
;   [rule-names (listof string?)]
;   [rules (hash/c string? m-rule?)]
;   [rcomb any/c]
;   [target m-formula?]
;   [idbs (hash/c string? (listof string?))])
;  #:transparent)

(define-struct/contract m-policy-set
  ([id string?]
   [theory-path path?]   
   [theory m-theory?]   
   [vardecs (hash/c string? m-vardec?)]
   [children (hash/c string? m-policy?)]
   [pcomb string?]
   [target m-formula?]
   [idbs (hash/c string? (listof string?))])
  #:transparent)

(define/contract 
  (m-rule->cmd policyid arule)  
  [string? m-rule? . -> . xexpr?]
  (xml-make-command "ADD" (list (xml-make-policy-identifier policyid) 
                                (xml-make-rule (m-rule-name arule)
                                               (xml-make-decision-type (m-rule-decision arule)
                                                                       (m-rule-headvars arule))
                                               (xml-make-target (xml-make-and* (map m-formula->xexpr (m-rule-rbody arule))))))))

(define/contract 
  (m-vardec->cmd policyid adec)  
  [string? m-vardec? . -> . xexpr?]
  (xml-make-command "ADD" (list (xml-make-policy-identifier policyid) 
                                (xml-make-variable-declaration (m-vardec-name adec)
                                                               (m-vardec-type adec)))))


; Convert from actual structs to a list describing it. This is needed because
; a struct defined at phase 0 is DIFFERENT from a struct defined at phase 1.
; e.g. at phase 0, (m-type? an-m-type-from-phase-1) will be #f
(define/contract (disassemble-transparent-struct the-struct)
  [struct? . -> . list?]
  (define struct-list (vector->list (struct->vector the-struct)))
  (define struct-name (string->symbol (substring (symbol->string (first struct-list)) 7)))    
  `(,struct-name ,@(rest struct-list)))

(define/contract (reassemble-margrave-struct the-list)
  [list? . -> . struct?]  
  (match the-list
    [`(m-type ,@(list args ...)) (apply m-type args)]
    [`(m-predicate ,@(list args ...)) (apply m-predicate args)]
    [`(m-constant ,@(list args ...)) (apply m-constant args)]
    [`(m-function ,@(list args ...)) (apply m-function args)]
    [`(m-rule ,@(list args ...)) (apply m-rule args)]
    [`(m-vardec ,@(list args ...)) (apply m-vardec args)]
    [`(m-vocabulary ,@(list args ...)) (apply m-vocabulary args)]
    [`(m-theory ,@(list args ...)) (apply m-theory args)]
    [`(m-policy ,@(list args ...)) (apply m-policy args)]
    [else (margrave-error "Internal error: bad argument to reassemble-margrave-struct" the-list)]))

; given a hash of id->disassembled entries, return the same hash with the values reassembled
(define/contract (assemble-struct-hash dhash)
  [hash? . -> . hash?]    
  (define reassembled-pairs (map (lambda (key) 
                                   (list key (reassemble-margrave-struct (hash-ref dhash key)))) 
                                 (hash-keys dhash))) 
  (apply hash (apply append reassembled-pairs)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dereference-term : m-scenario m-term -> string
; Consume a scenario and a term and produce the atom that
; the term denotes in that scenario. (Atoms are strings in
; Margrave.)
(define/contract (dereference-term in-scenario the-term)
  [m-scenario? m-term? . -> . string?]
  (match the-term
    ; valid-function? prevents ''c causing problems (since will appear to be '(quote c))
    [(list (? valid-function? funcid) subterms ...)   
     (define func-name (->string funcid))
     (define subterm-atoms (map (lambda (aterm) (dereference-term in-scenario aterm))
                                subterms))     
     (define func-relation (get-unique-relation (m-scenario-relations in-scenario) func-name))
     ; Don't just have a single tuple here, like in a constant or variable. Need to find the correct tuple:
     (define correct-tuples (filter (lambda (atup) (equal? subterm-atoms (take atup (- (length atup) 1))))
                                    (m-relation-tuples func-relation)))
     (unless (equal? (length correct-tuples) 1)
       (error 'dereference-term (format "The scenario did not contain a mapping for the term: ~v" the-term)))
     (last (first correct-tuples))]
    
    [(? valid-constant? cid) 
     (define const-name (->string (extract-constant-id cid)))
     (define constant-relation (get-unique-relation (m-scenario-relations in-scenario) const-name))
     (first (first (m-relation-tuples constant-relation)))]
    
    [(? valid-variable? vid)
     (define var-name (string-append "$" (->string vid)))
     (define skolem-relation (get-unique-relation (m-scenario-skolems in-scenario) var-name))
     (first (first (m-relation-tuples skolem-relation)))]
    [else (error 'dereference-term (format "The term ~v was invalid in the scenario given." the-term))]))

(define/contract (get-unique-relation relations-list relation-name)
  [(listof m-relation?) string? . -> . m-relation?]
  (define found-list (filter (lambda (arel) (equal? (m-relation-name arel) relation-name)) 
                             relations-list))
  (unless (equal? 1 (length found-list))
    (error 'get-skolem-relation (format "The scenario did not contain a unique relation named ~v" relation-name)))
  (first found-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tuple-involves-terms : m-scenario (listof string) (listof m-term) -> boolean
; In the scenario given, does the tuple of atoms given involve any of the atoms denoted 
; by the terms given?
; An atom is a string, so a tuple is a list of strings.
(define/contract (tuple-involves-terms in-scenario the-tuple the-terms)
  [m-scenario? (listof string?) (listof m-term?) . -> . boolean?]
  (define the-atoms (map (lambda (aterm) (dereference-term in-scenario aterm))
                         the-terms))  
  (ormap (lambda (anatom) (member? anatom the-atoms))
         the-tuple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-tuples: procedure scenario -> scenario
; Retains only the tuples that filter-pred returns non-#f on.
(define/contract (filter-tuples filter-pred a-scenario)
  [procedure? m-scenario? . -> . m-scenario?]
  (m-scenario
   (m-scenario-size a-scenario)
   (m-scenario-atoms a-scenario)
   (map (lambda (item)
          (m-relation
           (m-relation-name item)
           (m-relation-reltype item)
           (filter filter-pred (m-relation-tuples item))))
        (m-scenario-sorts a-scenario))
   (map (lambda (item)
          (m-relation
           (m-relation-name item)
           (m-relation-reltype item)
           (filter filter-pred (m-relation-tuples item))))
        (m-scenario-skolems a-scenario))
   (map (lambda (item)
          (m-relation
           (m-relation-name item)
           (m-relation-reltype item)
           (filter filter-pred (m-relation-tuples item))))
        (m-scenario-relations a-scenario))
   (m-scenario-statistics a-scenario)
   (m-scenario-annotations a-scenario)
   (m-scenario-query-id a-scenario)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filter-relations: procedure scenario -> scenario
; Retains only the relations that filter-pred returns non-#f on.
(define/contract (filter-relations filter-pred a-scenario)
  [(m-relation? . -> . boolean?) m-scenario? . -> . m-scenario?]
  
  (m-scenario
   (m-scenario-size a-scenario)
   (m-scenario-atoms a-scenario)
   (filter filter-pred (m-scenario-sorts a-scenario))
   (filter filter-pred (m-scenario-skolems a-scenario))
   (filter filter-pred (m-scenario-relations a-scenario))
   (m-scenario-statistics a-scenario)
   (m-scenario-annotations a-scenario)
   (m-scenario-query-id a-scenario)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Produce XML for declaring these vocabularies, theories, 
; policies... to the Java engine
(define/contract (m-vocabulary->xexprs vocab)
  [m-vocabulary? . -> . (listof xexpr?)]
  
  (define types-cmds (map (lambda (x) (m-type->cmd (m-vocabulary-name vocab) x))
                          (hash-values (m-vocabulary-types vocab))))
  
  (define predicates-cmds (map (lambda (x) (m-predicate->cmd (m-vocabulary-name vocab) x)) 
                               (hash-values (m-vocabulary-predicates vocab))))       
  
  (define constants-cmds (map (lambda (x) (m-constant->cmd (m-vocabulary-name vocab) x)) 
                              (hash-values (m-vocabulary-constants vocab))))
  
  (define functions-cmds (map (lambda (x) (m-function->cmd (m-vocabulary-name vocab) x))
                              (hash-values (m-vocabulary-functions vocab))))
  
  (append types-cmds predicates-cmds constants-cmds functions-cmds))

; Sometimes we just want to use a vocabulary w/o theory wrapper. Support that.
(define/contract (m-theory->xexprs thy)
  [(or/c m-vocabulary? m-theory?) . -> . (listof xexpr?)]
  (cond [(m-vocabulary? thy) (m-vocabulary->xexprs thy)]
        [else 
         (define axioms-xexprs (map (lambda (axiom) (make-axiom-command (m-theory-name thy) axiom)) 
                                    (m-theory-axioms thy)))
         (append (m-vocabulary->xexprs (m-theory-vocab thy)) 
                 axioms-xexprs)]))

; Does NOT auto-include the theory's XML
(define/contract (m-policy->xexprs policy)
  [m-policy? . -> . (listof xexpr?)]
    
  ; No reason to include target if it's 'true
  (define target-xexpr-list
    (cond [(equal? (m-policy-target policy) 'true)
           empty]
          [else (list (xml-make-command "SET TARGET FOR POLICY" 
                                  (list (xml-make-policy-identifier (m-policy-id policy))
                                        (m-formula->xexpr (m-policy-target policy)))))]))
  
  (define (comb->xexpr comb)
         (match comb
           [`(fa ,@(list args ...))            
            (xml-make-fa (map symbol->string args))]
           [`(over ,dec ,@(list odecs ...))
            (xml-make-over (->string dec)
                           (map ->string odecs))]
           [else empty]))
  
  (define rcomb-xexpr (xml-make-command "SET RCOMBINE FOR POLICY" 
                                           (list (xml-make-policy-identifier (m-policy-id policy)) 
                                                 (xml-make-comb-list (map comb->xexpr (m-policy-rcomb policy))))))
  
       
  (define create-xexpr (xml-make-command "CREATE POLICY LEAF" 
                                         (list (xml-make-policy-identifier (m-policy-id policy))
                                               (xml-make-vocab-identifier (m-theory-name (m-policy-theory policy))))))
       
  (define prepare-xexpr 
    (xml-make-command "PREPARE" (list (xml-make-policy-identifier (m-policy-id policy)))))
    
  ; Order of XML commands matters.
  (append (list create-xexpr)
          (map (lambda (ele) (m-vardec->cmd (m-policy-id policy) ele)) (hash-values (m-policy-vardecs policy)))
          ; make sure to PRESERVE ORDER on the rules. Order shouldn't matter for variable declarations.
          ; Don't map over by (hash-keys (m-policy-rules policy)). Use the rule-names list instead:
          (map (lambda (aname) (m-rule->cmd (m-policy-id policy) (hash-ref (m-policy-rules policy) aname))) (m-policy-rule-names policy))
          target-xexpr-list
          (list rcomb-xexpr prepare-xexpr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







