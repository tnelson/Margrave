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
 rackunit
 (only-in srfi/1 zip))

(provide
 (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cached-policies (make-hash))
(define cached-theories (make-hash))
(define cached-prior-queries (make-hash))

;****************************************************************
; Structs used to store information about policies, theories, etc.
; This data is also stored on the Java side, but we duplicate it
; here in order to produce helpful error messages and facilitate
; reflection. (E.g. "What sorts are available in theory X?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct/contract m-vocabulary  
  ([name string?]  
   [types (hash/c string? m-type?)] 
   [predicates (hash/c string? m-predicate?)] 
   [constants (hash/c string? m-constant?)] 
   [functions (hash/c string? m-function?)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct/contract m-prior-query
  ([id string?]   
   [vocab m-vocabulary?]   
   [idbs (hash/c string? (listof string?))])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct/contract m-theory
  ([name string?]   
   [path path?]
   [vocab m-vocabulary?]   
   [axioms (listof m-axiom?)])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct/contract m-vardec
  ([name symbol?]
   [type symbol?])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct/contract m-rule
  ([name string?]
   [decision string?]
   [headvars (listof symbol?)]
   [rbody m-formula?])
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; m-policy

;(define-struct/contract m-policy
;  ([id string?]
;   [theory m-theory?]   
;   [vardecs (hash/c symbol? m-vardec?)]
;   [rule-names (listof string?)]
;   [rules (hash/c string? m-rule?)]
;   [rcomb any/c]
;   [target m-formula?]
;   [idbs (hash/c string? (listof symbol?))])
;  #:transparent)

; Validate the m-policy fields. Check for consistency, etc.
(define/contract (prevalidate-m-policy id theory vardecs rule-names rules rcomb target idbs type-name)
  [-> string? m-theory? (hash/c symbol? m-vardec?) (listof string?) (hash/c string? m-rule?) any/c m-formula? (hash/c string? (listof symbol?)) any/c
      (values string? m-theory? (hash/c symbol? m-vardec?) (listof string?) (hash/c string? m-rule?) any/c m-formula? (hash/c string? (listof symbol?)))]        
  
  ; Environment for well-sortedness check: all vars declared in the policy
  (define env (foldl (lambda (next sofar) 
                       (hash-set sofar (m-vardec-name next) (m-vardec-type next)))
                     (hash)
                     (hash-values vardecs)))
  
  ; accepts either an immutable or mutable hash table
  ; and performs the appropriate hash-set (or set!) operation
  (define/contract (hash-set/safe a-hash a-key a-val)
    [hash? any/c any/c . -> . hash?]
    (cond [(immutable? a-hash)
           (hash-set a-hash a-key a-val)]
          [else 
           (hash-set! a-hash a-key a-val)
           a-hash]))
    
  
  ; Vocabulary for well-sortedness check
  ; The policy should be able to reference its own IDBs without qualification.
  ; E.g. not ([me permit] s a r) but just (permit s a r)
  ; To allow this, extend the base vocabulary with un-qualified predicates for this check ONLY:
  (define orig-vocab (m-theory-vocab theory))
  (define extended-vocab
    (m-vocabulary (m-vocabulary-name orig-vocab)
                  (m-vocabulary-types orig-vocab)
                  (foldl (lambda (idb-pair sofar)                             
                           (hash-set/safe sofar
                                          (car idb-pair)
                                          (m-predicate (car idb-pair) (map ->string (cdr idb-pair)))))
                         (m-vocabulary-predicates (m-theory-vocab theory))
                         (hash->list idbs))
                  (m-vocabulary-constants orig-vocab)
                  (m-vocabulary-functions orig-vocab)))
    
  
  ; The policy's variable declarations provide the environment under which we well-sort each rule. 
  ; So cannot validate rules via a guard on m-rule. Need to do it here:  
  
  (define (validate-rule r voc env)   
    (define desugared-body (desugar-formula (m-rule-rbody r)))
    (m-formula-is-well-sorted?/err voc desugared-body env))
  (for-each (lambda (arule) (validate-rule arule extended-vocab env)) (hash-values rules))
  
  ;(when ...
  ;  (error ...))
  
  ; All is well:
  (values id theory vardecs rule-names rules rcomb target idbs))

(struct m-policy (id theory vardecs rule-names rules rcomb target idbs)
  #:transparent
  #:guard prevalidate-m-policy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct/contract m-policy-set
  ([id string?]   
   [theory m-theory?]   
   ;[vardecs (hash/c string? m-vardec?)]
   [children (hash/c string? (or/c m-policy-set? m-policy?))]
   [pcomb any/c]
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
                                               (xml-make-target (m-formula->xexpr (m-rule-rbody arule)))))))

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

(define/contract (scenario-has-relation-or-sort a-scenario relname)
  [m-scenario? string? . -> . boolean?]
  (ormap (lambda (rel) (equal? (m-relation-name rel) relname))
         (append (m-scenario-sorts a-scenario)                 
                 (m-scenario-relations a-scenario))))


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

(define (comb->xexpr comb)
         (match comb
           [`(fa ,@(list args ...))            
            (xml-make-fa (map symbol->string args))]
           [`(over ,dec ,@(list odecs ...))
            (xml-make-over (->string dec)
                           (map ->string odecs))]
           [else empty]))

(define/contract (m-policy-set>xexprs pset)
  [m-policy-set? . -> . (listof xexpr?)]
  
  (define target-xexpr-list
    (list (xml-make-command "SET TARGET FOR POLICY" 
                                  (list (xml-make-policy-identifier (m-policy-set-id pset))
                                        (xml-make-target (m-formula->xexpr (m-policy-set-target pset))))))) 
  
  (define pcomb-xexpr (xml-make-command "SET PCOMBINE FOR POLICY" 
                                        (list (xml-make-policy-identifier (m-policy-set-id pset)) 
                                              (xml-make-comb-list (map comb->xexpr (m-policy-set-pcomb pset))))))
  
  (define create-xexpr (xml-make-command "CREATE POLICY SET" 
                                         (list (xml-make-policy-identifier (m-policy-set-id pset))
                                               (xml-make-vocab-identifier (m-theory-name (m-policy-set-theory pset))))))
  (define prepare-xexpr 
    (xml-make-command "PREPARE" (list (xml-make-policy-identifier (m-policy-set-id pset)))))
  
  (define (get-add-child-xml child-p-or-pset)
               (xml-make-command "ADD" (list `(PARENT ,(xml-make-policy-identifier (m-policy-set-id pset))
                                                      ,(xml-make-child-identifier (cond
                                                                                    [(m-policy-set? child-p-or-pset) 
                                                                                     (m-policy-set-id child-p-or-pset)]
                                                                                    [else
                                                                                     (m-policy-id child-p-or-pset)])))))) 
  
  ; Order of XML commands matters.
  (append (map m-policy->xexprs (m-policy-set-children pset))
          (list create-xexpr)
          (map (lambda (child) (get-add-child-xml child)) (m-policy-set-children pset))          
          target-xexpr-list
          (list pcomb-xexpr prepare-xexpr))
  )


; Does NOT auto-include the theory's XML
(define/contract (m-policy->xexprs policy)
  [m-policy? . -> . (listof xexpr?)]
      
  (define target-xexpr-list
    (list (xml-make-command "SET TARGET FOR POLICY" 
                                  (list (xml-make-policy-identifier (m-policy-id policy))
                                        (xml-make-target (m-formula->xexpr (m-policy-target policy)))))))   
  
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

;;;;;;;;;;;;;;;;;;;;;
; m-theory->key converts a theory to a key for use in hash tables. It uses both
; the theory's id and the theory's path. This way, different theories may be defined
; with the same name. E.g. the way we produce firewall vocabularies.      
; TODO: This is a hangup from reading theories from files. Should be able to stop doing this
; TODO: at some point.
(define/contract
  (m-theory->key athy)
  [m-theory? . -> . list?]
  (list (path->string (m-theory-path athy)) (m-theory-name athy)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Well-sortedness checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/contract (is-a-type-in-vocab?/err sname voc)
  [string? m-vocabulary? . -> . boolean? ]
  (unless (hash-has-key? (m-vocabulary-types voc) (->string sname))
    (margrave-error (format "Unknown type: ~a. Valid types were: ~v" (->symbol sname) (hash-keys (m-vocabulary-types voc))) sname))  
  #t)

(define/contract (is-a-constant-in-vocab? cname voc)
  [(or/c string? symbol?) m-vocabulary? . -> . boolean? ]
  (hash-has-key? (m-vocabulary-constants voc) (->string cname)))

; Inefficient, but works -- compute transitive closure of sort hierarchy.
(define/contract (m-type-child-names/rtrans voc sname)
  [m-vocabulary? symbol? . -> . (listof symbol?)]  
  
  (unless (hash-has-key? (m-vocabulary-types voc) (->string sname))
    (margrave-error (format "Unknown type: ~a. Valid types were: ~v" sname (hash-keys (m-vocabulary-types voc))) sname))
  
  (define the-type (hash-ref (m-vocabulary-types voc) (->string sname)))
  (define child-names (m-type-child-names the-type)) 
  ;(printf "child-names: ~v~n" child-names)
  (define result (append (list sname) ; *reflexive* transitive closure
                         ; do not include child-names explicitly; the map below will add them
                         (flatten 
                          (map (lambda (childname) (m-type-child-names/rtrans voc (->symbol childname))) child-names))))
  (remove-duplicates result))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What is the sort of this term? If the term is not well-sorted, throw a suitable error.
(define/contract (m-term->sort/err voc term env)
  [m-vocabulary? any/c hash? . -> . (or/c symbol? boolean?)]
  ;(printf "m-term->sort/err: ~v ~v ~v~n" voc term env)
  (match term
    [(or `(,(? valid-function? funcid) ,@(list (? m-term->xexpr terms) ...))
         (syntax-list-quasi ,(? valid-function? funcid) ,@(list (? m-term->xexpr terms) ...)))
     (define id-str (->string funcid))
     (unless (hash-has-key? (m-vocabulary-functions voc) id-str)
       (margrave-error (format "The function symbol ~v was not declared in the vocabulary context. Declared functions were: ~v"
                               id-str (hash-keys (m-vocabulary-functions voc))) term))   
     (define thefunc (hash-ref (m-vocabulary-functions voc) id-str))     
     (define pairs-to-check (zip terms (m-function-arity thefunc)))
     (for-each (lambda (p) 
                 (define-values (p-term p-sort) (values (first p) (second p)))
                 (define expected-sorts (m-type-child-names/rtrans voc (->symbol p-sort)))
                 (define term-sort (m-term->sort/err voc p-term env))
                 (unless (member? term-sort expected-sorts)
                   (margrave-error (format "The subterm ~v did not have the correct sort to be used in ~v. The subterm had sort ~v; expected one of these sorts: ~v" 
                                           p-term term term-sort expected-sorts) term)))
               pairs-to-check)                    
     (string->symbol (m-function-result thefunc))]
    [(? valid-constant? cid) 
     (define unquoted-id (extract-constant-id cid))
     (define unquoted-id-str (->string unquoted-id))     
     (unless (hash-has-key? (m-vocabulary-constants voc) unquoted-id-str)
       (margrave-error "The constant symbol was not declared in the query's vocabulary context" unquoted-id))     
     (string->symbol (m-constant-type (hash-ref (m-vocabulary-constants voc) unquoted-id-str)))]
    [(? valid-variable? vid) 
     (unless (hash-has-key? env term)
       ; Give special error if we think they might have forgotten the '
       (cond [(is-a-constant-in-vocab? vid voc)
              (margrave-error (format "The variable <~a> was not declared or quantified, but there was a constant of the same name. Should it be '~a?" 
                                      vid vid) vid)]
             [else
              (margrave-error (format "The variable <~a> was not declared or quantified. The formula expected one of the following: ~v" vid (hash-keys env)) vid)]))
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
     (set-union (gather-policy-references arg1) (gather-policy-references arg2))]  
    [(m-op-case iff arg1 arg2)
     (set-union (gather-policy-references arg1) (gather-policy-references arg2))]   
    [(m-op-case not arg)
     (gather-policy-references arg)]       
    [(m-op-case forall vname sname subfmla)
     (gather-policy-references subfmla)]     
    [(m-op-case exists vname sname subfmla)
     (gather-policy-references subfmla)]               
    [(m-op-case isa vname sname subfmla)
     (gather-policy-references subfmla)]     
    
    ; IDB: gather policy ids used!
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,@(list terms ...))
     (list->set pids)] ; will be empty set if saved-query IDB  
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (set)]         
    [else (margrave-error "Invalid formula given to gather-policy-references" fmla)]))

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
     (set-union (gather-query-references arg1) (gather-query-references arg2))]  
    [(m-op-case iff arg1 arg2)
     (set-union (gather-query-references arg1) (gather-query-references arg2))]   
    [(m-op-case not arg)
     (gather-query-references arg)]       
    [(m-op-case forall vname sname subfmla)
     (gather-query-references subfmla)]     
    [(m-op-case exists vname sname subfmla)
     (gather-query-references subfmla)]               
    [(m-op-case isa vname sname subfmla)
     (gather-query-references subfmla)]     
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,@(list terms ...))
     (if (empty? pids)
         (set idbname)
         (set))]     
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (set)]         
    [else (margrave-error "Invalid formula given to gather-query-references" fmla)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Get a cached policy by ID. If no such policy exists, throw a suitable error.
(define (get-cached-policy pid)  
  (if (hash-has-key? cached-policies (->string pid))    
      (hash-ref cached-policies (->string pid))
      #f))
; same for cached prior query
(define (get-prior-query qid)  
  (if (hash-has-key? cached-prior-queries (->string qid))    
      (hash-ref cached-prior-queries (->string qid))
      #f))

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
    [any/c (or/c string? symbol?) . -> . boolean?]               
    ;(printf "In internal correct. Can ~v be a ~v? ~n" tname sname)
    (define sname-sym (->symbol sname))                            
    (define valid-sort-names (m-type-child-names/rtrans voc sname-sym))
    ;(printf "done with valid-sort-names: ~v~n" valid-sort-names)
    (define term-sort (m-term->sort/err voc tname env))
    ;(printf "internal-correct checking: ~v ~v ~v ~v ~v~n" tname sname-sym valid-sort-names term-sort (member? term-sort valid-sort-names))
    (unless (member? term-sort valid-sort-names)
      (margrave-error (format "The formula ~v was not well-sorted. The term ~a was of type ~a, but expected to be of type ~v" fmla tname term-sort sname-sym) tname))
    #t)
  
  ; Handle case: (isa x A true)
  (define/contract (internal-correct/isa-sugar vname sname)
    [any/c (or/c symbol? string?) . -> . boolean?]  
    ; sugary isa formulas are always well-sorted
    ; unless the sort name is not defined in the vocab.
    ; Don't use internal-correct, which will force x to be subsort of A in (A x). 
    ; Instead, just check that A is valid.
    (is-a-type-in-vocab?/err sname voc))
  
  ; Handle case: (edbname x y z)
  (define (internal-correct/edb list-of-vars edbname)
    (unless (hash-has-key? (m-vocabulary-predicates voc) edbname)
      (margrave-error (format "The predicate ~v was not defined in the vocabulary context" edbname) fmla))
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
       (define this-prior-query (get-prior-query idbname))
       (unless this-prior-query
         (margrave-error (format "Formula ~v referenced the prior query identifier ~v but no such query was found." fmla idbname) fmla))
       (unless (hash-has-key? (m-prior-query-idbs this-prior-query) idbname)
         (margrave-error (format "Saved query ~v did not contain the IDB ~v. It contained: ~v" idbname idbname (m-prior-query-idbs this-prior-query)) fmla))
       (m-prior-query-idbs this-prior-query)]
      [else
       ; policy IDB
       (define this-policy (get-cached-policy (first pol-id-list)))
       (unless this-policy
         (margrave-error (format "Formula ~v referenced the policy identifier ~v but no such policy was found." fmla (first pol-id-list)) fmla))
       (unless (hash-has-key? (m-policy-idbs this-policy) idbname)
         (margrave-error (format "Policy ~v did not contain the IDB ~v. It contained: ~v" pol-id-list idbname (m-policy-idbs this-policy)) fmla))
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
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,@(list terms ...))
     (internal-correct/idb terms pids idbname)] 
    
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (cond
       [(and (valid-sort? dbname) 
             (empty? terms))
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
  
  ; can't use hash-union since we want to allow (and check) overlaps
  (define new-types (union-types-hashes (m-vocabulary-types v1) (m-vocabulary-types v2)))
  (define new-predicates (hash-union/overlap (m-vocabulary-predicates v1) (m-vocabulary-predicates v2) "Predicates did not match"))
  (define new-constants (hash-union/overlap (m-vocabulary-constants v1) (m-vocabulary-constants v2) "Constants did not match"))
  (define new-functions (hash-union/overlap (m-vocabulary-functions v1) (m-vocabulary-functions v2) "Functions did not match"))
  
  (m-vocabulary new-name new-types new-predicates new-constants new-functions))


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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Remove syntactic sugar from the formula. 
; (1) (and (isa x S) a b c) --> (isa x S (and a b c))
; (2) (and (S x) a b c) --> (isa x S (and a b c))
; This change allows policies to be well-sorted without
; an annoyingly deep scope of isas, so should be called
; by the well-sortedness check. 

; HOWEVER! The java engine is only smart enough to notice
; (isa x S true) is special --- the 'true subfmla triggers
; special behavior. The term counter knows how to handle
; S(x) as a *local* coercion. It will blow up the number of 
; elements if we actually PASS 
;(and (S x) a b c)
;as 
;(isa x S (and a b c))

; Also:
; (3) (S x) -> (isa x S)
(define/contract (desugar-formula fmla)
  [m-formula? . -> . m-formula?]
  
  (define (desugar-and conjuncts)    
    ; Partition the conjuncts into (isa x S) and others.
    ;(printf "Desugaring and: ~v~n" conjuncts)
    (define-values (isas others)
      (partition (lambda (e) (match e
                               [(m-op-case isa vname sname 'true) #t]
                               [else #f]))
                 conjuncts))
    ;(printf "  isas: ~v~n  others: ~v~n" isas others)
    ; Wrap the conjunction of the others with the isas. Special case: empty others.
    (define result (foldl (lambda (next sofar) 
                            (match next
                              [(m-op-case isa vname sname 'true) 
                               `(isa ,vname ,sname ,sofar)]))
                          (if (empty? others)
                              'true
                              `(and ,@others))
                          isas))
    ;(printf "    desugaring ~v:~n    isas: ~v~n    non:~v~n" `(and ,@conjuncts) isas others)
    ;(printf "    result: ~v~n~n" result)
    result)
  
  (match fmla
    [(maybe-identifier true)
     fmla]        
    [(maybe-identifier false)
     fmla]            
    [(m-op-case = t1 t2)     
     fmla]
    
    ; Desugar subformulas first. Now we should have (isa x S)
    ; everywhere instead of (S x)
    [(m-op-case and args ...)
     (desugar-and (map desugar-formula args))]    
    
    [(m-op-case or args ...)
     `(or ,@(map desugar-formula args))]
    [(m-op-case implies arg1 arg2)
     `(implies ,(desugar-formula arg1) ,(desugar-formula arg2))]   
    [(m-op-case iff arg1 arg2)
     `(iff ,(desugar-formula arg1) ,(desugar-formula arg2))]   
    [(m-op-case not arg)
     `(not ,(desugar-formula arg))]   
    
    [(m-op-case forall vname sname subfmla)
     `(forall ,vname ,sname ,(desugar-formula subfmla))]     
    [(m-op-case exists vname sname subfmla)
     `(exists ,vname ,sname ,(desugar-formula subfmla))]     
    [(m-op-case isa vname sname subfmla)
     `(isa ,vname ,sname ,(desugar-formula subfmla))]              
    [(maybe-syntax-list-quasi ,(maybe-syntax-list-quasi ,@(list pids ... idbname)) ,@(list terms ...))
     fmla] 
    
    [(maybe-syntax-list-quasi ,dbname ,term0 ,@(list terms ...))
     (cond
       [(and (valid-sort? dbname) (empty? terms))
        `(isa ,term0 ,dbname true)]   
       [else 
        fmla])] 
    
    [else (margrave-error "The formula was not well-formed" fmla) ]))

