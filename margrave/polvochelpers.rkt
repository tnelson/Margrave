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
   [xml (listof xexpr?)]
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
   [axioms-xml (listof xexpr?)]
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

(define-struct/contract m-policy
  ([id string?]
   [theory-path path?]
   [xml (listof xexpr?)]
   [theory m-theory?]   
   [vardecs (hash/c string? m-vardec?)]
   [rules (hash/c string? m-rule?)]
   [rcomb string?]
   [target m-formula?]
   [idbs (hash/c string? (listof string?))])
  #:transparent)

(define-struct/contract m-policy-set
  ([id string?]
   [theory-path path?]
   [xml (listof xexpr?)]
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

(define dereference-term-test-scenario-1 
  (m-scenario 5 
              '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
              (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
              (list (m-relation "$x" 'skolem '(("Atom#1"))) )
              (list (m-relation "c" 'constant '(("Atom#2")))
                    (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                ("Atom#4" "Atom#5")))
                    (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))                    
              (m-statistics #f #f #f empty (hash))
              empty))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 'x) "Atom#1"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 ''c) "Atom#2"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 '(f x)) "Atom#3"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 '(g 'c x)) "Atom#4"))
(check-true (equal? (dereference-term dereference-term-test-scenario-1 '(f (g 'c x))) "Atom#5"))
; (f c) not defined in this scenario, so should get an error:
(check-exn exn:fail? (lambda () (dereference-term dereference-term-test-scenario-1 '(f 'c))))
; no such variable
(check-exn exn:fail? (lambda () (dereference-term dereference-term-test-scenario-1 'z)))
; mis-use of constant as variable
(check-exn exn:fail? (lambda () (dereference-term dereference-term-test-scenario-1 'c)))
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
(check-true (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1") '(x)))
(check-false (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1") '('c)))
(check-true (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1" "Atom123") '(x)))
(check-exn exn:fail? (lambda () (tuple-involves-terms dereference-term-test-scenario-1 '("Atom#1") '((f 'c)))))



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
   (m-scenario-annotations a-scenario)))

;;test-cases
(check-true (equal? (filter-tuples (lambda (atuple)
				     (tuple-involves-terms dereference-term-test-scenario-1 
							   atuple
							   '()))
				   dereference-term-test-scenario-1)
		    (m-scenario 5 
				'("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
				(list (m-relation "ASort" 'sort '()))
				(list (m-relation "$x" 'skolem '()) )
				(list (m-relation "c" 'constant '())
				      (m-relation "f" 'relation '())
				      (m-relation "g" 'relation '()))                    
				(m-statistics #f #f #f empty (hash))
				empty)))
(check-true (equal? (filter-tuples (lambda (atuple)
				     (tuple-involves-terms dereference-term-test-scenario-1 
							   atuple
							   '(x)))
				   dereference-term-test-scenario-1)
		    (m-scenario 5
		     '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
		     (list (m-relation "ASort" 'sort '(("Atom#1"))))
		     (list (m-relation "$x" 'skolem '(("Atom#1"))))
		     (list
		      (m-relation "c" 'constant '())
		      (m-relation "f" 'relation '(("Atom#1" "Atom#3")))
		      (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))
		     (m-statistics #f #f #f empty (hash))
		     empty)))
(check-true (equal? (filter-tuples (lambda (atuple)
				     (tuple-involves-terms dereference-term-test-scenario-1 
							   atuple
							   '('c x (g 'c x))))
				   dereference-term-test-scenario-1)
  (m-scenario 5 
              '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
              (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#4"))))
              (list (m-relation "$x" 'skolem '(("Atom#1"))) )
              (list (m-relation "c" 'constant '(("Atom#2")))
                    (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                ("Atom#4" "Atom#5")))
                    (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))                    
              (m-statistics #f #f #f empty (hash))
              empty)))
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
   (m-scenario-annotations a-scenario)))


;;test-cases
(check-true (equal? (filter-relations (lambda (arelation)
				     true)
				   dereference-term-test-scenario-1)
  (m-scenario 5 
              '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
              (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
              (list (m-relation "$x" 'skolem '(("Atom#1"))))
              (list (m-relation "c" 'constant '(("Atom#2")))
                    (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                ("Atom#4" "Atom#5")))
                    (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))
              (m-statistics #f #f #f empty (hash))
              empty)))
(check-true (equal? (filter-relations (lambda (arelation)
				     false)
				   dereference-term-test-scenario-1)
  (m-scenario 5 
              '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
              (list )
              (list )
              (list )
              (m-statistics #f #f #f empty (hash))
              empty)))
(check-true (equal? (filter-relations (lambda (arelation)
					(or (equal? (m-relation-reltype arelation) 'constant)
					    (equal? (m-relation-reltype arelation) 'sort)))
				   dereference-term-test-scenario-1)
  (m-scenario 5 
              '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
              (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
              (list )
              (list (m-relation "c" 'constant '(("Atom#2"))))
              (m-statistics #f #f #f empty (hash))
              empty)))
(check-true (equal? (filter-relations (lambda (arelation)
				     (not (equal? (m-relation-name arelation) "$x")))
				   dereference-term-test-scenario-1)
  (m-scenario 5 
              '("Atom#1" "Atom#2" "Atom#3" "Atom#4" "Atom#5")
              (list (m-relation "ASort" 'sort '(("Atom#1") ("Atom#2") ("Atom#3") ("Atom#4") ("Atom#5"))))
              (list )
              (list (m-relation "c" 'constant '(("Atom#2")))
                    (m-relation "f" 'relation '(("Atom#1" "Atom#3")
                                                ("Atom#4" "Atom#5")))
                    (m-relation "g" 'relation '(("Atom#2" "Atom#1" "Atom#4"))))
              (m-statistics #f #f #f empty (hash))
              empty)))
