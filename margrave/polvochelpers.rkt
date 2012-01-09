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
 "helpers.rkt")
 
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




