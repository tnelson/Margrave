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
   [idbs (hash/c string? m-predicate?)])
  #:transparent)

;(define-struct/contract m-policyset
;  ([id string?]
;   [xml (listof xexpr?)]
;   ;[theory m-theory?]
;   [vocabulary m-vocabulary?]
;   [vardecs (listof m-vardec?)]
;   [rules (listof m-rule?)]
;   [rcomb string?])
;  #:transparent)

(define/contract 
  (m-rule->cmd policyid arule)  
  [string? m-rule? . -> . xexpr?]
  (xml-make-command "ADD" (list (xml-make-policy-identifier policyid) 
                                (xml-make-rule (m-rule-name arule)
                                               (xml-make-decision-type (m-rule-decision arule)
                                                                       (m-rule-headvars arule))
                                               (m-rule-rbody arule)))))

(define/contract 
  (m-vardec->cmd policyid adec)  
  [string? m-vardec? . -> . xexpr?]
  (xml-make-command "ADD" (list (xml-make-policy-identifier policyid) 
                                (xml-make-variable-declaration (m-vardec-name adec)
                                                               (m-vardec-type adec)))))
                                                                                    

; Convert from actual structs to syntax constructing the same struct.
(define/contract (repackage-transparent-struct the-struct)
  [struct? . -> . syntax?]
  (define struct-list (vector->list (struct->vector the-struct)))
  (define struct-name (string->symbol (substring (symbol->string (first struct-list)) 7)))  
  (define (safe-param x)
    (if (list? x)
        #`'#,x
        x))  
  (quasisyntax (#,struct-name #,@(map safe-param (rest struct-list)))))  

