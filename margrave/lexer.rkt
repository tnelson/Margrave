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

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr)

(provide lex 
         terminals 
         empty-terminals)

; *************************************************
; define-empty-tokens defines the tokens that take no value
; define-tokens defines the tokens that can contain a value
(define-empty-tokens empty-terminals
  (             EXPLORE AND OR NOT COLON IMPLIES IFF LPAREN RPAREN EQUALS SHOW
                        ALL ONE IS POSSIBLEQMARK PUBLISH COMMA UNDER TUPLING DEBUG
                        CEILING RENAME INFO COLLAPSE COMPARE INCLUDE 
                        FOR CASES ADD SUBSORT SORT CONSTRAINT DISJOINT
                        NONEMPTY SINGLETON ATMOSTONE PARTIAL FUNCTION TOTAL ABSTRACT
                        SUBSET SET TARGET PREDICATE RULE TO CREATE VOCABULARY DECISION
                        REQUESTVAR OTHERVAR POLICY LEAF RCOMBINE PCOMBINE PREPARE LOAD
                        XACML SQS GET COUNT SIZE RULES HIGHER PRIORITY THAN QUALIFIED
                        NEXT GUARANTEEDQMARK IN	AT CHILD REQUEST VECTOR QUIT DELETE SEMICOLON
                        EOF WITH TRUE REALIZED UNREALIZED GTHAN LTHAN IOS EMPTYID DEFVEC))
(define-tokens terminals (<identifier> <unsigned-integer> <comment>))



(define-lex-abbrevs
  [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [lex:nswhitespace (:or #\newline #\return #\tab #\vtab)]
  
  ; The last line of a file may be a comment. Greedy matching will match the entire line up to the end.
  [lex:comment (:: #\/ #\/ (:* (char-complement (:or #\newline #\return))))])



; Lexer syntax-transformer that creates SRE syntax for case-insensitive string tokens
; Example (lex-ci "explore") becomes an SRE for the regexp [Ee][Xx] ...
(define-lex-trans 
  lex-ci 
  (lambda (s) 
    (define the-string-syntax (car (cdr (syntax->list s))))
    (define the-string (syntax->datum the-string-syntax))
    
    (define (make-both-case-or the-char)
      `(union ,(char-upcase the-char) ,(char-downcase the-char)))
    (define (make-case-insensitive-regexp-symbol str)
      `(:: ,@ (map make-both-case-or (string->list str))))
    
    ; reconstruct using old syntax object's context, srcloc, prop, cert.
    (define result (datum->syntax the-string-syntax
                                  (make-case-insensitive-regexp-symbol the-string)
                                  the-string-syntax
                                  the-string-syntax 
                                  the-string-syntax))
   ; (printf "~a ~a~n" the-string-syntax result)
    result))

; *************************************************
; Lexer func only takes the input port, not the source
; (as parser does). We want to syntax-highlight lexer-level errors.
;(define current-lexer-source (make-parameter #f))

; *************************************************
; Produce a lexer function
; Assume the caller has downcased the input
(define lex  
  (lexer-src-pos
   ; Skip whitespace and comments
   [(:+ lex:whitespace) (return-without-pos (lex input-port))]
   
   ; Comment. Must appear before <identifier> rules or else something like
   ; //abc will be mis-tokenized. (Remember, priority is length and then order in the rule list.)
   [lex:comment (return-without-pos (lex input-port))]
   
   [(eof) (token-EOF)]
   
   [":" (token-COLON)] 
   [";" (token-SEMICOLON)] 
   ["(" (token-LPAREN)] 
   [")" (token-RPAREN)] 
   ["=" (token-EQUALS)] 
   ["," (token-COMMA)]
   ["<" (token-LTHAN)] 
   [">" (token-GTHAN)]
   ["\"\"" (token-EMPTYID)]
   
   [(lex-ci "defvec") (token-DEFVEC)]
   [(lex-ci "explore") (token-EXPLORE)]
   [(lex-ci "load") (token-LOAD)]
   [(lex-ci "policy") (token-POLICY)] 
   [(lex-ci "and") (token-AND)] 
   [(lex-ci "or") (token-OR)] 
   [(lex-ci "not") (token-NOT)]  
   [(lex-ci "implies") (token-IMPLIES)] 
   [(lex-ci "iff") (token-IFF)] 
   [(lex-ci "show") (token-SHOW)] 
   [(lex-ci "all") (token-ALL)] 
   [(lex-ci "one") (token-ONE)] 
   [(lex-ci "is") (token-IS)]   
   [(lex-ci "possible?") (token-POSSIBLEQMARK)] 
   [(lex-ci "publish") (token-PUBLISH)] 
   [(lex-ci "under") (token-UNDER)] 
   [(lex-ci "tupling") (token-TUPLING)] 
   [(lex-ci "debug") (token-DEBUG)] 
   [(lex-ci "ceiling") (token-CEILING)] 
   [(lex-ci "rename") (token-RENAME)] 
   [(lex-ci "info") (token-INFO)] 
   [(lex-ci "collapse") (token-COLLAPSE)] 
   [(lex-ci "compare") (token-COMPARE)] 
   [(lex-ci "include") (token-INCLUDE)] 
   [(lex-ci "realized") (token-REALIZED)] 
   [(lex-ci "unrealized") (token-UNREALIZED)] 
   [(lex-ci "for") (token-FOR)] 
   [(lex-ci "cases") (token-CASES)] 
   [(lex-ci "add") (token-ADD)] 
   [(lex-ci "subsort") (token-SUBSORT)] 
   [(lex-ci "sort") (token-SORT)] 
   [(lex-ci "constraint") (token-CONSTRAINT)] 
   [(lex-ci "disjoint") (token-DISJOINT) ] 
   [(lex-ci "nonempty") (token-NONEMPTY)] 
   [(lex-ci "singleton") (token-SINGLETON)] 
   [(lex-ci "atmostone") (token-ATMOSTONE)] 
   [(lex-ci "partial") (token-PARTIAL)] 
   [(lex-ci "function") (token-FUNCTION)] 
   [(lex-ci "total") (token-TOTAL)] 
   [(lex-ci "abstract") (token-ABSTRACT)] 
   [(lex-ci "subset") (token-SUBSET)] 
   [(lex-ci "set") (token-SET)] 
   [(lex-ci "target") (token-TARGET)] 
   [(lex-ci "predicate") (token-PREDICATE)] 
   [(lex-ci "rule") (token-RULE)] 
   [(lex-ci "to") (token-TO)] 
   [(lex-ci "create") (token-CREATE)] 
   [(lex-ci "vocabulary") (token-VOCABULARY)] 
   [(lex-ci "decision") (token-DECISION)] 
   [(lex-ci "requestvar") (token-REQUESTVAR)] 
   [(lex-ci "othervar") (token-OTHERVAR)] 
   [(lex-ci "leaf") (token-LEAF)] 
   [(lex-ci "rcombine") (token-RCOMBINE)] 
   [(lex-ci "pcombine") (token-PCOMBINE)] 
   [(lex-ci "prepare") (token-PREPARE)] 
   [(lex-ci "load") (token-LOAD)] 
   [(lex-ci "xacml") (token-XACML)] 
   [(lex-ci "sqs") (token-SQS)] 
   [(lex-ci "get") (token-GET)] 
   [(lex-ci "count") (token-COUNT)] 
   [(lex-ci "size") (token-SIZE)] 
   [(lex-ci "rules") (token-RULES)] 
   [(lex-ci "higher") (token-HIGHER)] 
   [(lex-ci "priority") (token-PRIORITY)] 
   [(lex-ci "than") (token-THAN)] 
   [(lex-ci "qualified") (token-QUALIFIED)] 
   [(lex-ci "next") (token-NEXT)] 
   [(lex-ci "guaranteed?") (token-GUARANTEEDQMARK)] 
   [(lex-ci "in") (token-IN)]	
   [(lex-ci "at") (token-AT)] 
   [(lex-ci "child") (token-CHILD)] 
   [(lex-ci "request") (token-REQUEST)] 
   [(lex-ci "vector") (token-VECTOR)] 
   [(lex-ci "quit") (token-QUIT)] 
   [(lex-ci "delete") (token-DELETE)]
   [(lex-ci "with") (token-WITH)]
   [(lex-ci "true") (token-TRUE)]
   [(lex-ci "ios") (token-IOS)]
   [(lex-ci "sqs") (token-SQS)]
   [(lex-ci "xacml") (token-XACML)]     
   
   ; Natural nums
   [(:: lex:digit (:* lex:digit)) 
    (token-<unsigned-integer> (string->symbol lexeme))]
   
   ; Un-quoted Identifiers -- everything but whitespace and: ( ) < > \" , = :
   ; Use ----> char-complement <----, not complement.
   [(:: (:+ (char-complement (:or lex:whitespace "\"" "(" ")" "<" ">" "," "=" ":" ";"))))
    (token-<identifier> (string->symbol lexeme))]
   
   ; Quoted Identifiers -- anything but quote or non-space-whitespace wrapped in quotes
   ; strip the quotes when returning the identifier value     
   [(:: "\"" (:+ (char-complement (:or lex:nswhitespace "\""))) "\"") 
    (token-<identifier> (string->symbol (substring lexeme 1 (- (string-length lexeme) 1))))]  
   
   ; Lexer error (parameter allows for syntax highlighting on the first character)
   [any-char
    (raise-read-error (format "Could not assign a lexical token starting at character: ~a.~n" lexeme)
                      (object-name input-port)
                      (position-line start-pos)
                      (position-col start-pos)
                      (position-offset start-pos)      
                      (- (position-offset end-pos) (position-offset start-pos)))]
   ))

; *************************************************
