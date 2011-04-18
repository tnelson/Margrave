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
  (    
   ; delimiters, constant signifier
   DOT QUOTESYMBOL COLON                      
       
       ; logical connectives, parens, equality, etc. and fmla context control
       LET BE LSQBRACK RSQBRACK
       AND OR NOT IMPLIES IFF LPAREN RPAREN EQUALS COMMA TRUE
       
       ; vector sugar
       GTHAN LTHAN DEFVEC
       
       ;COMPARE
       
       ; command tokens
       SHOW ALL REALIZED UNREALIZED FOR CASES ISPOSSQ COUNT AT
       
       ; optional parameters to some commands
       UNDER INCLUDE   
       
       ; directives and options
       INFO SET GET WITH IN
       LOAD POLICY XACML SQS IOS 
       TUPLING DEBUG CEILING 
       QUIT 
       
       ; end of command
       SEMICOLON EOF ))

; Margrave identifiers are case-sensitive. We distinguish between
; e.g. sorts and predicates by capitalization (Sorts are capitalized, 
; predicates are not).
(define-tokens terminals 
  (
   ; all lowercase id
   <lowercase-id>
   ; capitalized id
   <capitalized-id>
   ; quoted-id
   <quoted-id>
   ; natural
   <natural>
   ; comment
   <comment>))

(define-lex-abbrevs
  [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:uppercase-letter (:/ #\A #\Z)]
  [lex:lowercase-letter (:/ #\a #\z)]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [lex:nswhitespace (:or #\newline #\return #\tab #\vtab)]
  
  ; The last line of a file may be a comment. Greedy matching will match the entire line up to the end.
  [lex:comment (:: #\/ #\/ (:* (char-complement (:or #\newline #\return))))])


; *************************************************


; Lexer syntax-transformer that creates SRE syntax for case-insensitive string tokens
; Example (lex-ci "explore") becomes an SRE for the regexp [Ee][Xx] ...
; Used to allow keywords to be case insensitive without making the entire syntax case-insensitive.
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
; Produce a lexer function
; Assume the caller has downcased the input
(define lex  
  (lexer-src-pos
   ; Skip whitespace and comments
   [(:+ lex:whitespace) (return-without-pos (lex input-port))]
   
   ; Comment. Must appear before <identifier> rules or else something like
   ; //abc will be mis-tokenized. (Remember, priority is length and then order in the rule list.)
   ; (Leaving the above comment in even though it doesn't apply w/o //abc being a viable identifier - TN 4/11)
   [lex:comment (return-without-pos (lex input-port))]
   
   [(eof) (token-EOF)]
   
   [":" (token-COLON)] 
   ["." (token-DOT)] 
   [";" (token-SEMICOLON)] 
   ["(" (token-LPAREN)] 
   [")" (token-RPAREN)] 
   ["=" (token-EQUALS)] 
   ["," (token-COMMA)]
   ["<" (token-LTHAN)] 
   [">" (token-GTHAN)]
   ["[" (token-LSQBRACK)] 
   ["]" (token-RSQBRACK)]
      
   ; formulas
   
   [(lex-ci "let") (token-LET)]   
   [(lex-ci "be") (token-BE)]
      
   [(lex-ci "true") (token-TRUE)]
   [(lex-ci "and") (token-AND)] 
   [(lex-ci "or") (token-OR)] 
   [(lex-ci "not") (token-NOT)]  
   [(lex-ci "implies") (token-IMPLIES)] 
   [(lex-ci "iff") (token-IFF)] 

   ; commands
   
;   [(lex-ci "compare") (token-COMPARE)] 

   [(lex-ci "realized") (token-REALIZED)] 
   [(lex-ci "unrealized") (token-UNREALIZED)] 
   [(lex-ci "for") (token-FOR)] 
   [(lex-ci "cases") (token-CASES)] 
   [(lex-ci "count") (token-COUNT)] 

   [(lex-ci "show") (token-SHOW)] 
   [(lex-ci "all") (token-ALL)] 
   [(lex-ci "is poss?") (token-ISPOSSQ)] 

   ; command args
   
   [(lex-ci "include") (token-INCLUDE)]    
   [(lex-ci "under") (token-UNDER)] 
   
   ; Directives
   
   [(lex-ci "#defvec") (token-DEFVEC)]      
   [(lex-ci "#set") (token-SET)] 
   [(lex-ci "#get") (token-GET)] 
   [(lex-ci "#info") (token-INFO)] 
   [(lex-ci "#quit") (token-QUIT)]    
   [(lex-ci "#load") (token-LOAD)] 
    
   ; directive options

   [(lex-ci "policy") (token-POLICY)]    
   [(lex-ci "xacml") (token-XACML)] 
   [(lex-ci "sqs") (token-SQS)] 
   [(lex-ci "ios") (token-IOS)]
   [(lex-ci "xacml") (token-XACML)]     
   
   [(lex-ci "with") (token-WITH)]   
   [(lex-ci "in") (token-IN)]   
   
   [(lex-ci "tupling") (token-TUPLING)] 
   [(lex-ci "debug") (token-DEBUG)] 
   [(lex-ci "ceiling") (token-CEILING)] 
   
   ; Natural nums
   [(:: lex:digit (:* lex:digit)) 
    (token-<natural> (string->symbol lexeme))]
   
   ; lowercase-ids
   [(:: lex:lowercase-letter (:* (:or lex:letter lex:digit)))
    (token-<lowercase-id> (string->symbol lexeme))]
   
   ; Capitalized ids
   [(:: lex:uppercase-letter (:* (:or lex:letter lex:digit)))
    (token-<capitalized-id> (string->symbol lexeme))]
   
   ; Quoted ids (trim off the quote)
   [(:: "'" (:+ (:or lex:letter lex:digit)))
    (token-<quoted-id> (string->symbol (substring lexeme 1)))]
   
   ; Identifiers enclosed in double quotes
   ; Can contain symbols. Used for filenames
   ; allow escaping the double-quote via \"   
   
   ; Quoted only lowercase letters and digits? produce a lowercase id:
   [(:: #\" lex:lowercase-letter (:* (:or lex:letter lex:digit)) #\")
    (token-<lowercase-id> (string->symbol 
                             (substring lexeme 1 (- (string-length lexeme) 1))))]
   
   
   ; for now, produce a capitalized id. (will never allow functions or variables or constants in quotes!)
   ; regexp is annoying. Racket requires an escape \, but so does regexp!
   ; so a backslash REGEXP is \\\\ in first arg below
   [(:: #\" (:+ (:or (:: #\\ #\") (char-complement (:or lex:nswhitespace #\")))) #\")
    (token-<capitalized-id> (string->symbol (regexp-replace* "\\\\\"" 
                                                             (substring lexeme 1 (- (string-length lexeme) 1))
                                                             "\"")))]
            
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
; *************************************************
; *************************************************
; Tests

; Return a thunk that gives tokens from s
(define (get-lexer-for s)
  (define in (open-input-string s))
  (port-count-lines! in)
  (lambda () (lex in)))

(define (pretty-position pos)
  (list (position-offset pos)
        (position-line pos)
        (position-col pos)))

; Prints out the tokens generated for a given input string
(define (run-lexer-on s)
  (printf "Running lexer on string: ~a~n" s)
  
  (define lex-func (get-lexer-for s))
  
  (define (inner-func)
    (define my-token (lex-func))
    (printf "~a @ start: ~a, end: ~a. ~n~n" 
            (position-token-token my-token)
            (pretty-position (position-token-start-pos my-token))
            (pretty-position (position-token-end-pos my-token)))
    (unless (equal? 'EOF (position-token-token my-token))
      (inner-func)))

  (inner-func))

; (run-lexer-on "let myquery[x] be R(x) and Foo('CX); show all; 6")
; (run-lexer-on " \"*foo\\\" bar zot\" \"foo\"")