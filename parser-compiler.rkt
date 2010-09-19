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

(require racket 
         syntax/stx
         parser-tools/yacc 
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax/readerr
         
         ; Need to require here since SHOW/GET ALL symbols are evaluated here.
         racket/generator)
(require "margrave-xml.rkt" "margrave-policy-vocab.rkt")

(provide
 ; Access the parser through these functions only! (They pre-process)
 parse-and-compile
 evaluate-parse 
 parse-and-compile-read
 parse-and-compile-read-syntax
 parse-and-compile-port
 make-simple-load-script)

;

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
                        EOF WITH TRUE REALIZED UNREALIZED GTHAN LTHAN DOUBLESEMICOLON))
(define-tokens terminals (<identifier> <unsigned-integer> <comment>))



(define-lex-abbrevs
  [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)]
  [lex:nswhitespace (:or #\newline #\return #\tab #\vtab)]
  
  ; The last line of a file may be a comment. Greedy matching will match the entire line up to the end.
  [lex:comment (:: #\/ #\/ (:* (char-complement (:or #\newline #\return))))])

; still rquires whitespace between // and comment text. WHY?

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
; Produce a lexer function
; Assume the caller has downcased the input
(define lex  
  (lexer-src-pos
   [(:+ lex:whitespace) (begin 
                          ;(printf "Skipping whitespace~n") 
                          (return-without-pos (lex input-port)))]
   [(eof) 'EOF]
   
   [":" (token-COLON)] 
   [";;" (token-DOUBLESEMICOLON)] 
   [";" (token-SEMICOLON)] 
   ["(" (token-LPAREN)] 
   [")" (token-RPAREN)] 
   ["=" (token-EQUALS)] 
   ["," (token-COMMA)]
   ["<" (token-LTHAN)] 
   [">" (token-GTHAN)]
   
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
   
      
   ; Comment. Must appear before <identifier> rules or else something like
   ; //abc will be mis-tokenized. (Remember, priority is length and then order in the rule list.)
   [lex:comment (token-<comment> lexeme)]
   
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
      
   ))

; *************************************************


;Taken from /collects/parser-tools/examples/read.ss
(define stx-for-original-property (read-syntax #f (open-input-string "original")))

;Macro that takes a value and the start and end positions of the total expression, and returns a syntax object
(define-syntax (build-so stx)
  (syntax-case stx ()
    ((_ value start end)
     ;First have to bind start-pos, end-pos, and source
     (with-syntax ((start-pos (datum->syntax 
                               (syntax end)
                               (string->symbol 
                                (format "$~a-start-pos"
                                        ;Have to wrap in syntax and then unwrap to datum, because otherwise
                                        ; we get this error: "end: pattern variable cannot be used outside of a template in: end"
                                        (syntax->datum (syntax start))
                                        ))))
                   (end-pos (datum->syntax
                             (syntax end)
                             (string->symbol 
                              (format "$~a-end-pos"
                                      (syntax->datum (syntax end))
                                      ))))                   
                   ;Source is passed in to parse
                   (source (datum->syntax
                            (syntax end)
                            "source-name"
                            ;'source-name
                            )))
       (syntax
        (datum->syntax
         #f
         value
         (list source 
               (position-line start-pos)
               (position-col start-pos)
               (position-offset start-pos)
               (- (position-offset end-pos)
                  (position-offset start-pos)))
         stx-for-original-property)))))) 


(define (parse source-name)
  (parser
   (src-pos) 
   (start start)
   ; Stop at either end of file or a ;;
   (end EOF DOUBLESEMICOLON)
   (tokens empty-terminals terminals)
   
   (error (lambda (tok-ok? token-name token-value start-pos end-pos) 
           ; (printf "Reporting read error: ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~n" tok-ok? token-name token-value start-pos end-pos source-name 
           ;         (position-line start-pos) (position-col start-pos) (position-offset start-pos) (position-offset end-pos))
            (raise-read-error (format "Error parsing token (of type = ~a): ~a" token-name token-value)
                              source-name
                              (position-line start-pos)
                              (position-col start-pos)
                              (position-offset start-pos)      
                              (- (position-offset end-pos) (position-offset start-pos)))))
   
   ; Order of precedence: negation > conjunction > disjunction > implication > bi-implication
   ; Implication is not associative (and of course, neither is the unary operator NOT.)
   ; Precedence in reverse order here. Order of productions doesn't override this.
   (precs (left IFF)
          (nonassoc IMPLIES)          
          (left OR)
          (left AND)
          (nonassoc NOT)
          
          (left COMMA)
          (nonassoc IN)
          (nonassoc COLON))
   (debug "parser-debug.txt")
   
   (grammar
    
    ;**************************************************
    ; Streams of tokens are either empty, an error, or a valid margrave-command.
    
    (start 
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           
           ; stand-alone Margrave command without a semicolon:
           ; removed this production since it's now covered by margrave-script. was a reduce/reduce conflict without removal.
          ; [(margrave-command) $1]            
           
           ; A margrave-script is a list of margrave commands each ending in a semicolon
           [(margrave-script) (build-so (append (list 'MARGRAVE-SCRIPT) $1) 1 1)]) 
    
    ;**************************************************
    ; One production for each kind of command
    
    (margrave-script
     [(margrave-command SEMICOLON) (list (build-so (list 'COMMAND $1) 1 2))]
     [(margrave-command) (list (build-so (list 'COMMAND $1) 1 1))]
     [(margrave-command SEMICOLON margrave-script) (append (list (build-so (list 'COMMAND $1) 1 2)) $3)]
     
     ; Comments are not terminated with semicolons.
     [(comment-token) (list (build-so (list 'COMMENT $1) 1 1))]
     [(comment-token margrave-script) (append (list (build-so (list 'COMMENT $1) 1 1)) $2) ])    
    
    ;**************************************************
    
    (margrave-command 
     [(explore-statement) $1]
     [(create-statement) $1]
     [(add-statement) $1]
         
     [(LOAD POLICY <identifier>) (build-so (list 'LOAD-POLICY $3) 1 3)]
     
     [(RENAME <identifier> <identifier>) (build-so (list 'RENAME $2 $3) 1 3)]

     ; ALL
     [(GET ALL numeric-id) (build-so (list 'GETALL $3) 1 3)]
     [(GET ALL) (build-so (list 'GETALL) 1 2)]
     [(SHOW ALL numeric-id) (build-so (list 'SHOWALL $3) 1 3)]
     [(SHOW ALL) (build-so (list 'SHOWALL) 1 2)]
          
     ; NEXT/ONE/CEILING
     [(GET get-type numeric-id) (build-so (list 'GET $2 $3) 1 3)]
     [(GET get-type) (build-so (list 'GET $2) 1 2)]
     [(SHOW get-type numeric-id) (build-so (list 'SHOW $2 $3) 1 3)]
     [(SHOW get-type) (build-so (list 'SHOW $2) 1 2)]
     
     [(COUNT numeric-id) (build-so (list 'COUNT $2) 1 2)]
     [(COUNT) (build-so (list 'COUNT) 1 1)]
     [(COUNT numeric-id AT SIZE size) (build-so (list 'COUNT-WITH-SIZE $2 $5) 1 5)]
     [(COMPARE policy policy) (build-so (list 'COMPARE $2 $3) 1 3)]
     
     ; SHOW REALIZED and friends
     ; TN 8/26: replaced "populated" with "realized"
     [(SHOW REALIZED numeric-id atomic-formula-list) 
      (build-so (list 'SHOWREALIZED $3 $4 empty) 1 4)]
     [(SHOW UNREALIZED numeric-id atomic-formula-list)
      (build-so (list 'SHOWUNREALIZED $3 $4 empty) 1 4)]
     [(SHOW REALIZED numeric-id atomic-formula-list FOR CASES atomic-formula-list)
      (build-so (list 'SHOWREALIZED $3 $4 $7) 1 7)]
     [(SHOW UNREALIZED numeric-id atomic-formula-list FOR CASES atomic-formula-list)
      (build-so (list 'SHOWUNREALIZED $3 $4 $7) 1 7)]     

     ; SHOW REALIZED without a numeric-id
     [(SHOW REALIZED atomic-formula-list) 
      (build-so (list 'LSHOWREALIZED $3 empty) 1 3)]
     [(SHOW UNREALIZED atomic-formula-list)
      (build-so (list 'LSHOWUNREALIZED $3 empty) 1 3)]
     [(SHOW REALIZED atomic-formula-list FOR CASES atomic-formula-list)
      (build-so (list 'LSHOWREALIZED $3 $6) 1 6)]
     [(SHOW UNREALIZED atomic-formula-list FOR CASES atomic-formula-list)
      (build-so (list 'LSHOWUNREALIZED $3 $6) 1 6)]     
         
     ;IS POSSIBLE?
     [(IS POSSIBLEQMARK numeric-id) (build-so (list 'IS-POSSIBLE? $3) 1 3)]
     [(IS POSSIBLEQMARK) (build-so (list 'IS-POSSIBLE?) 1 1)]
     
     ; Get information
     [(INFO) (build-so (list 'INFO) 1 1)]
     [(INFO <identifier>) (build-so (list 'INFO $2) 1 2)]
     [(GET RULES IN <identifier>) (build-so (list 'GETRULES $4) 1 4) ]
     [(GET QUALIFIED RULES IN <identifier>) (build-so (list 'GETQRULES $5) 1 5)]
     [(GET RULES IN <identifier> WITH DECISION <identifier>) (build-so (list 'GETRULESDEC $4 $7) 1 7)]
     [(GET QUALIFIED RULES IN <identifier> WITH DECISION <identifier>) (build-so (list 'GETQRULESDEC $5 $8) 1 8)]
     
     ; Close out the engine
     [(QUIT) (build-so (list 'QUIT) 1 1)]
     
     ; Margrave command wrapped in parantheses
     [(LPAREN margrave-command RPAREN) (build-so (list 'PARANTHESIZED-EXPRESSION $2) 1 3)]
     )
     ; end of margrave-commend
    
    ;**************************************************
    
    (comment-token [(<comment>) (build-so (list 'COMMENT $1) 1 1)])
    
    ;**************************************************
    ; CREATE statements
    (create-statement
     [(CREATE VOCABULARY vocabulary) (build-so (list 'CREATE-VOCABULARY $3) 1 3)]
     [(CREATE POLICY LEAF policy vocabulary) (build-so (list 'CREATE-POLICY-LEAF $4 $5) 1 5)])
    
    ;**************************************************
    ; ADD
    (add-statement
     [(ADD TO vocabulary add-content) (build-so (list 'ADD $3 $4) 1 4)]

     ;TODO!!: Have to finish this up
     ;These are the functions to use:
     ;(xml-make-decision-type dtype) 
     ;(xml-make-rule-list rule-list) and then:
     ;(define (xml-make-rule rule-name dtype rule-list)
     [(ADD RULE TO policy rule) (build-so (list 'ADD $4 $5) 1 4)])
    (rule
     [(rule-name decision-type rule-list) (build-so (list 'RULE $1 $2 $3) 1 3)])
    (rule-name
     [(<identifier>) (build-so (list 'RULE-NAME $1) 1 1)])
    (decision-type
     [(<identifier>) (build-so (list 'DECISION-TYPE $1) 1 1)])
    (rule-list
     [(<identifier>) (build-so (list 'RULE-LIST $1) 1 1)])
    
    ;**************************************************
    (add-content
     [(SORT sort) $2]
     [(SUBSORT subsort) $2]
     [(DECISION decision) $2]
     [(REQUESTVAR requestvar) $2])
    
    
    
    ;**************************************************
    (explore-statement
     [(EXPLORE condition) (build-so (list 'EXPLORE $2 empty) 1 2)]
     [(EXPLORE condition explore-modifiers-list) (build-so (list 'EXPLORE $2 $3) 1 3)])
    
    ;**************************************************
    ; parameters for GET (one/next, id of result object)
    
    (numeric-id
     [(<unsigned-integer>) (build-so (list 'id $1) 1 1)])
    (get-type
     [(ONE) (build-so (list 'type 'ONE) 1 1)]
     [(NEXT) (build-so (list 'type 'NEXT) 1 1)]
     [(CEILING) (build-so (list 'type 'CEILING) 1 1)])
    
    ;**************************************************
    ; Optional modifiers for the explore statement
    
    (explore-modifier
     [(UNDER policy) (build-so (list 'UNDER $2) 1 2)]
     [(PUBLISH variable-list) (build-so (list 'PUBLISH (append (list 'VARIABLE-VECTOR) $2)) 1 2)]
     
     ; Used to be IDBOUTPUT. Internal symbol still is!
     ; (INCLUDE fmla fmla...)
     [(INCLUDE atomic-formula-list) (build-so (append (list 'IDBOUTPUT) $2) 1 2)]
     [(TUPLING) (build-so (list 'TUPLING) 1 1)]
     [(DEBUG <unsigned-integer>) (build-so (list 'DEBUG $2) 1 2)]
     [(CEILING <unsigned-integer>) (build-so (list 'CEILING $2) 1 2)])
    
    (explore-modifiers-list
     [(explore-modifier) (list $1)]
     [(explore-modifier explore-modifiers-list) (append (list $1) $2)])
    
    ;**************************************************
    ;; ???
    (policy
     [(<identifier>) (build-so (list 'POLICY $1) 1 1)])    
    (vocabulary
     [(<identifier>) (build-so (list 'VOCABULARY $1) 1 1)])  
    (sort
     [(<identifier>) (build-so (list 'SORT $1) 1 1)])
    (subsort
     [(<identifier> <identifier>) (build-so (list 'SUBSORT $1 $2) 1 2)])  
    (decision
     [(<identifier>) (build-so (list 'DECISION $1) 1 1)])
    (requestvar
     [(<identifier> <identifier>) (build-so (list 'REQUESTVAR $1 $2) 1 1)])
    (size
     [(<unsigned-integer>) (build-so (list 'SIZE $1) 1 1)])
    
    
    ; *************************************************
    ; condition-formula: A sub-formula of the condition
    (condition-formula 
     
     [(LPAREN condition-formula RPAREN) (build-so $2 1 3)]
     [(NOT condition-formula) (build-so (list 'NOT $2) 1 2)]     
     [(condition-formula AND condition-formula) (build-so (list 'AND $1 $3) 1 3)]
     [(condition-formula OR condition-formula) (build-so (list 'OR $1 $3) 1 3)]
     [(condition-formula IMPLIES condition-formula) (build-so (list 'IMPLIES $1 $3) 1 3)]
     [(condition-formula IFF condition-formula) (build-so (list 'IFF $1 $3) 1 3)]
     [(equals-formula) (build-so $1 1 1)]
     [(atomic-formula) (build-so $1 1 1)]
     [(in-formula) (build-so $1 1 1)]
     
     ;[(relation) $1]
     ;[(<identifier> COLON relation) "a"]
     )
    
    ; *************************************************
    ; represents a top-level condition, the fully-developed formula in EXPLORE 
    ; Can be trivially true
    (condition [(condition-formula) (list 'CONDITION $1)]
               [(TRUE) (list 'CONDITION (build-so (list 'TRUE) 1 1))])

    ; *************************************************
    ; (x, y, z) IN DB   or x in DB    
    ; syntactic sugar for predicate notation
    ; needs the parens to resolve shift/reduce (deal with this)
    (in-formula ((LPAREN variable-list RPAREN IN <identifier> COLON <identifier>) 
                 (build-so (list 'ATOMIC-FORMULA-Y $5 ":" $7 (append (list 'VARIABLE-VECTOR) $2)) 1 7))
                ((LPAREN variable-list RPAREN IN <identifier>)
                 (build-so (list 'ATOMIC-FORMULA-N $5 (append (list 'VARIABLE-VECTOR) $2)) 1 5))
                ((<identifier> IN <identifier> COLON <identifier>)
                 (build-so (list 'ATOMIC-FORMULA-Y $3 ":" $5 (append (list 'VARIABLE-VECTOR) (list (build-so (list 'VARIABLE $1) 1 1)))) 1 5))
                ((<identifier> IN <identifier>)
                 (build-so (list 'ATOMIC-FORMULA-N $3 (append (list 'VARIABLE-VECTOR) (list (build-so (list 'VARIABLE $1) 1 1)))) 1 3))
                )
    
    
    
    ; *************************************************
    ; Variable equality         
    ; OR constant = variable, variable = constant
    ; context is discovered in Java engine
    (equals-formula ((<identifier> EQUALS <identifier>) (build-so (list 'EQUALS $1 $3) 1 3)))
      
    
    ; *************************************************
    ; An atomic formula can be either of the form
    ; R(x, y, ...)   or
    ; Policyname:R(x, y, ...)   
    
    
    
    (atomic-formula [(<identifier> LPAREN variable-list RPAREN) 
                     (build-so (list 'ATOMIC-FORMULA-N $1 (append (list 'VARIABLE-VECTOR) $3)) 1 4)]
                    [(<identifier> COLON <identifier> LPAREN variable-list RPAREN) 
                     (build-so (list 'ATOMIC-FORMULA-Y $1 ":" $3 (append (list 'VARIABLE-VECTOR) $5)) 1 6)]
                    )
    
    ; ***********************************************************
    ; Used by IDBOUTPUT, SHOW REALIZED, SHOW UNREALIZED
    ; May be a normal atomic formula. May also have an empty vector (no parens).
    
    (nullary-atomic-formula [(<identifier> COLON <identifier>)
                             (build-so (list 'EMPTY-ATOMIC-FORMULA-Y $1 ":" $3 empty) 1 3)]
                            [(<identifier>)
                             (build-so (list 'EMPTY-ATOMIC-FORMULA-N $1 empty) 1 1)])
    
    (non-nullary-atomic-formula ((equals-formula) 
                                 $1)
                                [(atomic-formula) 
                                 $1]
                                [(in-formula)
                                 $1])   
    
    (non-nullary-atomic-formula-list
     [(non-nullary-atomic-formula) (list $1)]
     [(non-nullary-atomic-formula-list COMMA non-nullary-atomic-formula) (append $1 (list $3))])
    
    (nullary-atomic-formula-list
     [(nullary-atomic-formula) (list $1)]
     [(nullary-atomic-formula-list COMMA nullary-atomic-formula) (append $1 (list $3))])    
    
    (atomic-formula-list 
     ((nullary-atomic-formula-list) $1)
     ((non-nullary-atomic-formula-list) $1))
    

    
    ; *************************************************
    ; a vector of variables. e.g.     x, y, z
    ; beware of cons -- it will end up giving us dotted pairs here. we want to build a flat list.
    (variable-list [(<identifier>) (list (build-so (list 'VARIABLE $1) 1 1))]
                   ;[(<identifier> variable-list) (cons $1 $2)]
                   [(<identifier> COMMA variable-list) (append (list (list 'VARIABLE $1)) $3 )]
                   
                   ; For now, only support <polname:req>. 
                   ; Problems with <req> only: We want to allow policies to have different req. vectors.
                   ; So maybe we could detect which policy IDB <req> is appearing in.
                   ;   but it doesn't always have one (saved query, for instance)
                   
                   ; Allow <>, <> for later (via above production)
                   [(LTHAN <identifier> COLON <identifier> GTHAN) 
                    (list (build-so (list 'CUSTOM-VECTOR $2 $4) 1 5))]
                   [(LTHAN <identifier> COLON <identifier> GTHAN COMMA variable-list)
                    (append (list (build-so (list 'CUSTOM-VECTOR $2 $4) 1 5)) $7)]
                   )))) 
; end of parser def


; Take a syntax object for a Margrave command. Return a '(lambda ... 
; May be a single COMMAND, or a MARGRAVE-SCRIPT with a list of commands.
(define (compile-margrave-syntax syn)
  (let* ([interns (syntax-e syn)])
   ; (printf "CONVERTING: syn=~a interns=~a ~n" syn interns)
    (let* ([first-intern (first interns)] 
           [first-datum (syntax->datum first-intern)])
      (cond 
        
        ; ************************************
        
        ; Single command: compile its contents
        [(equal? first-datum 'COMMAND) (compile-margrave-syntax (second interns))]
        ; Multiple commands: compile each command separately and return a list.
        [(equal? first-datum 'MARGRAVE-SCRIPT) (compose-scripts (map compile-margrave-syntax (rest interns)))]
        
        ; ************************************
        ; ************************************
        ; ************************************
        [(equal? first-datum 'PARANTHESIZED-EXPRESSION)
         (compile-margrave-syntax (second interns))]
        
        ; third and fourth of the list are func syntax that create vocab, pol respectively
        ; So create an uber-func that does both
        [(equal? first-datum 'LOAD-POLICY)
         (let ([policy-creation-list (evaluate-policy (symbol->string (syntax->datum (second interns))))])
           (make-simple-load-script (append (third policy-creation-list)
                                       (fourth policy-creation-list))))]
        
        ; ************************************        
        ; Commands are handled here. Inner syntax is handled by
        ; recursive calls to helper-syn->xml
        
        [(equal? first-datum 'CREATE-VOCABULARY)
         (make-single-wrapper `(xml-make-command "CREATE VOCABULARY" ,(map helper-syn->xml (rest interns))))]
        
        [(equal? first-datum 'ADD)
         (make-single-wrapper `(xml-make-command "ADD" ,(map helper-syn->xml (rest interns))))]
                       
        [(equal? first-datum 'CREATE-POLICY-LEAF)
         (make-single-wrapper `(xml-make-create-policy-leaf-command ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns))))]
        
        [(equal? first-datum 'IS-POSSIBLE?)
         (make-single-wrapper `(xml-make-is-possible-command ,(if (< 1 (length interns))
                                                                (helper-syn->xml (second interns))
                                                                '(xml-make-id "-1"))))]
        [(equal? first-datum 'COUNT)
         (make-single-wrapper `(xml-make-count-command 
                                ,(if (< 1 (length interns))
                                     (helper-syn->xml (second interns))
                                     '(xml-make-id "-1"))))]
        [(equal? first-datum 'COUNT-WITH-SIZE)
         (make-single-wrapper `(xml-make-count-with-size-command ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns))))]
        
        
        [(equal? first-datum 'EXPLORE)
         ; (printf "Symbol exp: ~a ~a ~a~n" first-intern (second interns) (third interns))
         (make-single-wrapper 
           `(xml-make-explore-command 
             (list ,(helper-syn->xml (second interns)))
             ;List of modifiers could be empty, but use (list instead of '( since we may have funcs to evaluate inside
             (list ,@(if (empty? (rest (rest interns)))
                        empty
                        (map helper-syn->xml (syntax-e (third interns)))))))] 
        
        
        ; id, list, optional for-cases list
        [(equal? first-datum 'SHOWREALIZED)
         ;(printf "~a ~n" (syntax->datum (second interns)))
         (if (empty? (fourth interns))
             (make-single-wrapper 
              `(xml-make-show-realized-command ,(helper-syn->xml (second interns)) 
                                               ,(map helper-syn->xml (syntax-e (third interns)))))
             (make-single-wrapper 
              `(xml-make-show-realized-command ,(helper-syn->xml (second interns)) 
                                               ,(append (map helper-syn->xml (syntax-e (third interns))) 
                                                        (list `(xml-make-forcases ,(map helper-syn->xml (syntax-e (fourth interns)))))))))]
        [(equal? first-datum 'SHOWUNREALIZED)
         (if (empty? (fourth interns))
             (make-single-wrapper
              `(xml-make-show-unrealized-command ,(helper-syn->xml (second interns)) 
                                                 ,(map helper-syn->xml (syntax-e (third interns)))))
             (make-single-wrapper
              `(xml-make-show-unrealized-command ,(helper-syn->xml (second interns))
                                                 ,(append (map helper-syn->xml (syntax-e (third interns))) 
                                                          (list `(xml-make-forcases ,(map helper-syn->xml (syntax-e (fourth interns)))))))))]
        ; same but without the result ID
        [(equal? first-datum 'LSHOWREALIZED)
         ;(printf "~a ~n" (syntax->datum (second interns)))
         (if (empty? (third interns))
             (make-single-wrapper 
              `(xml-make-show-realized-command (xml-make-id "-1") 
                                               (map helper-syn->xml (syntax-e (second interns)))))
             (make-single-wrapper 
              `(xml-make-show-realized-command (xml-make-id "-1")
                                               (append (map helper-syn->xml (syntax-e (second interns))) 
                                                       (list `(xml-make-forcases ,(map helper-syn->xml (syntax-e (third interns)))))))))]
        [(equal? first-datum 'LSHOWUNREALIZED)
         (if (empty? (third interns))
             (make-single-wrapper 
              `(xml-make-show-unrealized-command (xml-make-id "-1")
                                                 ,(map helper-syn->xml (syntax-e (second interns)))))
             (make-single-wrapper 
              `(xml-make-show-unrealized-command (xml-make-id "-1")
                                                 ,(append (map helper-syn->xml (syntax-e (second interns))) 
                                                          (list `(xml-make-forcases ,(map helper-syn->xml (syntax-e (third interns)))))))))]
        
        [(equal? first-datum 'RENAME)
         (make-single-wrapper
          `(xml-make-rename-command ,(symbol->string (syntax->datum (second interns)))
                                    ,(symbol->string (syntax->datum (third interns)))))]
        
        ; pass (type ONE) to get first
        ;;     (type NEXT) to get next in Java's iterator
        
        [(equal? first-datum 'GET)
         (make-single-wrapper
          `(xml-make-get-command ,(helper-syn->xml (second interns)) 
                                 ;Use -1 if nothing is supplied
                                 ,(if (< 2 (length interns))
                                    (helper-syn->xml (third interns))
                                    '(xml-make-id "-1"))))]
        ; Like GET, only pretty-print the result
        [(equal? first-datum 'SHOW)
         ;         (printf "~a ~a ~n" (second interns) (if (< 2 (length interns)) (third interns) "last" ))
         `(lambda () (pretty-print-response-xml 
                      (send-and-receive-xml
                       (xml-make-get-command ,(helper-syn->xml (second interns)) 
                                             ;Use -1 if nothing is supplied
                                             ,(if (< 2 (length interns))
                                                  (helper-syn->xml (third interns))
                                                  '(xml-make-id "-1"))))))]
        
        
       ; ALL gets its own command type:
        [(equal? first-datum 'SHOWALL)
         (make-show-all  
          (if (< 2 (length interns))
              (helper-syn->xml (second interns))
              '(xml-make-id "-1")))]
        
        [(equal? first-datum 'GETALL)
         (make-get-all  
          (if (< 2 (length interns))
              (helper-syn->xml (second interns))
              '(xml-make-id "-1")))]
        
        [(equal? first-datum 'INFO)
         (if (empty? (rest interns))
             (make-single-wrapper `(xml-make-info-command))
             (make-single-wrapper `(xml-make-info-id-command ,(symbol->string (syntax->datum (second interns))))))]
        
        ; Allow user to get the rules in a policy
        [(equal? first-datum 'GETRULES)
         (make-single-wrapper `(xml-make-get-rules-command ,(syntax->datum (second interns))))]
        [(equal? first-datum 'GETQRULES)
         (make-single-wrapper `(xml-make-get-qrules-command ,(syntax->datum (second interns))))]
        [(equal? first-datum 'GETRULESDEC)
         (make-single-wrapper `(xml-make-get-rules-command ,(syntax->datum (second interns)) ,(symbol->string (syntax->datum (third interns)))))]
        [(equal? first-datum 'GETQRULESDEC)
         (make-single-wrapper `(xml-make-get-qrules-command ,(syntax->datum (second interns)) ,(symbol->string (syntax->datum (third interns)))))]
                
        [(equal? first-datum 'QUIT)
         '(lambda () (stop-margrave-engine))]
        
        ; Do nothing with a comment
        [(equal? first-datum 'COMMENT)
         '(lambda () (void))]
        
        [else
         (printf "UNEXPECTED COMMAND SYMBOL: ~a ~a ~n" first-intern first-datum)]))))


(define (compose-scripts list-of-func-syntax)
  ;(printf "CS: ~a~n" list-of-func-syntax)
  `(lambda () (list ,@(map (lambda (f) `(,f)) 
                           list-of-func-syntax))))

; Show-all is based on get-all
(define (make-show-all explore-id)
  `(lambda () (let* ([string-buffer (open-output-string)]
                     [the-generator-func ,(make-get-all explore-id)]
                     [the-generator (the-generator-func)])
                
                ; iterate over results of the generator.
                ; make sure to STOP at unsat.  
                (letrec ([helper-func (lambda () 
                                        (let ([response (the-generator)]) 
                                          (when (not (response-is-unsat? response))
                                            (write-string (pretty-print-response-xml response) string-buffer) 
                                            (write-string "\n" string-buffer)                                                 
                                            (helper-func))))])
                  (helper-func)
                  (let ([result (get-output-string string-buffer)])
                    (if (equal? result "")
                        "---> NO RESULTS <---"
                        result))))))

; GET ALL returns a generator for all the models
; using 2nd form of let and being tail-recursive
(define (make-get-all explore-id)
  `(lambda () 
     (generator ()  
                (let loop-func ([is-first #t])
                  (if (equal? is-first #f)
                      (begin
                        (yield (send-and-receive-xml (xml-make-get-command `(type "NEXT") ,explore-id)))
                        (loop-func #f))
                      (begin
                        (yield (send-and-receive-xml (xml-make-get-command `(type "ONE") ,explore-id)))                                 
                        (loop-func #f)))))))

; This is the *symbol* 'send-and-receive-xml, not the function
; It gets evaluated in a context where we know what the symbol means.
(define (make-single-wrapper thexml-constructor)  
  `(lambda () (send-and-receive-xml ,thexml-constructor)))

(define (make-simple-load-script list-of-xexprs)
#| gmarceau personal preference:
  `(lambda () ,@(for/list ([an-xml list-of-xml])
                          `(send-and-receive-xml ,an-xml))))
|#
  `(lambda () (list ,@(map (lambda (an-xexpr) 
                             `(send-and-receive-xml ',an-xexpr)) 
                           list-of-xexprs))))

(define (syntax->string s)
  (symbol->string (syntax->datum s)))

(define (helper-syn->xml syn)
  (let* ([interns (syntax-e syn)])
   ; (printf "CONVERTING: syn=~a interns=~a ~n" syn interns)
    (let* ([first-intern (first interns)] 
           [first-datum (syntax->datum first-intern)])
      (cond 
        ; ************************************      
        ; Syntax inside commands. These have XML semantics, not function-syntax semantics
        
        [(equal? first-datum 'VOCABULARY)
         `(xml-make-vocab-identifier ,(symbol->string (syntax->datum (second interns))))]
        
        [(equal? first-datum 'SORT)
         `(xml-make-sort ,(symbol->string (syntax->datum (second interns))))]
        [(equal? first-datum 'SUBSORT)
         `(xml-make-subsort ,(symbol->string (syntax->datum (second interns)))
                           ,(symbol->string (syntax->datum (third interns))))]
        [(equal? first-datum 'DECISION)
         `(xml-make-decision ,(symbol->string (syntax->datum (second interns))))]
        [(equal? first-datum 'REQUESTVAR)
         `(xml-make-request-var ,(symbol->string (syntax->datum (second interns)))
                               ,(symbol->string (syntax->datum (third interns))))]
                
        [(equal? first-datum 'SIZE)
         `(xml-make-size ,(symbol->string (syntax->datum (second interns))))]
        [(equal? first-datum 'COMPARE)
         `(xml-make-size ,(symbol->string (syntax->datum (second interns))))]
        
        [(equal? first-datum 'VARIABLE) ;Will be returned to variable vector
         ;(printf "Symbol var: ~a~n" first-intern)
         (symbol->string (syntax->datum (second interns)))]
        
        ; <polname>:<vecname>
        [(equal? first-datum 'CUSTOM-VECTOR)
         ; Is there a better way to do this?
         `(resolve-custom-vector ',(syntax->datum (second interns)) ',(syntax->datum (third interns)) ,(syntax-line (second interns)) ,(syntax-column (second interns)))]        
        
        ; If vecname is not "req", error.
        ; If polname does not exist, error.                           
        ; Otherwise ask for info on polname and produce a variable vector for its request vector
         

        
        [(equal? first-datum 'VARIABLE-VECTOR)
         ;(printf "Symbol varvec: ~a~n" first-intern)
         ; flatten, not append because some map results are lists, others not
         `(xml-make-identifiers-list (flatten (list ,@(map helper-syn->xml (rest interns)))))]
        
        [(equal? first-datum 'TRUE)
         '(TRUE)]
        
        [(equal? first-datum 'ATOMIC-FORMULA-N)
         `(xml-make-atomic-formula-n ',(syntax->datum (second interns)) 
                                     ,(helper-syn->xml (third interns)))]
        
        [(equal? first-datum 'ATOMIC-FORMULA-Y)
         ;Third is the colon
         `(xml-make-atomic-formula-y ',(syntax->datum (second interns))
                                     ',(syntax->datum (fourth interns)) 
                                     ,(helper-syn->xml (fifth interns)))]
        
        [(equal? first-datum 'EMPTY-ATOMIC-FORMULA-N)
         `(xml-make-atomic-formula-n ',(syntax->datum (second interns))
                                     empty)]
        
        [(equal? first-datum 'EMPTY-ATOMIC-FORMULA-Y)        
         ;Third is the colon
         `(xml-make-atomic-formula-y ',(syntax->datum (second interns))
                                     ',(syntax->datum (fourth interns))
                                     empty)]
        
        [(equal? first-datum 'EQUALS)
         `(xml-make-equals-formula ,(symbol->string (syntax->datum (second interns))) ,(symbol->string (syntax->datum (third interns))))]
        
        [(equal? first-datum 'CONDITION)
         (helper-syn->xml (second interns))]      

        [(equal? first-datum 'TUPLING)
         `(xml-make-tupling)]
        [(equal? first-datum 'CEILING)
         `(xml-make-ceiling ,(if (< 1 (length interns))
                                   (syntax->string (second interns))
                                   (xml-make-id "-1")))]
        [(equal? first-datum 'DEBUG)
         `(xml-make-debug ,(syntax->string (second interns)))]
        [(equal? first-datum 'UNDER)
         `(xml-make-under ,(helper-syn->xml (second interns)))]
        [(equal? first-datum 'POLICY)
         `(xml-make-policy-identifier ,(syntax->string (second interns)))]
        [(equal? first-datum 'PUBLISH)
         `(xml-make-publish ,(helper-syn->xml (second interns)))]
        [(equal? first-datum 'AND)
         `(xml-make-and ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns)))]                
        [(equal? first-datum 'OR)
         `(xml-make-or ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns)))]
        [(equal? first-datum 'IMPLIES)
         `(xml-make-implies ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns)))]
        [(equal? first-datum 'IFF)
         `(xml-make-iff ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns)))]
        [(equal? first-datum 'NOT)
         `(xml-make-not ,(helper-syn->xml (second interns)))]

        
        
        [(equal? first-datum 'type)
         `(xml-make-type ,(syntax->string (second interns)))]
        [(equal? first-datum 'id)
         `(xml-make-id ,(syntax->string (second interns)))]
        [(equal? first-datum 'IDBOUTPUT)
         `(xml-make-idbout ,(map helper-syn->xml (rest interns)))]
        [else
         (printf "UNEXPECTED SYMBOL: ~a ~a ~n" first-intern first-datum)]))))

; ***************************************************************************************
; !!! Don't need to string-downcase anymore. The lexer is case-insensitive now.
; Access the parser through these functions ONLY!
(define (evaluate-parse sn s)
  (let ((in (open-input-string (string-downcase s))))
    (port-count-lines! in)
    ((parse sn) (lambda() (lex in)))))

(define (parse-and-compile s)
  (let ((in (open-input-string (string-downcase s))))
    (port-count-lines! in)
    (compile-margrave-syntax ((parse "source") (lambda () (lex in))))))

(define (parse-and-compile-port src in)
  (port-count-lines! in)  
  (compile-margrave-syntax ((parse src) (lambda () (lex in)))))

(define (parse-and-compile-read in)
  (port-count-lines! in)
  (compile-margrave-syntax ((parse "source") (lambda () (lex in)))))

(define (parse-and-compile-read-syntax in src line col pos)
  (port-count-lines! in)
  (compile-margrave-syntax ((parse src) (lambda () (lex in)))))

; ***************************************************************************************
