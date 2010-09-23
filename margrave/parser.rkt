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

(require parser-tools/yacc 
         parser-tools/lex
         syntax/stx
         syntax/readerr
         
         margrave/lexer)



(provide parse)


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


; Parse ONE COMMAND
(define (parse source-name)
  (parser
   (src-pos) 
   (start start)
   ; Stop at either end of file or a ;
   (end EOF SEMICOLON)
   (tokens empty-terminals terminals)
   
   (error (lambda (tok-ok? token-name token-value start-pos end-pos) 
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
           
           ; Prevent trailing whitespace and comments from clogging the parser
           [() #'(IGNORE)]
           
           ; stand-alone Margrave command without a semicolon:
           ; removed this production since it's now covered by margrave-script. was a reduce/reduce conflict without removal.
          ; [(margrave-command) $1]            
          
           ; No more script in parser. Single command. Semicolon is dealt with as a termination token above
           [(margrave-command) $1])
           
           ; A margrave-script is a list of margrave commands each ending in a semicolon
           ;[(margrave-script) (build-so (append (list 'MARGRAVE-SCRIPT) $1) 1 1)]) 
    
    ;**************************************************
    ; One production for each kind of command
    
   ; (margrave-script
   ;  [(margrave-command SEMICOLON) (list (build-so (list 'COMMAND $1) 1 2))]
   ;  [(margrave-command) (list (build-so (list 'COMMAND $1) 1 1))]
   ;  [(margrave-command SEMICOLON margrave-script) (append (list (build-so (list 'COMMAND $1) 1 2)) $3)])
         
    ;**************************************************
    
    (poss-empty-id 
     [(<identifier>) $1]
     [(EMPTYID) ""])
    
    (margrave-command 
     [(explore-statement) $1]
     [(create-statement) $1]
     [(add-statement) $1]
         
     ; .p/v policy
     [(LOAD POLICY <identifier>) (build-so (list 'LOAD-POLICY $3) 1 3)]
     
     ; Cisco IOS configuration
     [(LOAD IOS <identifier>) (build-so (list 'LOAD-IOS $3) 1 3)]     
     ; With prefix and suffix
     [(LOAD IOS <identifier> WITH poss-empty-id poss-empty-id) (build-so (list 'LOAD-IOS-WITH $3 $5 $6) 1 6)]
     
     ; XACML configuration
     [(LOAD XACML <identifier>) (build-so (list 'LOAD-XACML $3) 1 3)]
     
     ; Amazon SQS configuration
     [(LOAD SQS <identifier>) (build-so (list 'LOAD-SQS $3) 1 3)]
          
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