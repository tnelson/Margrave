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
         margrave/helpers
         margrave/lexer)



(provide parse)


;Taken from /collects/parser-tools/examples/read.ss
(define stx-for-original-property (read-syntax #f (open-input-string "original")))

; Special Margrave parser error
; Modified from build-so
(define-syntax (margrave-parse-error stx)
  (syntax-case stx ()    
    ((_ msg start end)
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
                            (string->symbol "source-name"))))
       
       (syntax
        (datum->syntax
         #f
         (raise-read-error 
          msg
          source
          (position-line start-pos)
          (position-col start-pos)
          (position-offset start-pos)      
          (- (position-offset end-pos) (position-offset start-pos)))
         (list source 
               (position-line start-pos)
               (position-col start-pos)
               (position-offset start-pos)
               (- (position-offset end-pos)
                  (position-offset start-pos)))))))))


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
                            (string->symbol "source-name")
                            ;"source-name"
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


; In order to give better error messages, we remember each token in a command (or a malformed command)
;   and clear out the history when done (or when presenting an error)
; We do this by changing the lexer function that the caller gives us before sending it on to the parser.
(define token-history empty)


; Parse ONE COMMAND
;
(define (parse source-name)

  ; Syntax that produces a func that takes a lexer func
  (define internal-parse
      (parser
       (src-pos) 
       (start start)
       ; Stop at either end of file or a ;
       (end EOF SEMICOLON)
       (tokens empty-terminals terminals)
       
       ;**************************************************   
       (error (lambda (tok-ok? token-name token-value start-pos end-pos)                 
                (define rev-tokens (reverse (map position-token-token token-history)))
                
                ;(printf "History: ~a~n" rev-tokens)
                
                (define first-token (if (> (length rev-tokens) 0)
                                        (first rev-tokens)
                                        #f))
                (define second-token (if (> (length rev-tokens) 1)
                                         (second rev-tokens)
                                         #f))                  
                
                ; Can we guess at the kind of command they were trying to use?
                ; customize an error message depending on history:
                (define error-message
                  (cond [(empty? rev-tokens)
                         "Please enter a Margrave command."]
                        [(token? first-token) ; token? is not true of empty-tokens like all the correct command keywords.
                         "Please enter a valid Margrave command."]
                       
                        [(equal? first-token 'RENAME)
                         "Please use the following: RENAME <old name> <new name>"]                        
                                              
                        [(and (> (length rev-tokens) 1)
                              (equal? first-token 'LOAD)
                              (equal? second-token 'IOS))
                         "LOAD IOS must be followed by a configuration file name."]
                        [(and (> (length rev-tokens) 1)
                              (equal? first-token 'LOAD)
                              (equal? second-token 'POLICY))
                         "LOAD POLICY must be followed by a .p file name."]
                        [(and (equal? first-token 'LOAD)
                              (equal? second-token 'XACML))
                         "LOAD IOS must be followed by an XACML policy file name."]
                        [(and (equal? first-token 'LOAD)
                              (equal? second-token 'SQS))
                         "LOAD SQS must be followed by an Amazon SQS JSON file name."]
                        [(equal? first-token 'LOAD)
                         "LOAD what? (LOAD POLICY to load a .p file, LOAD IOS to load a Cisco IOS configuration, etc.)" ]
                        
                        [(equal? first-token 'COUNT)
                         "COUNT is a stand-alone command."]
                        
                        [(and (equal? first-token 'SHOW)
                              (equal? second-token 'REALIZED))
                         "That was not a valid SHOW REALIZED command."]
                        [(and (equal? first-token 'SHOW)
                              (equal? second-token 'UNREALIZED))
                         "That was not a valid SHOW UNREALIZED command."]
                        
                        
                        [(equal? first-token 'SHOW)
                         "SHOW must be followed by a mode. Which SHOW did you intend to use? (SHOW ONE, SHOW ALL, SHOW NEXT, SHOW CEILING...)"]
                        [(equal? first-token 'GET)
                         "GET must be followed by a mode. Which GET did you intend to use? (GET ONE, GET ALL, GET NEXT, GET RULES IN...)"]
                        
                        [(and (equal? first-token 'IS)
                              (equal? second-token 'POSSIBLEQMARK))
                         "IS POSSIBLE? is a standalone command."]
                        [(equal? first-token 'IS)
                         "Did you mean IS POSSIBLE? ?"]
                        
                        [(equal? first-token 'DEFVEC)
                         "A DEFVEC command is of the form DEFVEC <vecid> x, y, z..."]
                        
                        [(equal? first-token 'EXPLORE)
                         (format "EXPLORE must be followed by a boolean condition. Margrave did not understand the condition or options given around \"~a\"." (if token-value
                                                                                                                                                                  token-value
                                                                                                                                                                  token-name))]
                        
                        ; Last resort
                        [else (format "Margrave did not understand that command.")]))
                       
                (set! token-history empty)
                (raise-read-error error-message
                                  source-name
                                  (position-line start-pos)
                                  (position-col start-pos)
                                  (position-offset start-pos)      
                                  (- (position-offset end-pos) (position-offset start-pos)))))
       
       ;**************************************************   
       ;**************************************************   
       
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
       
       ; Uncomment this to help diagnose any reduce/reduce errors that come up after parser revisions.
       ;(debug "parser-debug.txt")
       
       (grammar
        
        ;**************************************************
        ; Streams of tokens are either empty, an error, or a valid margrave-command.
        
        (start 
         ;; If there is an error, ignore everything before the error
         ;; and try to start over right after the error
         [(error start) $2]
         
         ; Prevent trailing whitespace and comments from clogging the parser
         [() #'(IGNORE)]
         
         ; No more script in parser. Single command. Semicolon is dealt with as a termination token above
         [(margrave-command) (begin (set! token-history empty)
                                    $1)])
        
        ;**************************************************        
        (poss-empty-id 
         [(<identifier>) $1]
         [(EMPTYID) ""])   
        
        (list-of-filenames 
         [(<identifier>) (list (symbol->string/safe $1))]
         [(list-of-filenames COMMA <identifier>) (append $1 (list (symbol->string/safe $3)))])
        
        (margrave-command 
         [(explore-statement) $1]
         [(compare-statement) $1]
         [(create-statement) $1]
         [(add-statement) $1]
         
         ;**************************************************

         [(DEFVEC LTHAN <identifier> GTHAN variable-list)
          (build-so (list 'DEFVEC $3 $5) 1 5)]
         
         ; .p/v policy
         [(LOAD POLICY <identifier>) (build-so (list 'LOAD-POLICY $3) 1 3)]
         
         ; Cisco IOS configuration
         [(LOAD IOS <identifier>) (build-so (list 'LOAD-IOS $3) 1 3)]     
         ; With prefix and suffix
         [(LOAD IOS <identifier> WITH poss-empty-id poss-empty-id) (build-so (list 'LOAD-IOS-WITH $3 $5 $6) 1 6)]
         
         ; Multiple configs at once:
         [(LOAD IOS list-of-filenames IN <identifier>) 
          (build-so (list 'LOAD-MULT-IOS $3 (symbol->string/safe $5)) 1 5)]
         [(LOAD IOS list-of-filenames IN <identifier> WITH poss-empty-id poss-empty-id) 
          (build-so (list 'LOAD-MULT-IOS-WITH $3 (symbol->string/safe $5) $7 $8) 1 8)]    
         
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
                
        (compare-statement                  
         [(COMPARE policy policy) (build-so (list 'COMPARE $2 $3 empty) 1 3)]
         [(COMPARE policy policy explore-modifiers-list) (build-so (list 'COMPARE $2 $3 $4) 1 4)]
         )
        
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
         [(UNDER list-of-policies) (build-so (list 'UNDER $2) 1 2)]
         [(PUBLISH variable-list) (build-so (list 'PUBLISH (append (list 'VARIABLE-VECTOR) $2)) 1 2)]
         
         ; Used to be IDBOUTPUT. Internal symbol still is!
         ; (INCLUDE fmla fmla...)
         [(INCLUDE atomic-formula-list) (build-so (append (list 'INCLUDE) $2) 1 2)]
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
        (list-of-policies 
         [(policy) (list $1)]
         [(list-of-policies COMMA policy) (append $1 (list $3))])
        
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
         [(in-formula) (build-so $1 1 1)])
        
        ; *************************************************
        ; represents a top-level condition, the fully-developed formula in EXPLORE 
        ; Can be trivially true
        (condition [(condition-formula) (list 'CONDITION $1)]
                   [(TRUE) (build-so (list 'TRUE-CONDITION) 1 1)])
        
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
                        (list (build-so (list 'CUSTOM-VECTOR-Y $2 $4) 1 5))]
                       [(LTHAN <identifier> COLON <identifier> GTHAN COMMA variable-list)
                        (append (list (build-so (list 'CUSTOM-VECTOR-Y $2 $4) 1 5)) $7)]

                       [(LTHAN <identifier> GTHAN) 
                        (list (build-so (list 'CUSTOM-VECTOR-N $2 ) 1 3))]
                       [(LTHAN <identifier> GTHAN COMMA variable-list)
                        (append (list (build-so (list 'CUSTOM-VECTOR-N $2) 1 3)) $5)])
        
        ;**************************************************    
        
        )))

  ; wrap the parser func. wrapper has same type
  (lambda (gen)             
    (define (new-gen) 
      (let ([token-result (gen)])
        (set! token-history (cons token-result token-history)) ; backwards. reverse later
        token-result))
     (internal-parse new-gen))) 



; end of parser def
