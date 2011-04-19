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
         
         ; !!! todo: update this
         ;margrave/lexer
         (file "lexer.rkt")
         )


(provide parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Helpers

(define (started-with token-list)  
  (define rev-tokens (reverse (map position-token-token token-history)))  
  (and (>= (length rev-tokens) (length token-list))
       (equal? token-list (take rev-tokens (length token-list)))))

(define (started-with-identifier/ci val)  
  (unless (empty? token-history)
    (define the-tok (position-token-token (last token-history)))  
    ;(printf "~a~n" the-tok)
    (and (token? the-tok)
         (equal? (string-downcase (symbol->string (token-value the-tok))) (string-downcase (symbol->string val))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Taken from /collects/parser-tools/examples/read.ss
(define stx-for-original-property (read-syntax #f (open-input-string "original")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; In order to give better error messages, we remember each token in a command (or a malformed command)
;   and clear out the history when done (or when presenting an error)
; We do this by changing the lexer function that the caller gives us before sending it on to the parser.
(define token-history empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                ; Token-history is reversed here.
                ;(printf "History: ~a~n" (map position-token-token token-history))
                
                ; Can we guess at the kind of command they were trying to use?
                ; customize an error message depending on history:
                (define error-message
                  (cond [(empty? token-history)
                         "Please enter a Margrave command."]
                        ;[(token? first-token) ; token? is not true of empty-tokens like all the correct command keywords.
                        ; "Please enter a valid Margrave command."]
                                                                     
                        [(started-with '(LOAD IOS))
                         "To load an IOS configuration, use #LOAD IOS <file name>. Other options are given in the documentation."]
                                                
                        [(started-with-identifier/ci 'LOAD)
                         "The #LOAD directive must be prefixed by a # symbol."]
                        
                        [(started-with '(LOAD POLICY))
                         "To load a .p policy file, use #LOAD POLICY <desired name> = <file name>"]
                        
                        [(started-with '(LOAD XACML))
                         "To load an XACML policy file, use #LOAD XACML <desired name> = <file name>"]
                        
                        [(started-with '(LOAD SQS))
                         "To load an Amazon SQS policy file, use #LOAD SQS <desired name> = <file name>"]
                        
                        [(started-with '(LOAD))
                         "Load what? (#LOAD POLICY to load a .p file, #LOAD IOS to load a Cisco IOS configuration, etc.)" ]
                        
                        [(started-with '(LET))
                         (format "To bind a formula context, use LET <name>[<variable-declarations>] BE <condition>. 
Margrave did not understand the condition or options given around \"~a\"." 
                                 (if token-value
                                     token-value
                                     token-name))]
                        
                        
;                        [(equal? first-token 'COUNT)
;                         "COUNT is a stand-alone command."]
;                        
;                        [(and (equal? first-token 'SHOW)
;                              (equal? second-token 'REALIZED))
;                         "That was not a valid SHOW REALIZED command."]
;                        [(and (equal? first-token 'SHOW)
;                              (equal? second-token 'UNREALIZED))
;                         "That was not a valid SHOW UNREALIZED command."]
;                        
;                        
;                        [(equal? first-token 'SHOW)
;                         "SHOW must be followed by a mode. Which SHOW did you intend to use? (SHOW ONE, SHOW ALL, SHOW NEXT, SHOW CEILING...)"]
;                        [(equal? first-token 'GET)
;                         "GET must be followed by a mode. Which GET did you intend to use? (GET ONE, GET ALL, GET NEXT, GET RULES IN...)"]
;                        
;                        [(and (equal? first-token 'IS)
;                              (equal? second-token 'POSSIBLEQMARK))
;                         "IS POSSIBLE? is a standalone command."]
;                        [(equal? first-token 'IS)
;                         "Did you mean IS POSSIBLE? ?"]
;                        
;                        [(equal? first-token 'DEFVEC)
;                         "A DEFVEC command is of the form DEFVEC <vecid> x, y, z..."]
;                        
                        
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
       ; end of error handling
       
       ; Order of precedence: negation > conjunction > disjunction > implication > bi-implication
       ; Implication is not associative (and of course, neither is the unary operator NOT.)
       ; Precedence in reverse order here. Order of productions doesn't override this.
       (precs (left IFF)
              (nonassoc IMPLIES)          
              (left OR)
              (left AND)
              (nonassoc NOT)
              
              (left COMMA)
             ;(nonassoc IN)
              (nonassoc COLON)
              (nonassoc DOT))
       
       ; Uncomment this to help diagnose any reduce/reduce errors that come up after parser revisions.
       ;(debug "parser-debug.txt")
       
       (grammar
        
        ;**************************************************
        ; Streams of tokens are either empty, an error, or 
        ; (a) a fmla-binding statement;
        ; (b) a scenario-finding command
        ; (c) a directive
        
        (start 
         ;; If there is an error, ignore everything before the error
         ;; and try to start over right after the error
         [(error start) $2]
         
         ; Prevent trailing whitespace and comments from clogging the parser
         [() #'(IGNORE)]
         
         ; Single command. Semicolon is dealt with as a termination token above
         [(m-bind-fmla) (begin (set! token-history empty)
                                    $1)]
         [(m-command) (begin (set! token-history empty)
                                    $1)]
         [(m-directive) (begin (set! token-history empty)
                                    $1)])
        
        ;**************************************************        
        ; Helper productions
                
        (file-id
         [(<lowercase-id>) $1]
         [(<capitalized-id>) $1])
        
        (list-of-filenames 
         [(file-id) (list (symbol->string/safe $1))]
         [(list-of-filenames COMMA file-id) (append $1 (list (symbol->string/safe $3)))])
        
        (numeric-id
         [(<natural>) (build-so (list 'id $1) 1 1)])
        
        ;(get-type
        ; [(ONE) (build-so (list 'type 'ONE) 1 1)]
        ; [(NEXT) (build-so (list 'type 'NEXT) 1 1)]
        ; [(CEILING) (build-so (list 'type 'CEILING) 1 1)])
       
                        
        ;**************************************************
        ;(m-bind-fmla
        ; [(let-statement) $1]
         ;[(compare-statement) $1]
         ;)
        
        ;**************************************************
        (m-directive
                   
         ; Get information
         [(INFO) (build-so (list 'INFO) 1 1)]
         [(INFO <capitalized-id>) (build-so (list 'INFO $2) 1 2)]
         
         ; Close out the engine
         [(QUIT) (build-so (list 'QUIT) 1 1)]
         
         ; !!! todo: re-enable
         ;[(DEFVEC LTHAN <lowercase-id> GTHAN variable-list)
         ; (build-so (list 'DEFVEC $3 $5) 1 5)]
         
         ; .p/v policy
         [(LOAD POLICY <capitalized-id> EQUALS file-id) (build-so (list 'LOAD-POLICY $3 $5) 1 5)]
         
         ; Cisco IOS configuration
         [(LOAD IOS file-id) (build-so (list 'LOAD-IOS $3) 1 3)]     
         
         ; With prefix and suffix
         [(LOAD IOS file-id WITH file-id file-id) (build-so (list 'LOAD-IOS-WITH $3 $5 $6) 1 6)]
         
         ; Multiple configs at once:
         [(LOAD IOS list-of-filenames IN file-id) 
          (build-so (list 'LOAD-MULT-IOS $3 (symbol->string/safe $5)) 1 5)]
         [(LOAD IOS list-of-filenames IN file-id WITH file-id file-id) 
          (build-so (list 'LOAD-MULT-IOS-WITH $3 (symbol->string/safe $5) $7 $8) 1 8)]    
         
         ; XACML configuration
         [(LOAD XACML <capitalized-id> EQUALS file-id) (build-so (list 'LOAD-XACML $3) 1 3)]
         
         ; Amazon SQS configuration
         [(LOAD SQS <capitalized-id> EQUALS file-id) (build-so (list 'LOAD-SQS $3) 1 3)]         
         
         )
         
        ; Note the new loading paradigm: user has to provide policy ID at the command line
        ; prevents ambiguity resulting from just taking what's in the policy file
        
        ;**************************************************
        (m-command
         
         ; !!! todo: re-enable implicit "last" without identifier given
         
         ; ALL
         ;[(GET ALL numeric-id) (build-so (list 'GETALL $3) 1 3)]
         ;[(GET ALL) (build-so (list 'GETALL) 1 2)]
         [(SHOW ALL <capitalized-id>) (build-so (list 'SHOWALL $3) 1 3)]
         ;[(SHOW ALL) (build-so (list 'SHOWALL) 1 2)]
         
         ; 
         ;[(GET get-type numeric-id) (build-so (list 'GET $2 $3) 1 3)]
         ;[(GET get-type) (build-so (list 'GET $2) 1 2)]
         ;[(SHOW get-type numeric-id) (build-so (list 'SHOW $2 $3) 1 3)]
         ;[(SHOW get-type) (build-so (list 'SHOW $2) 1 2)]
         [(SHOW <capitalized-id>) (build-so (list 'SHOW $2) 1 2)]
         ;[(SHOW ) (build-so (list 'SHOW $2) 1 1)]
         
         [(RESET <capitalized-id>) (build-so (list 'RESET $2) 1 2)]
         
         [(COUNT <capitalized-id>) (build-so (list 'COUNT $2) 1 2)]
         ;[(COUNT) (build-so (list 'COUNT) 1 1)]
         [(COUNT <capitalized-id> AT size) (build-so (list 'COUNT-WITH-SIZE $2 $4) 1 4)]
                  
         ; SHOW REALIZED and friends
         ; TN 8/26: replaced "populated" with "realized"
         [(SHOW REALIZED <capitalized-id> atomic-formula-list) 
          (build-so (list 'SHOWREALIZED $3 $4 empty) 1 4)]
         [(SHOW UNREALIZED <capitalized-id> atomic-formula-list)
          (build-so (list 'SHOWUNREALIZED $3 $4 empty) 1 4)]
         [(SHOW REALIZED <capitalized-id> atomic-formula-list FOR CASES atomic-formula-list)
          (build-so (list 'SHOWREALIZED $3 $4 $7) 1 7)]
         [(SHOW UNREALIZED <capitalized-id> atomic-formula-list FOR CASES atomic-formula-list)
          (build-so (list 'SHOWUNREALIZED $3 $4 $7) 1 7)]     
         
         ; SHOW REALIZED without a numeric-id
         ;[(SHOW REALIZED atomic-formula-list) 
         ; (build-so (list 'LSHOWREALIZED $3 empty) 1 3)]
         ;[(SHOW UNREALIZED atomic-formula-list)
         ; (build-so (list 'LSHOWUNREALIZED $3 empty) 1 3)]
         ;[(SHOW REALIZED atomic-formula-list FOR CASES atomic-formula-list)
         ; (build-so (list 'LSHOWREALIZED $3 $6) 1 6)]
         ;[(SHOW UNREALIZED atomic-formula-list FOR CASES atomic-formula-list)
         ; (build-so (list 'LSHOWUNREALIZED $3 $6) 1 6)]     
         
         ;IS POSSIBLE?
         [(ISPOSSQ <capitalized-id>) (build-so (list 'IS-POSSIBLE? $2) 1 2)]
         ;[(ISPOSSQ) (build-so (list 'IS-POSSIBLE?) 1 1)]        
         
         ; Margrave command wrapped in parantheses
         ; !!! todo: why is this here? - tn removed for now
         ;[(LPAREN margrave-command RPAREN) (build-so (list 'PARANTHESIZED-EXPRESSION $2) 1 3)]
         )    
        
        ;**************************************************
        ;**************************************************
        ;**************************************************
        
        (typed-variable-list
         [(typed-variable-term) (list $1)]
         [(typed-variable-list COMMA typed-variable-term) (append $1 (list $3))])
        
        (typed-variable-term
         [(variable-term COLON <capitalized-id>) (build-so (list 'VARIABLE-DECL $1 $3) 1 3)])
        
        (m-bind-fmla
         [(LET <capitalized-id> LSQBRACK typed-variable-list RSQBRACK BE condition) 
          (build-so (list 'EXPLORE $2 (append (list 'TERM-LIST) $4) $7 empty) 1 7)]
         [(LET <capitalized-id> LSQBRACK typed-variable-list RSQBRACK BE condition explore-modifiers-list) 
          (build-so (list 'EXPLORE $2 (append (list 'TERM-LIST) $4) $7 $8) 1 8)])
                
        ;(compare-statement                  
        ; [(COMPARE policy policy) (build-so (list 'COMPARE $2 $3 empty) 1 3)]
        ; [(COMPARE policy policy explore-modifiers-list) (build-so (list 'COMPARE $2 $3 $4) 1 4)]
        ; )
                        
        ;**************************************************
        ; Optional modifiers for the explore statement
        
        (explore-modifier
         [(UNDER list-of-policies) (build-so (list 'UNDER $2) 1 2)]         
         
         ;[(INCLUDE atomic-formula-list) (build-so (append (list 'INCLUDE) $2) 1 2)]
         [(TUPLING) (build-so (list 'TUPLING) 1 1)]
         [(DEBUG <natural>) (build-so (list 'DEBUG $2) 1 2)]
         [(CEILING <natural>) (build-so (list 'CEILING $2) 1 2)])
        
        (explore-modifiers-list
         [(explore-modifier) (list $1)]
         [(explore-modifier explore-modifiers-list) (append (list $1) $2)])
        
        (policy
         [(<capitalized-id>) (build-so (list 'POLICY $1) 1 1)])
        
        (list-of-policies 
         [(policy) (list $1)]
         [(list-of-policies COMMA policy) (append $1 (list $3))])
        
        (size
         [(<natural>) (build-so (list 'SIZE $1) 1 1)])
        
        
        ; *************************************************
        ; condition-formula: A sub-formula of the condition
        (condition-formula          
         [(LPAREN condition-formula RPAREN) (build-so $2 1 3)]
         [(NOT condition-formula) (build-so (list 'NOT $2) 1 2)]     
         [(condition-formula AND condition-formula) (build-so (list 'AND $1 $3) 1 3)]
         [(condition-formula OR condition-formula) (build-so (list 'OR $1 $3) 1 3)]
         [(condition-formula IMPLIES condition-formula) (build-so (list 'IMPLIES $1 $3) 1 3)]
         [(condition-formula IFF condition-formula) (build-so (list 'IFF $1 $3) 1 3)]
         
         ; !!! todo, more than one per FORLALL, e.g.: FORALL x:A, y:B, ...
         ; !!! also parens or not?
         [(FORALL <lowercase-id> COLON <capitalized-id> LPAREN condition-formula RPAREN)
          (build-so (list 'FORALL $2 $4 $6) 1 6)]
         [(EXISTS <lowercase-id> COLON <capitalized-id> LPAREN condition-formula RPAREN) 
          (build-so (list 'EXISTS $2 $4 $6) 1 6)]
         
         [(atomic-formula) (build-so $1 1 1)])
        
        ; *************************************************
        ; represents a top-level condition, the fully-developed formula in EXPLORE 
        ; Can be trivially true
        (condition [(condition-formula) (list 'CONDITION $1)]
                   [(TRUE) (build-so (list 'TRUE-CONDITION) 1 1)])
        
        ;**************************************************
        ; terms
        (variable-term [(<lowercase-id>) (build-so (list 'VARIABLE-TERM $1) 1 1)])
        (constant-term [(<quoted-id>) (build-so (list 'CONSTANT-TERM $1) 1 1)])
        (function-term [(<lowercase-id> LPAREN condition-term-list RPAREN) (build-so (list 'FUNCTION-TERM $1 $3) 1 4)])
        
        (condition-term-list
         [(condition-term) (list $1)]
         [(condition-term-list COMMA condition-term) (append $1 (list $3))])
        
        ; a term is a variable, a constant, or a function called on multiple sub-terms
        (condition-term 
         [(variable-term) $1]
         [(constant-term) $1]
         [(function-term) $1])
        
        
        ; *************************************************
        ; (x, y, z) IN DB   or x in DB    
        ; syntactic sugar for predicate notation
        ; needs the parens to resolve shift/reduce (deal with this)
      ;  (in-formula ((LPAREN variable-list RPAREN IN <identifier> COLON <identifier>) 
      ;               (build-so (list 'ATOMIC-FORMULA-Y $5 ":" $7 (append (list 'VARIABLE-VECTOR) $2)) 1 7))
      ;              ((LPAREN variable-list RPAREN IN <identifier>)
      ;               (build-so (list 'ATOMIC-FORMULA-N $5 (append (list 'VARIABLE-VECTOR) $2)) 1 5))
      ;              ((<identifier> IN <identifier> COLON <identifier>)
      ;               (build-so (list 'ATOMIC-FORMULA-Y $3 ":" $5 (append (list 'VARIABLE-VECTOR) (list (build-so (list 'VARIABLE $1) 1 1)))) 1 5))
      ;              ((<identifier> IN <identifier>)
      ;               (build-so (list 'ATOMIC-FORMULA-N $3 (append (list 'VARIABLE-VECTOR) (list (build-so (list 'VARIABLE $1) 1 1)))) 1 3))
       ;;             )
        
        ; !!! removed sugar for initial parser - TN
        
        ; *************************************************
        ; Variable equality         
        ; Since variables and constants/functions are lexically different, we can distinguish them here.
        (equals-formula 
         [(condition-term EQUALS condition-term) (build-so (list 'EQUALS $1 $3) 1 3)])
        
        ; Downcasting
        (isa-formula
         [(<lowercase-id> COLON <capitalized-id>) (build-so (list 'ISA $1 $3) 1 3)])
                
        ; ***********************************************************        
        ; atomic formulas        
        
        ; a predicate in the condition can be
        ; R or pol.R or Pol.Child.R and so on
        (condition-predicate 
         [(<capitalized-id>) (list $1)]
         [(condition-predicate DOT <capitalized-id>) (append $1 (list $3))])
        
        (atomic-formula [(condition-predicate LPAREN condition-term-list RPAREN) 
                         (build-so (list 'ATOMIC-FORMULA $1 (append (list 'TERM-LIST) $3)) 1 4)]
                       ; !!! TODO re-add in 
                        ; [(in-formula)
                       ;  $1]
                        [(equals-formula) 
                         $1]
                        [(isa-formula)
                         $1])   
        
        (atomic-formula-list
         [(atomic-formula) (list $1)]
         [(atomic-formula-list COMMA atomic-formula) (append $1 (list $3))])  
       
        ; !!! todo re-enable vector support
        ; *************************************************
        ; a vector of variables. e.g.     x, y, z
        ; beware of cons -- it will end up giving us dotted pairs here. we want to build a flat list.
        ;(variable-list [(<identifier>) (list (build-so (list 'VARIABLE $1) 1 1))]
        ;               ;[(<identifier> variable-list) (cons $1 $2)]
        ;               [(<identifier> COMMA variable-list) (append (list (list 'VARIABLE $1)) $3 )]
        ;               
        ;               ; For now, only support <polname:req>. 
        ;               ; Problems with <req> only: We want to allow policies to have different req. vectors.
        ;               ; So maybe we could detect which policy IDB <req> is appearing in.
        ;               ;   but it doesn't always have one (saved query, for instance)
        ;               
        ;               ; Allow <>, <> for later (via above production)
        ;               [(LTHAN <identifier> COLON <identifier> GTHAN) 
        ;                (list (build-so (list 'CUSTOM-VECTOR-Y $2 $4) 1 5))]
        ;               [(LTHAN <identifier> COLON <identifier> GTHAN COMMA variable-list)
        ;                (append (list (build-so (list 'CUSTOM-VECTOR-Y $2 $4) 1 5)) $7)]
        ;
        ;               [(LTHAN <identifier> GTHAN) 
        ;                (list (build-so (list 'CUSTOM-VECTOR-N $2 ) 1 3))]
        ;               [(LTHAN <identifier> GTHAN COMMA variable-list)
        ;                (append (list (build-so (list 'CUSTOM-VECTOR-N $2) 1 3)) $5)])
        
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

; *************************************************
; *************************************************
; *************************************************
; Tests

(define (test-parse-s s)
  (define in (open-input-string s))
  (port-count-lines! in)
  ((parse "test param to parse-s") (lambda () (lex in))))

; (parse-s "let F[x, y, z] be P.C.Foo(x, y) and not Bar(z);")
