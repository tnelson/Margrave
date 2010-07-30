#lang racket

(require racket syntax/stx parser-tools/yacc parser-tools/lex (prefix-in : parser-tools/lex-sre) "margrave-xml.rkt"
         syntax/readerr)

(provide
 port->xml
 evalxml
 evaluate)


; *************************************************
; define-empty-tokens defines the tokens that take no value
; define-tokens defines the tokens that can contain a value
(define-empty-tokens empty-terminals
  (             EXPLORE AND OR NOT COLON IMPLIES IFF LPAREN RPAREN EQUALS SHOW
                        ALL ONE IS POSSIBLEQMARK PUBLISH COMMA UNDER TUPLING DEBUG
                        CEILING RENAME INFO COLLAPSE COMPARE IDBOUTPUT POPULATED
                        UNPOPULATED FOR CASES ADD SUBSORT SORT CONSTRAINT DISJOINT
                        NONEMPTY SINGLETON ATMOSTONE PARTIAL FUNCTION TOTAL ABSTRACT
                        SUBSET SET TARGET PREDICATE RULE TO CREATE VOCABULARY DECISION
                        REQUESTVAR OTHERVAR POLICY LEAF RCOMBINE PCOMBINE PREPARE LOAD
                        XACML SQS GET COUNT SIZE RULES HIGHER PRIORITY THAN QUALIFIED
                        NEXT GUARANTEEDQMARK IN	AT CHILD REQUEST VECTOR QUIT DELETE SEMICOLON
                        EOF))
(define-tokens terminals (<identifier> <unsigned-integer>))



(define-lex-abbrevs
  [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)])





; *************************************************
; Produce a lexer function
; Assume the caller has downcased the input
(define lex
  (lexer-src-pos
   [(:+ lex:whitespace) (begin 
                          ;(printf "Skipping whitespace~n") 
                          (return-without-pos (lex input-port)))]
   [(eof) 'EOF]
   ["explore" (token-EXPLORE)]
   ["load" (token-LOAD)]
   ["policy" (token-POLICY)] 
   ["and" (token-AND)] 
   ["or" (token-OR)] 
   ["not" (token-NOT)] 
   [":" (token-COLON)] 
   [";" (token-SEMICOLON)] 
   ["implies" (token-IMPLIES)] 
   ["iff" (token-IFF)] 
   ["(" (token-LPAREN)] 
   [")" (token-RPAREN)] 
   ["=" (token-EQUALS)] 
   ["show" (token-SHOW)] 
   ["all" (token-ALL)] 
   ["one" (token-ONE)] 
   ["is" (token-IS)]   
   ["possible?" (token-POSSIBLEQMARK)] 
   ["publish" (token-PUBLISH)] 
   ["," (token-COMMA)] 
   ["under" (token-UNDER)] 
   ["tupling" (token-TUPLING)] 
   ["debug" (token-DEBUG)] 
   ["ceiling" (token-CEILING)] 
   ["rename" (token-RENAME)] 
   ["info" (token-INFO)] 
   ["collapse" (token-COLLAPSE)] 
   ["compare" (token-COMPARE)] 
   ["idboutput" (token-IDBOUTPUT)] 
   ["populated" (token-POPULATED)] 
   ["unpopulated" (token-UNPOPULATED)] 
   ["for" (token-FOR)] 
   ["cases" (token-CASES)] 
   ["add" (token-ADD)] 
   ["subsort" (token-SUBSORT)] 
   ["sort" (token-SORT)] 
   ["constraint" (token-CONSTRAINT)] 
   ["disjoint" (token-DISJOINT) ] 
   ["nonempty" (token-NONEMPTY)] 
   ["singleton" (token-SINGLETON)] 
   ["atmostone" (token-ATMOSTONE)] 
   ["partial" (token-PARTIAL)] 
   ["function" (token-FUNCTION)] 
   ["total" (token-TOTAL)] 
   ["abstract" (token-ABSTRACT)] 
   ["subset" (token-SUBSET)] 
   ["set" (token-SET)] 
   ["target" (token-TARGET)] 
   ["predicate" (token-PREDICATE)] 
   ["rule" (token-RULE)] 
   ["to" (token-TO)] 
   ["create" (token-CREATE)] 
   ["vocabulary" (token-VOCABULARY)] 
   ["decision" (token-DECISION)] 
   ["requestvar" (token-REQUESTVAR)] 
   ["othervar" (token-OTHERVAR)] 
   ["leaf" (token-LEAF)] 
   ["rcombine" (token-RCOMBINE)] 
   ["pcombine" (token-PCOMBINE)] 
   ["prepare" (token-PREPARE)] 
   ["load" (token-LOAD)] 
   ["xacml" (token-XACML)] 
   ["sqs" (token-SQS)] 
   ["get" (token-GET)] 
   ["count" (token-COUNT)] 
   ["size" (token-SIZE)] 
   ["rules" (token-RULES)] 
   ["higher" (token-HIGHER)] 
   ["priority" (token-PRIORITY)] 
   ["than" (token-THAN)] 
   ["qualified" (token-QUALIFIED)] 
   ["next" (token-NEXT)] 
   ["guaranteed?" (token-GUARANTEEDQMARK)] 
   ["in" (token-IN)]	
   ["at" (token-AT)] 
   ["child" (token-CHILD)] 
   ["request" (token-REQUEST)] 
   ["vector" (token-VECTOR)] 
   ["quit" (token-QUIT)] 
   ["delete" (token-DELETE)]
   
   ; Natural nums
   [(:: lex:digit (:* lex:digit)) 
    (token-<unsigned-integer> (string->symbol lexeme))]
   
   ; Un-quoted Identifiers -- everything but whitespace and: ( ) \" , = :
   ; Use ----> char-complement <----, not complement.
   [(:: (:+ (char-complement (:or lex:whitespace "\"" "(" ")" "," "=" ":" ";"))))
    (token-<identifier> (string->symbol lexeme))]
   
   ; Quoted Identifiers -- anything but quote or whitespace wrapped in quotes
   ; strip the quotes when returning the identifier value
   ;; !!!TODO!!! Why disallow whitespace in quotes? XACML likes sort names with spaces in them.
   [(:: "\"" (:+ (char-complement (:or lex:whitespace "\""))) "\"") 
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
   (end EOF)
   (tokens empty-terminals terminals)
;   (error (lambda (a name val start end)
;              (raise-read-error 
;               "read-error"
;               source-name
;               (position-line start)
;               (position-col start)
;               (position-offset start)
;               (- (position-offset end)
;                  (position-offset start)))))
   
   (error (lambda (tok-ok? token-name token-value start-pos end-pos) 
            (if (equal? tok-ok? #f)
                (printf "Invalid lexical token at: ~a~n" token-value)
                (printf "Parser error on token: ~a~n" token-value))
            (printf "Start: line ~a column ~a offset ~a End: line ~a column ~a offset ~a~n"
                    (position-line start-pos) (position-col start-pos) (position-offset start-pos)
                    (position-line end-pos) (position-col end-pos) (position-offset end-pos))))
   
   ; Order of precedence: negation > conjunction > disjunction > implication > bi-implication
   ; Implication is not associative (and of course, neither is the unary operator NOT.)
   (precs (left IFF)
          (nonassoc IMPLIES)
          (left OR)
          (left AND)
          (nonassoc NOT))
   
   (grammar
    
    ;**************************************************
    ; Streams of tokens are either empty, an error, or a valid margrave-command.
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(margrave-script) (build-so (append (list 'MARGRAVE-SCRIPT) $1) 1 1)]) ;A margrave-script is a list of margrave-commands, separated by semicolons
    
    ;**************************************************
    ; One production for each kind of command
    
    (margrave-script
     [(margrave-command SEMICOLON) (list 'COMMAND $1)]
     [(margrave-command SEMICOLON margrave-script) (append (list (list 'COMMAND $1) $3))])
    
    #;(variable-list [(<identifier>) (list (build-so (list 'VARIABLE $1) 1 1))]
                   ;[(<identifier> variable-list) (cons $1 $2)]
                   [(<identifier> COMMA variable-list) (append (list (list 'VARIABLE $1)) $3 )])
    
    (margrave-command 
     [(explore-statement) $1]
     
     ;         [(LOAD-POLICY) "b"]
     ;[(LOAD exp) (build-so (list 'LOAD $2) 1 2)]
     
     [(RENAME <identifier> <identifier>) (build-so (list 'RENAME $2 $3) 1 3)]
     [(GET get-type numeric-id) (build-so (list 'GET $2 $3) 1 3)]
     [(SHOW get-type numeric-id) (build-so (list 'SHOW $2 $3) 1 3)]
     
     ; SHOW POPULATED and friends
     [(SHOW POPULATED numeric-id atomic-formula-list) 
      (build-so (list 'SHOWPOPULATED $3 $4 empty) 1 4)]
     [(SHOW UNPOPULATED numeric-id atomic-formula-list)
      (build-so (list 'SHOWUNPOPULATED $3 $4 empty) 1 4)]
     [(SHOW POPULATED numeric-id atomic-formula-list FOR CASES atomic-formula-list)
      (build-so (list 'SHOWPOPULATED $3 $4 $7) 1 7)]
     [(SHOW UNPOPULATED numeric-id atomic-formula-list FOR CASES atomic-formula-list)
      (build-so (list 'SHOWUNPOPULATED $3 $4 $7) 1 7)]     
     
     ; Get information
     [(INFO) (build-so (list 'INFO) 1 1)]
     [(INFO <identifier>) (build-so (list 'INFO $2) 1 2)]
     
     ; Close out the engine
     [(QUIT) (build-so (list 'QUIT) 1 1)]                      
     )
    
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
     [(NEXT) (build-so (list 'type 'NEXT) 1 1)])
    
    ;**************************************************
    ; Optional modifiers for the explore statement
    
    (explore-modifier
     [(UNDER policy) (build-so (list 'UNDER $2) 1 2)]
     [(PUBLISH LPAREN variable-list RPAREN) (build-so (list 'PUBLISH (append (list 'VARIABLE-VECTOR) $3)) 1 2)]
     
     ; (IDBOUTPUT fmla fmla...)
     [(IDBOUTPUT atomic-formula-list) (build-so (append (list 'IDBOUTPUT) $2) 1 2)]
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
    
    
    ; *************************************************
    ; condition-formula: A sub-formula of the condition
    (condition-formula 
     
     
     [(condition-formula OR condition-formula) (build-so (list 'OR $1 $3) 1 1)]
     [(condition-formula AND condition-formula) (build-so (list 'AND $1 $3) 1 1)]
     [(condition-formula IMPLIES condition-formula) (build-so (list 'IMPLIES $1 $3) 1 1)]
     [(condition-formula IFF condition-formula) (build-so (list 'IFF $1 $3) 1 1)]
     [(NOT condition-formula) (build-so (list 'NOT $2) 1 1)]
     [(LPAREN condition-formula RPAREN) (build-so $2 1 1)]
     [(atomic-formula) (build-so $1 1 1)]
     
     ;[(relation) $1]
     ;[(<identifier> COLON relation) "a"]
     )
    
    ; *************************************************
    ; represents a top-level condition, the fully-developed formula in EXPLORE 
    (condition [(condition-formula) (list 'CONDITION $1)])
    
    ; *************************************************
    ; An atomic formula can be either of the form
    ; R(x, y, ...)   or
    ; Policyname:R(x, y, ...)   
    
    (atomic-formula [(<identifier> LPAREN variable-list RPAREN) 
                     (build-so (list 'ATOMIC-FORMULA-N $1 (append (list 'VARIABLE-VECTOR) $3)) 1 4)]
                    [(<identifier> COLON <identifier> LPAREN variable-list RPAREN) 
                     (build-so (list 'ATOMIC-FORMULA-Y $1 ":" $3 (append (list 'VARIABLE-VECTOR) $5)) 1 5)])
    
    ; ***********************************************************
    ; Used by IDBOUTPUT, SHOW POPULATED, SHOW UNPOPULATED
    ; May be a normal atomic formula. May also have an empty vector (no parens).
    
    (poss-nullary-atomic-formula [(<identifier> COLON <identifier>)
                                  (build-so (list 'EMPTY-ATOMIC-FORMULA-Y $1 ":" $3 empty) 1 3)]
                                 [(<identifier>)
                                  (build-so (list 'EMPTY-ATOMIC-FORMULA-N $1 empty) 1 1)]
                                 [(atomic-formula) 
                                  $1])
    
    (atomic-formula-list
     [(poss-nullary-atomic-formula) (list $1)]
     [(poss-nullary-atomic-formula COMMA atomic-formula-list) (append (list $1) $3)])
    
    
    ;    (atom [(<identifier>) (build-so (list 'VARIABLE $1) 1 1)])
    
    ; *************************************************
    ; a vector of variables. e.g.     x, y, z
    ; beware of cons -- it will end up giving us dotted pairs here. we want to build a flat list.
    (variable-list [(<identifier>) (list (build-so (list 'VARIABLE $1) 1 1))]
                   ;[(<identifier> variable-list) (cons $1 $2)]
                   [(<identifier> COMMA variable-list) (append (list (list 'VARIABLE $1)) $3 )]))))

;(define (e stx)
;  (syntax-e stx))

;Returns a list of xml documents
(define (helper-syn->xml syn)
  (let* ([interns (syntax-e syn)])
    ;(printf "CONVERTING: ~a ~n" interns)
    (let* ([first-intern (first interns)]
           [first-datum (syntax->datum first-intern)])
      (cond [(equal? first-datum 'COMMAND) (helper-syn->xml (second interns))]
            [(equal? first-datum 'VARIABLE) ;Will be returned to variable vector
             ;(printf "Symbol var: ~a~n" first-intern)
             (symbol->string (syntax->datum (second interns)))
             #;(append (list 'VARIABLE) (list (list (list 'name (symbol->string (syntax->datum (second interns)))))))]
            
            [(equal? first-datum 'VARIABLE-VECTOR)
             ;(printf "Symbol varvec: ~a~n" first-intern)
             (xml-make-identifiers-list (begin (map helper-syn->xml (rest interns))))
             #;(append (list 'VARIABLE-VECTOR) (map helper-syn->xml (rest interns)))]
            
            [(equal? first-datum 'ATOMIC-FORMULA-N)
             ;(printf "Symbol atn: ~a~n" first-intern)
             (begin
               ;(display (xml-make-atomic-formula-n (symbol->string (syntax->datum (second interns))) (helper-syn->xml (third interns))))
               (xml-make-atomic-formula-n (symbol->string (syntax->datum (second interns))) (helper-syn->xml (third interns))))]
            
            [(equal? first-datum 'ATOMIC-FORMULA-Y)
             ;(printf "Symbol aty: ~a~n" first-intern)
             ;Third is the colon
             (xml-make-atomic-formula-y (symbol->string (syntax->datum (second interns))) (symbol->string (syntax->datum (fourth interns))) (helper-syn->xml (fifth interns)))]
            
            [(equal? first-datum 'EMPTY-ATOMIC-FORMULA-N)
             ; (printf "Symbol empty atn: ~a~n" first-intern)
             (begin
               ;(display (xml-make-atomic-formula-n (symbol->string (syntax->datum (second interns))) (helper-syn->xml (third interns))))
               (xml-make-atomic-formula-n (symbol->string (syntax->datum (second interns))) empty))]
            
            [(equal? first-datum 'EMPTY-ATOMIC-FORMULA-Y)
             ;(printf "Symbol empty aty: ~a~n ~a ~a ~n" first-intern (second interns) (fourth interns))
             ;Third is the colon
             (xml-make-atomic-formula-y (symbol->string (syntax->datum (second interns))) (symbol->string (syntax->datum (fourth interns))) empty)]
            
            
            
            [(equal? first-datum 'CONDITION)
             ;(printf "Symbol cond: ~a~n" first-intern)
             (begin
               (helper-syn->xml (second interns)))]
            ;(append (list 'CONDITION) (map helper-syn->xml (rest interns)))]
            [(equal? first-datum 'EXPLORE)
             ; (printf "Symbol exp: ~a ~a ~a~n" first-intern (second interns) (third interns))
             (begin
               (xml-make-explore-command (list (helper-syn->xml (second interns)))
                                         ;List of modifiers could be empty
                                         (if (empty? (rest (rest interns)))
                                             empty
                                             (map helper-syn->xml (syntax-e (third interns))))))] ;(syntax-e (third interns)) is the list of modifiers
            [(equal? first-datum 'TUPLING)
             (xml-make-tupling)]
            [(equal? first-datum 'CEILING)
             (xml-make-ceiling (syntax->string (second interns)))]
            [(equal? first-datum 'DEBUG)
             (xml-make-debug (syntax->string (second interns)))]
            [(equal? first-datum 'UNDER)
             (xml-make-under (helper-syn->xml (second interns)))]
            [(equal? first-datum 'POLICY)
             (xml-make-policy-identifier (syntax->string (second interns)))]
            [(equal? first-datum 'PUBLISH)
             (xml-make-publish (helper-syn->xml (second interns)))]
            ;(append (list 'EXPLORE) (map helper-syn->xml (rest interns)))]
            [(equal? first-datum 'AND)
             ;(printf "Symbol and: ~a~n" first-intern)
             (begin
               (list 'AND (helper-syn->xml (second interns)) (helper-syn->xml (third interns))))]
            ;(append (list 'AND) (map helper-syn->xml (rest interns)))]
            
            ; id, list, optional for-cases list
            [(equal? first-datum 'SHOWPOPULATED)
             (printf "~a ~n" (syntax->datum (second interns)))
             (if (empty? (fourth interns))
                 (xml-make-show-populated-command (helper-syn->xml (second interns)) 
                                                  (map helper-syn->xml (syntax-e (third interns))))
                 (xml-make-show-populated-command (helper-syn->xml (second interns)) 
                                                  (append (map helper-syn->xml (syntax-e (third interns))) 
                                                          (list (xml-make-forcases (map helper-syn->xml (syntax-e (fourth interns))))))))]
            [(equal? first-datum 'SHOWUNPOPULATED)
             (if (empty? (fourth interns))
                 (xml-make-show-unpopulated-command (helper-syn->xml (second interns)) 
                                                    (map helper-syn->xml (syntax-e (third interns))))
                 (xml-make-show-unpopulated-command (helper-syn->xml (second interns))
                                                    (append (map helper-syn->xml (syntax-e (third interns))) 
                                                            (list (xml-make-forcases (map helper-syn->xml (syntax-e (fourth interns))))))))]
            
            [(equal? first-datum 'OR)
             (list 'OR (helper-syn->xml (second interns)) (helper-syn->xml (third interns)))]
            [(equal? first-datum 'IMPLIES)
             (list 'IMPLIES (helper-syn->xml (second interns)) (helper-syn->xml (third interns)))]
            [(equal? first-datum 'IFF)
             (list 'AIFF (helper-syn->xml (second interns)) (helper-syn->xml (third interns)))]
            [(equal? first-datum 'NOT)
             (list 'NOT (helper-syn->xml (second interns)) (helper-syn->xml (third interns)))]
            [(equal? first-datum 'RENAME)
             (xml-make-rename-command (symbol->string (syntax->datum (second interns)))
                                      (symbol->string (syntax->datum (third interns))))]
            [(equal? first-datum 'GET)
             (xml-make-get-command (helper-syn->xml (second interns)) (helper-syn->xml (third interns)))]
            [(equal? first-datum 'type)
             (xml-make-type (syntax->string (second interns)))]
            [(equal? first-datum 'id)
             (xml-make-id (syntax->string (second interns)))]
            [(equal? first-datum 'INFO)
             (if (empty? (rest interns))
                 (xml-make-info-command)
                 (xml-make-info-id-command (symbol->string (syntax->datum (second interns)))))]
            [(equal? first-datum 'IDBOUTPUT)
             (xml-make-idbout (map helper-syn->xml (rest interns)))]
            [(equal? first-datum 'QUIT)
             (xml-make-quit)]
            [else
             (printf "UNEXPECTED SYMBOL: ~a ~a ~n" first-intern first-datum)]))))

(define (syntax->string s)
  (symbol->string (syntax->datum s)))

(define (syntax->xml syn)
  (map helper-syn->xml (rest (syntax-e syn))))

#;(make-document
   (make-prolog empty #f empty)
   (string->xml helper-result)
   empty)

#;(define (rs sn ip)
    (port-count-lines! ip)
    ((parse sn) (lambda() (lex ip)))) 


; These functions enforce case-insensitivity by downcasing the input string before lexing.
(define (evaluate sn s)
  (let ((in (open-input-string (string-downcase s))))
    ((parse sn) (lambda() (lex in)))))

(define (evalxml s)
  (let ((in (open-input-string (string-downcase s))))
    (syntax->xml ((parse "source") (λ() (lex in))))))

(define (port->xml in-port)
  (syntax->xml ((parse "source") (λ() (lex in-port)))))
