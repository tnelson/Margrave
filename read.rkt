#lang scheme

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens non-terminals
  (<identifier> <unsigned-integer> <string>
                LOAD-POLICY
                EXPLORE AND OR NOT COLON IMPLIES IFF LPAREN RPAREN EQUALS SHOW ALL ONE IS POSSIBLEQMARK PUBLISH COMMA UNDER TUPLING DEBUG CEILING RENAME INFO COLLAPSE COMPARE IDBOUTPUT POPULATED UNPOPULATED FOR CASES ADD SUBSORT SORT CONSTRAINT DISJOINT NONEMPTY SINGLETON ATMOSTONE PARTIAL FUNCTION TOTAL ABSTRACT SUBSET SET TARGET PREDICATE RULE TO CREATE VOCABULARY DECISION REQUESTVAR OTHERVAR POLICY LEAF RCOMBINE PCOMBINE PREPARE LOAD XACML SQS GET COUNT SIZE RULES HIGHER PRIORITY THAN QUALIFIED NEXT GUARANTEEDQMARK IN	AT CHILD REQUEST VECTOR QUIT DELETE
                EOF))

(define-lex-abbrevs
  [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
  [lex:digit (:/ #\0 #\9)]
  [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)])


;Taken from /collects/algol60/parse.ss
(define stx-for-original-property (read-syntax #f (open-input-string "original")))

(define-syntax (token stx)
  (syntax-case stx ()
    [(_ name val)
     (identifier? (syntax name))
     (let ([name (syntax name)])
       (with-syntax ([token-name (datum->syntax
                                  name
                                  (string->symbol
                                   (format "token-~a" (syntax-e name))))]
                     ["source-name" (datum->syntax name '"source-name")]
                     [start-pos (datum->syntax name 'start-pos)]
                     [end-pos (datum->syntax name 'end-pos)])
         (syntax 
          (token-name 
           (datum->syntax #f val
                                 (list
                                  "source-name"
                                  (position-line start-pos)
                                  (position-col start-pos)
                                  (position-offset start-pos)
                                  (- (position-offset end-pos)
                                     (position-offset start-pos)))
                                 stx-for-original-property)))))]))
(define-syntax (ttoken stx)
  (syntax-case stx ()
    [(_ name)
     (identifier? (syntax name))
     (syntax (token name 'name))]))

(define lex
  (lexer
   [(:+ lex:whitespace) (begin (display "Skipping whitespace") (lex input-port))];(void)]
   [(eof) 'EOF]
   ["EXPLORE" (token-EXPLORE 'EXPLORE)];(ttoken EXPLORE)] 
   ["LOAD POLICY" (token-LOAD-POLICY 'LOAD-POLICY)]
   ;   ["AND" ] 
   ;   ["OR" ] 
   ;   ["NOT" ] 
   ;   ["COLON" ] 
   ;   ["IMPLIES" ] 
   ;   ["IFF" ] 
   ;   ["LPAREN" ] 
   ;   ["RPAREN" ] 
   ;   ["EQUALS" ] 
   ;   ["SHOW" ] 
   ;   ["ALL" ] 
   ;   ["ONE" ] 
   ;   ["IS" ] 
   ;   ["POSSIBLEQMARK" ] 
   ;   ["PUBLISH" ] 
   ;   ["COMMA" ] 
   ;   ["UNDER" ] 
   ;   ["TUPLING" ] 
   ;   ["DEBUG" ] 
   ;   ["CEILING" ] 
   ;   ["RENAME" ] 
   ;   ["INFO" ] 
   ;   ["COLLAPSE" ] 
   ;   ["COMPARE" ] 
   ;   ["IDBOUTPUT" ] 
   ;   ["POPULATED" ] 
   ;   ["UNPOPULATED" ] 
   ;   ["FOR" ] 
   ;   ["CASES" ] 
   ;   ["ADD" ] 
   ;   ["SUBSORT" ] 
   ;   ["SORT" ] 
   ;   ["CONSTRAINT" ] 
   ;   ["DISJOINT" ] 
   ;   ["NONEMPTY" ] 
   ;   ["SINGLETON" ] 
   ;   ["ATMOSTONE" ] 
   ;   ["PARTIAL" ] 
   ;   ["FUNCTION" ] 
   ;   ["TOTAL" ] 
   ;   ["ABSTRACT" ] 
   ;   ["SUBSET" ] 
   ;   ["SET" ] 
   ;   ["TARGET" ] 
   ;   ["PREDICATE" ] 
   ;   ["RULE" ] 
   ;   ["TO" ] 
   ;   ["CREATE" ] 
   ;   ["VOCABULARY" ] 
   ;   ["DECISION" ] 
   ;   ["REQUESTVAR" ] 
   ;   ["OTHERVAR" ] 
       ["POLICY" (token-POLICY 'POLICY)] 
   ;   ["LEAF" ] 
   ;   ["RCOMBINE" ] 
   ;   ["PCOMBINE" ] 
   ;   ["PREPARE" ] 
   ;   ["LOAD" ] 
   ;   ["XACML" ] 
   ;   ["SQS" ] 
   ;   ["GET" ] 
   ;   ["COUNT" ] 
   ;   ["SIZE" ] 
   ;   ["RULES" ] 
   ;   ["HIGHER" ] 
   ;   ["PRIORITY" ] 
   ;   ["THAN" ] 
   ;   ["QUALIFIED" ] 
   ;   ["NEXT" ] 
   ;   ["GUARANTEEDQMARK" ] 
   ;   ["IN" ]	
   ;   ["AT" ] 
   ;   ["CHILD" ] 
   ;   ["REQUEST" ] 
   ;   ["VECTOR" ] 
   ;   ["QUIT" ] 
   ;   ["DELETE" ]
   ;Identifiers
   [(:: lex:letter (:* (:or lex:letter lex:digit))) 
    (token-<identifier> (string->symbol lexeme))]
   ;Can't figure out how to represent strings correctly. The first below is from algol60/parse.ss, the second is my attempt, the third is the temporary hack (start the string with "ss"
   #;[(:: #\` (:* (:~ #\' #\`)) #\') (let ([s lexeme])
                                          (token <string> (substring s 1 (sub1 (string-length s)))))]
   #;[(:: "\"" (:* (:or lex:letter lex:digit)) "\"") (let ([s lexeme])
                                          (token-<string> (substring s 1 (sub1 (string-length s)))))]
   [(:: "ss" (:* (:or lex:letter lex:digit))) (let ([s lexeme])
                                          (token-<string> (substring s 1)))]
   ))


(define parse
  (parser
   (start start)
   (end EOF)
   (tokens non-terminals)
   (error (lambda (a b c) (void)))
   
   ;(precs )
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    
    (exp [(EXPLORE POLICY) "a"]
         [(LOAD-POLICY) "b"]
         [(LOAD-POLICY <string>) (string-append "Loading " $2)]))))

;runs the Lexer/Parser on the input port
(define (evaluate ip)
  (port-count-lines! ip)
  (letrec ((one-line
            (lambda ()
              (let ((result (parse (lambda () (lex ip)))))
                (when result
                  (printf "~a~n" result)
                  (one-line))))))
    (one-line)))

(lex (open-input-string "LOAD POLICY"))
(evaluate (open-input-string "EXPLORE POLICY"))
(evaluate (open-input-string "LOAD POLICY sstestPolicy"))
