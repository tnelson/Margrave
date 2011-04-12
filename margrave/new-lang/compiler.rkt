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

(require margrave/margrave-xml
         margrave/margrave-policy-vocab
         margrave/helpers
         margrave/lexer
         margrave/parser)

(provide parse-and-compile
         evaluate-parse 
         parse-and-compile-read
         parse-and-compile-read-syntax
         parse-and-compile-port
         make-simple-load-func)

; Take a syntax object for a Margrave command. Return a '(lambda ... OR a list of them
; May be a single COMMAND, or a MARGRAVE-SCRIPT with a list of commands.
(define (compile-margrave-syntax syn)
  (let* ([interns (syntax-e syn)])
   ; (printf "CONVERTING: syn=~a interns=~a ~n" syn interns)
    (let* ([first-intern (first interns)] 
           [first-datum (syntax->datum first-intern)])
      (cond 
        
        ; ************************************
        ; No-op. (Used from null production in parser that prevents clogging due to trailing comments/whitespace)
        [(equal? first-datum 'IGNORE)
         '(lambda () (void))]
        
        ; ************************************
        
        ; Single command: compile its contents
        [(equal? first-datum 'COMMAND)
         (let ()
           ;(printf "COMMAND: ~a~n ~a~n~n" (syntax->datum (second interns)) (compile-margrave-syntax (second interns)))
           (compile-margrave-syntax (second interns)))]
        
        ; Multiple commands: compile each command separately and return a list.
        [(equal? first-datum 'MARGRAVE-SCRIPT) (create-script-list (map compile-margrave-syntax (rest interns)))]
        
        ; ************************************
        ; ************************************
        ; ************************************
        [(equal? first-datum 'PARANTHESIZED-EXPRESSION)
         (compile-margrave-syntax (second interns))]
        
        ; third and fourth of the list are func syntax that create vocab, pol respectively
        ; So create an uber-func that does both
        [(equal? first-datum 'LOAD-POLICY)
         (let ([policy-creation-list (evaluate-policy (symbol->string (syntax->datum (second interns)))
                                                      #:syntax (second interns))])
                      
           (define xml-cmds (append (third policy-creation-list)                                    
                                    (fourth policy-creation-list)))
           
           (make-simple-load-func (first policy-creation-list)
                                  (second policy-creation-list)
                                  xml-cmds
                                  (second interns)))]
        
        [(equal? first-datum 'LOAD-IOS)
         `(lambda () (parse-and-load-ios-by-filename ,(symbol->string/safe (syntax->datum (second interns))) 
                                                     #:syntax #',(second interns)))]
        
        [(equal? first-datum 'LOAD-IOS-WITH)
         `(lambda () (parse-and-load-ios-by-filename ,(symbol->string/safe (syntax->datum (second interns)))
                                                     #:prefix ,(symbol->string/safe (syntax->datum (third interns)))
                                                     #:suffix ,(symbol->string/safe (syntax->datum (fourth interns)))
                                                     #:syntax #',(second interns)))]
        
        [(equal? first-datum 'LOAD-MULT-IOS)
         `(lambda () (parse-and-load-multi-ios (list ,@(syntax->datum (second interns)))
                                               ,(syntax->datum (third interns))
                                               #:syntax #',(second interns)))]
        
        [(equal? first-datum 'LOAD-MULT-IOS-WITH)
         `(lambda () (parse-and-load-multi-ios (list ,@(syntax->datum (second interns)))
                                               ,(syntax->datum (third interns))
                                               #:prefix ,(symbol->string/safe (syntax->datum (fourth interns)))
                                               #:suffix ,(symbol->string/safe (syntax->datum (fifth interns)))
                                               #:syntax #',(second interns)))]
        
        
        [(equal? first-datum 'LOAD-XACML)
         `(lambda () (load-xacml-policy ,(resolve-margrave-filename-keyword (syntax->datum (second interns)))
                                        #:syntax #',(second interns)))]
        
        [(equal? first-datum 'LOAD-SQS)
         `(lambda () (load-sqs-policy ,(resolve-margrave-filename-keyword (syntax->datum (second interns)))
                                      #:syntax #',(second interns)))]
        
        
        ; ************************************        
        ; Commands are handled here. Inner syntax is handled by
        ; recursive calls to helper-syn->xml
        
        
        [(equal? first-datum 'DEFVEC)
         (let ()
           
           (define (quote-variables-helper-syn syn)
             (let ([result (helper-syn->xml syn)])
               (if (symbol? result)
                   `',result
                   result)))
           
           `(lambda () (define-custom-vector ',(second interns) (list ,@(map quote-variables-helper-syn
                                                                             (syntax-e (third interns)))))))]
        
        [(equal? first-datum 'CREATE-VOCABULARY)
         (make-single-wrapper `(xml-make-command "CREATE VOCABULARY" ,(map helper-syn->xml (rest interns))) syn)]
        
        [(equal? first-datum 'ADD)
         (make-single-wrapper `(xml-make-command "ADD" ,(map helper-syn->xml (rest interns)))  syn)]
                       
        [(equal? first-datum 'CREATE-POLICY-LEAF)
         (make-single-wrapper `(xml-make-create-policy-leaf-command ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns)))  syn)]
        
        [(equal? first-datum 'IS-POSSIBLE?)
         (make-single-wrapper `(xml-make-is-possible-command ,(if (< 1 (length interns))
                                                                (helper-syn->xml (second interns))
                                                                '(xml-make-id "-1")))  syn)]
        [(equal? first-datum 'COUNT)
         (make-single-wrapper `(xml-make-count-command 
                                ,(if (< 1 (length interns))
                                     (helper-syn->xml (second interns))
                                     '(xml-make-id "-1"))) syn)]
        [(equal? first-datum 'COUNT-WITH-SIZE)
         (make-single-wrapper `(xml-make-count-with-size-command ,(helper-syn->xml (second interns)) ,(helper-syn->xml (third interns)))  syn)]
        
        
        [(equal? first-datum 'EXPLORE)
         ; (printf "Symbol exp: ~a ~a ~a~n" first-intern (second interns) (third interns))
         (make-single-wrapper 
           `(xml-make-explore-command 
             (list ,(helper-syn->xml (second interns)))
             ;List of modifiers could be empty, but use (list instead of '( since we may have funcs to evaluate inside
             (list ,@(if (empty? (rest (rest interns)))
                        empty
                        (map helper-syn->xml (syntax-e (third interns))))))  syn)] 
                
        [(equal? first-datum 'COMPARE)
         (make-single-wrapper 
          `(xml-make-compare-command
            ,(helper-syn->xml (second interns))
            ,(helper-syn->xml (third interns))

            ;List of modifiers could be empty, but use (list instead of '( since we may have funcs to evaluate inside
            (list ,@(if (empty? (rest (rest (rest interns))))
                        empty
                        (map helper-syn->xml (syntax-e (fourth interns)))))) syn)]
        
        ; id, list, optional for-cases list
        [(equal? first-datum 'SHOWREALIZED)
         ;(printf "~a ~n" (syntax->datum (second interns)))
         (if (empty? (syntax->datum (fourth interns)))
             (make-single-wrapper 
              
              `(xml-make-show-realized-command ,(helper-syn->xml (second interns)) 
                                               (list ,@(map helper-syn->xml (syntax-e (third interns)))))  syn)
             (make-single-wrapper 
              `(xml-make-show-realized-command ,(helper-syn->xml (second interns)) 
                                               (list ,@(append (map helper-syn->xml (syntax-e (third interns))) 
                                                               (list `(xml-make-forcases (list ,@(map helper-syn->xml (syntax-e (fourth interns)))))))))  syn))]
        [(equal? first-datum 'SHOWUNREALIZED)
         (if (empty? (syntax->datum (fourth interns)))
             (make-single-wrapper
              `(xml-make-show-unrealized-command ,(helper-syn->xml (second interns)) 
                                                 (list ,@(map helper-syn->xml (syntax-e (third interns)))))  syn)
             (make-single-wrapper
              `(xml-make-show-unrealized-command ,(helper-syn->xml (second interns))
                                                 (list ,@(append (map helper-syn->xml (syntax-e (third interns))) 
                                                                  (list `(xml-make-forcases (list ,(map helper-syn->xml (syntax-e (fourth interns)))))))))  syn))]
        ; same but without the result ID
        [(equal? first-datum 'LSHOWREALIZED)
         
         (if (empty? (syntax->datum (third interns)))
             (make-single-wrapper 
              `(xml-make-show-realized-command (xml-make-id "-1") 
                                               (list ,@(map helper-syn->xml (syntax-e (second interns)))))  syn) 
             (make-single-wrapper 
              `(xml-make-show-realized-command (xml-make-id "-1")
                                               (list ,@(append (map helper-syn->xml (syntax-e (second interns))) 
                                                               (list `(xml-make-forcases (list ,@(map helper-syn->xml (syntax-e (third interns)))))))))  syn))]
        [(equal? first-datum 'LSHOWUNREALIZED)
         (if (empty? (syntax->datum (third interns)))
             (make-single-wrapper 
              `(xml-make-show-unrealized-command (xml-make-id "-1")
                                                 (list ,@(map helper-syn->xml (syntax-e (second interns)))))  syn)
             (make-single-wrapper 
              `(xml-make-show-unrealized-command (xml-make-id "-1")
                                                 (list ,@(append (map helper-syn->xml (syntax-e (second interns))) 
                                                                 (list `(xml-make-forcases (list ,@(map helper-syn->xml (syntax-e (third interns)))))))))  syn))]
        
        [(equal? first-datum 'RENAME)
         (make-single-wrapper
          `(xml-make-rename-command ,(symbol->string (syntax->datum (second interns)))
                                    ,(symbol->string (syntax->datum (third interns))))  syn)]
        
        ; pass (type ONE) to get first
        ;;     (type NEXT) to get next in Java's iterator
        
        [(equal? first-datum 'GET)
         (make-single-wrapper
          `(xml-make-get-command ,(helper-syn->xml (second interns)) 
                                 ;Use -1 if nothing is supplied
                                 ,(if (< 2 (length interns))
                                    (helper-syn->xml (third interns))
                                    '(xml-make-id "-1")))  syn)]
        ; Like GET, only pretty-print the result
        [(equal? first-datum 'SHOW)
         ;         (printf "~a ~a ~n" (second interns) (if (< 2 (length interns)) (third interns) "last" ))
         `(lambda () (pretty-print-response-xml 
                      (send-and-receive-xml
                       (xml-make-get-command ,(helper-syn->xml (second interns)) 
                                             ;Use -1 if nothing is supplied
                                             ,(if (< 2 (length interns))
                                                  (helper-syn->xml (third interns))
                                                  '(xml-make-id "-1"))) #:syntax  #',syn)))]
        
        
       ; ALL gets its own command type:
        [(equal? first-datum 'SHOWALL)
         (make-show-all  
          (if (< 2 (length interns))
              (helper-syn->xml (second interns))
              '(xml-make-id "-1")) syn)]
        
        [(equal? first-datum 'GETALL)
         (make-get-all  
          (if (< 2 (length interns))
              (helper-syn->xml (second interns))
              '(xml-make-id "-1"))  syn)]
        
        [(equal? first-datum 'INFO)
         (if (empty? (rest interns))
             (make-single-wrapper `(xml-make-info-command) syn)
             (make-single-wrapper `(xml-make-info-id-command ,(symbol->string (syntax->datum (second interns)))) syn))]
        
        ; Allow user to get the rules in a policy
        [(equal? first-datum 'GETRULES)
         (make-single-wrapper
          `(xml-make-get-rules-command ',(syntax->datum (second interns)))  syn)]
        
        [(equal? first-datum 'GETQRULES)
         (make-single-wrapper
          `(xml-make-get-qrules-command ',(syntax->datum (second interns)))  syn)]
        
        [(equal? first-datum 'GETRULESDEC)
         (make-single-wrapper 
          `(xml-make-get-rules-command ',(syntax->datum (second interns))
                                       ,(symbol->string (syntax->datum (third interns))))  syn)]
        
        [(equal? first-datum 'GETQRULESDEC)
         (make-single-wrapper 
          `(xml-make-get-qrules-command ',(syntax->datum (second interns))
                                        ,(symbol->string (syntax->datum (third interns))))  syn)]
                
        [(equal? first-datum 'QUIT)
         '(lambda () 
            (stop-margrave-engine)
            (printf "Closing Margrave. Have a nice day.~n" )
            (exit))]
                
        [else
         (printf "UNEXPECTED COMMAND SYMBOL: ~a ~a ~n" first-intern first-datum)]))))


(define (create-script-list list-of-func-syntax)
  ;(printf "CS: ~a~n" list-of-func-syntax)
;  `(list ,@(map (lambda (f) `(,f)) 
;                           list-of-func-syntax)))
         `(list ,@list-of-func-syntax))

; Show-all is based on get-all
(define (make-show-all explore-id syn)
  `(lambda () (let* ([string-buffer (open-output-string)]
                     [the-generator-func ,(make-get-all explore-id syn)]
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
(define (make-get-all explore-id syn)
  `(lambda () 
     (generator ()  
                (let loop-func ([is-first #t])
                  (if (equal? is-first #f)
                      (begin
                        (yield (send-and-receive-xml (xml-make-get-command `(type "NEXT") ,explore-id) #:syntax  #',syn))
                        (loop-func #f))
                      (begin
                        (yield (send-and-receive-xml (xml-make-get-command `(type "ONE") ,explore-id) #:syntax  #',syn))  
                        (loop-func #f)))))))

; This is the *symbol* 'send-and-receive-xml, not the function
; It gets evaluated in a context where we know what the symbol means.
(define (make-single-wrapper thexml-constructor syn)  
  
  ; #',syn here because (INFO) will still read as "evaluate INFO" even if it's a syntax object.
  ; Behavior of #':
  ;(define foo #'14)
  ;(define bar #'`,foo)
  ; (eval bar) results in the correct, original syntax object (pointing to the initial 14.)
  
  `(lambda () (send-and-receive-xml ,thexml-constructor #:syntax #',syn)))

(define (make-simple-load-func polname vocname list-of-xexprs src-syntax)
#| gmarceau personal preference:
  `(lambda () ,@(for/list ([an-xml list-of-xml])
                          `(send-and-receive-xml ,an-xml))))
|#
  ; Using implicit begin; the policy name will be the result of the func
  `(lambda () ,@(append (map (lambda (an-xexpr) 
                               `(send-and-receive-xml ',an-xexpr #:syntax #',src-syntax)) 
                             list-of-xexprs)
                        ; resulting lambda will just return polname
                        (list polname))))

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
        [(equal? first-datum 'CUSTOM-VECTOR-Y)
         `(resolve-custom-vector-y ',(syntax->datum (second interns)) ',(syntax->datum (third interns)) #',syn)]        
        ; <vecname>
        [(equal? first-datum 'CUSTOM-VECTOR-N)
         `(resolve-custom-vector-n ',(syntax->datum (second interns)) #',syn)]        
        

        
        [(equal? first-datum 'VARIABLE-VECTOR)
         ;(printf "Symbol varvec: ~a~n" first-intern)
         ; flatten, not append because some map results are lists, others not
         `(xml-make-identifiers-list (flatten (list ,@(map helper-syn->xml (rest interns)))))]
                
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

        
        [(equal? first-datum 'TRUE-CONDITION)
         '(xml-make-true-condition)]
        
        [(equal? first-datum 'TUPLING)
         `(xml-make-tupling)]
        [(equal? first-datum 'CEILING)
         `(xml-make-ceiling ,(if (< 1 (length interns))
                                   (syntax->string (second interns))
                                   (xml-make-id "-1")))]
        [(equal? first-datum 'DEBUG)
         `(xml-make-debug ,(syntax->string (second interns)))]
        [(equal? first-datum 'UNDER)
         `(xml-make-under (list ,@(map helper-syn->xml (syntax-e (second interns)))))]
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
        [(equal? first-datum 'INCLUDE)
         ; List instead of quote since there's evaluation to be done inside
         `(xml-make-include (list ,@(map helper-syn->xml (rest interns))))]
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
