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

(require 
 xml 
 "margrave-policy-vocab.rkt"
 "helpers.rkt"
 "lexer.rkt"
 "parser.rkt"
 "margrave-xml.rkt")

(provide parse-and-compile
         evaluate-parse 
         parse-and-compile-read
         parse-and-compile-read-syntax
         parse-and-compile-port         
         create-policy-loader)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-policy-loader policy-id policy-fn src-syntax)  

  ; Prevent re-binding of a policy identifier
  (when (hash-has-key? cached-policies policy-id)
    (raise-user-error (format "Error: The policy identifier ~v has already been used.~n" policy-id)))

  (define policy-instance (evaluate-policy policy-fn
                                           policy-id
                                           #:syntax src-syntax))
  
  
  (define theory-instance (m-policy-theory policy-instance))
  (define vocab-instance (m-theory-vocab theory-instance))
  (define thy-key (m-theory->key theory-instance))
  
  ; If we already loaded the theory, don't re-send the vocab/axiom commands.
  ; Also cache the theory if this is the first time we've seen it.
  ; Use the PATH as the key, because may have multiple theories of the same "name"
  (define vocab-and-axioms-xml    
    (if (hash-has-key? cached-theories thy-key)
        empty
        (begin
          (hash-set! cached-theories
                     thy-key
                     theory-instance)
          (m-theory->xexprs theory-instance))))
    
  ;; Cache the policy
  (hash-set! cached-policies 
             policy-id
             policy-instance)
  
  (define xml-cmds (append vocab-and-axioms-xml
                           (m-policy->xexprs policy-instance)))
  
  ; Load the policy, but also bind the result in our environment
  (make-simple-load-func policy-id                                                     
                         xml-cmds                            
                         src-syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Take a syntax object for a Margrave command. 
; Return a '(lambda ...)  

;; TODO: this should be rewritten more cleanly, probably using match.

(define (compile-margrave-syntax syn)
  (define interns (syntax-e syn))
  
  ; (printf "CONVERTING: syn=~a interns=~a ~n" syn interns)
  
  (define first-intern (first interns))
  (define first-datum (syntax->datum first-intern))
  
  (cond 
    
    ; ************************************
    ; No-op. (Used from null production in parser that prevents
    ; clogging due to trailing comments/whitespace)
    [(equal? first-datum 'IGNORE)
     '(lambda () (void))]
        
    ; ************************************
    ; ************************************
    ; ************************************
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    [(equal? first-datum 'LOAD-POLICY)
     (define policy-id (symbol->string (syntax->datum (second interns))))
     (define policy-file-name-syntax (third interns))
     
     (create-policy-loader policy-id
                           (syntax->string policy-file-name-syntax)
                           policy-file-name-syntax)]

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    [(equal? first-datum 'LOAD-IOS)
     `(lambda () (parse-and-load-ios-by-filename ,(->string (syntax->datum (second interns))) 
                                                 #:syntax #',(second interns)))]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    [(equal? first-datum 'LOAD-IOS-WITH)
     `(lambda () (parse-and-load-ios-by-filename ,(->string (syntax->datum (second interns)))
                                                 #:prefix ,(->string (syntax->datum (third interns)))
                                                 #:suffix ,(->string (syntax->datum (fourth interns)))
                                                 #:syntax #',(second interns)))]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'LOAD-MULT-IOS)
     `(lambda () (parse-and-load-multi-ios (list ,@(syntax->datum (second interns)))
                                           ,(syntax->datum (third interns))
                                           #:syntax #',(second interns)))]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'LOAD-MULT-IOS-WITH)
     `(lambda () (parse-and-load-multi-ios (list ,@(syntax->datum (second interns)))
                                           ,(syntax->datum (third interns))
                                           #:prefix ,(->string (syntax->datum (fourth interns)))
                                           #:suffix ,(->string (syntax->datum (fifth interns)))
                                           #:syntax #',(second interns)))]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'LOAD-XACML)
     (define policy-id (syntax->datum (second interns)))
     (define policy-file-name-syntax (third interns))
     `(lambda () (load-xacml-policy policy-id
                                    ,(resolve-margrave-filename-keyword (syntax->datum policy-file-name-syntax))
                                    #:syntax #',policy-file-name-syntax))]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'LOAD-SQS)
     (define policy-id (syntax->datum (second interns)))
     (define policy-file-name-syntax (third interns))     
     `(lambda () (load-sqs-policy policy-id
                                  ,(resolve-margrave-filename-keyword (syntax->datum policy-file-name-syntax))
                                  #:syntax #',policy-file-name-syntax))]
    
    
    ; ************************************        
    ; Commands are handled here. Inner syntax is handled by
    ; recursive calls to helper-syn->xml
    
    
    ;        [(equal? first-datum 'DEFVEC)
    ;         (let ()
    ;           
    ;           (define (quote-variables-helper-syn syn)
    ;             (let ([result (helper-syn->xml syn)])
    ;               (if (symbol? result)
    ;                   `',result
    ;                   result)))
    ;           
    ;           `(lambda () (define-custom-vector ',(second interns) (list ,@(map quote-variables-helper-syn
    ;                                                                             (syntax-e (third interns)))))))]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'IS-POSSIBLE?)
     (define fmla-id (if (< 1 (length interns))
                         (symbol->string (syntax->datum (second interns)))
                         ""))
     (make-single-wrapper `(xml-make-is-possible-command  ,fmla-id) syn)]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'COUNT)
     (define fmla-id (if (< 1 (length interns))
                         (symbol->string (syntax->datum (second interns)))
                         ""))
     (make-single-wrapper `(xml-make-count-command ,fmla-id) syn)]
            
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'EXPLORE)
     ; (printf "Symbol exp: ~a ~a ~a~n" first-intern (second interns) (third interns))
     (define query-id (symbol->string (syntax->datum (second interns))))
     (define free-var-vector (helper-syn->xml (third interns)))
     (define query-condition (helper-syn->xml (fourth interns))) 
     (define query-options (map helper-syn->xml (syntax-e (fifth interns))))
      
     ; Don't allow re-binding of query identifiers
     (when (hash-has-key? cached-prior-queries query-id)
       (raise-user-error (format "Unable to create query. The query name ~v is already in use." query-id)))
      
     ; TODO well-sortedness check for these queries. Need to translate to m-formula struct and use m-let's approach
     ;(hash-set! prior-query-vocabs query-id (m-prior-query query-id uber-vocab (hash qryid idb-arity)))
     
     
    ; (printf "Fmla binding:~a~n~n~a~n~a~n~a~n~a~n~n" interns query-id free-var-vector query-condition query-options)
     
     (make-single-wrapper 
      `(xml-make-explore-command 
        ,query-id
        ,free-var-vector
        (list ,query-condition)
        ;List of modifiers could be empty, but use (list instead of '( since we may have funcs to evaluate inside
        ,(if (empty? query-options)
             'empty
             `(list ,@query-options)))  syn)] 
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
   ; [(equal? first-datum 'COMPARE)
   ;  ; Sugar for a LET that does change-impact
   ;  ; Produce xexpr for the let, not special compare xexpr.
   ;  (define query-id (second interns))
   ;  (define pol-id-1 (third interns))
   ;  (define pol-id-2 (fourth interns))
   ;  
   ;  (make-single-wrapper 
   ;   `(xml-make-compare-command
   ;     ,(helper-syn->xml (second interns))
   ;     ,(helper-syn->xml (third interns))
   ;     
   ;     ;            ;List of modifiers could be empty, but use (list instead of '( since we may have funcs to evaluate inside
   ;     (list ,@(if (empty? (rest (rest (rest interns))))
   ;                 empty
   ;                 (map helper-syn->xml (syntax-e (fourth interns)))))) syn)]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    ; id, list, optional for-cases list
    [(equal? first-datum 'SHOWREALIZED)     
     ;(printf "~a~n" interns)
     (define query-id (symbol->string (syntax->datum (second interns))))     
     ; candidates and cases come as LISTS of atomic-fmlas
     (define candidates (map helper-syn->xml (syntax-e (third interns))))
     (define cases (map helper-syn->xml (syntax-e (fourth interns))))         
     (make-single-wrapper       
      `(xml-make-show-realized-command ,query-id
                                       ,(if (empty? cases)
                                            `(list ,@candidates)
                                            `(list ,@(append candidates 
                                                             (list `(xml-make-forcases (list ,@cases)))))))  syn)]  
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
    [(equal? first-datum 'SHOWUNREALIZED)     
     ;(printf "~a~n" interns)
     (define query-id (symbol->string (syntax->datum (second interns))))     
     ; candidates and cases come as LISTS of atomic-fmlas
     (define candidates (map helper-syn->xml (syntax-e (third interns))))
     (define cases (map helper-syn->xml (syntax-e (fourth interns))))         
     (make-single-wrapper       
      `(xml-make-show-unrealized-command ,query-id
                                         ,(if (empty? cases)
                                              `(list ,@candidates)
                                              `(list ,@(append candidates 
                                                               (list `(xml-make-forcases (list ,@cases)))))))  syn)]  
    
    ; pass (type ONE) to get first
    ;;     (type NEXT) to get next in Java's iterator
    
    [(equal? first-datum 'SHOW)
     (define query-id (symbol->string (syntax->datum (second interns))))
     (define options (map helper-syn->xml (syntax-e (third interns))))
     
     `(lambda () (response->string 
                  (send-and-receive-xml
                   (xml-make-get-command (xml-make-type "NEXT") 
                                         ,query-id
                                         (list ,@options)) #:syntax  #',syn)))]
    
    [(equal? first-datum 'RESET)
     (define query-id (symbol->string (syntax->datum (second interns))))
     (make-single-wrapper `(xml-make-reset-command ,query-id) syn)]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    ; ALL is treated specially:
    [(equal? first-datum 'SHOWALL)
     (define query-id (symbol->string (syntax->datum (second interns))))
     (define options (map helper-syn->xml (syntax-e (third interns))))
     (make-show-all query-id options syn)]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    [(equal? first-datum 'INFO)
     (if (empty? (rest interns))
         (make-single-wrapper `(xml-make-info-command) syn)
         (make-single-wrapper `(xml-make-info-id-command ,(symbol->string (syntax->datum (second interns)))) syn))]
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    [(equal? first-datum 'QUIT)
     '(lambda () 
        (stop-margrave-engine)
        (printf "Closing Margrave. Have a nice day.~n" )
        (exit))]
    
    [else
     (printf "UNEXPECTED COMMAND SYMBOL: ~a ~a ~n" first-intern first-datum)]))


(define (create-script-list list-of-func-syntax)
  ;(printf "CS: ~a~n" list-of-func-syntax)
  ;  `(list ,@(map (lambda (f) `(,f)) 
  ;                           list-of-func-syntax)))
  `(list ,@list-of-func-syntax))

; Show-all is based on get-all
(define (make-show-all explore-id options syn)
  `(lambda () (let* ([string-buffer (open-output-string)]
                     [the-generator-func ,(make-get-all explore-id options syn)]
                     [the-generator (the-generator-func)])
                
                ; iterate over results of the generator.
                ; make sure to STOP at unsat.  
                (letrec ([helper-func (lambda () 
                                        (let ([response (the-generator)]) 
                                          (when (not (response-is-unsat? response))
                                            (write-string (response->string response) string-buffer) 
                                            (write-string "\n" string-buffer)                                                 
                                            (helper-func))))])
                  (helper-func)
                  (let ([result (get-output-string string-buffer)])
                    (if (equal? result "")
                        "---> NO RESULTS <---\n"
                        result))))))

; GET ALL returns a generator for all the models
; using 2nd form of let and being tail-recursive
(define (make-get-all explore-id options syn)
  `(lambda () 
     (generator ()  
                (let loop-func ([is-first #t])
                  (if (equal? is-first #f)
                      (begin
                        (yield (send-and-receive-xml (xml-make-get-command `(type "NEXT") ,explore-id (list ,@options) ) #:syntax  #',syn))
                        (loop-func #f))
                      (begin
                        (yield (begin
                                 (send-and-receive-xml (xml-make-reset-command ,explore-id) #:syntax  #',syn)
                                 (send-and-receive-xml (xml-make-get-command `(type "NEXT") ,explore-id (list ,@options)) #:syntax  #',syn)))  
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

(define (make-simple-load-func polname list-of-xexprs src-syntax)    
  `(lambda () ,@(append                  
                 ; (1) xml for each command
                 (for/list ([an-xml list-of-xexprs])
                   `(send-and-receive-xml ',an-xml #:syntax #',src-syntax))
                 
                 ; Environment change happened in _the caller_: create-policy-loader
                 
                 ; (3) resulting lambda will just return polname. Give something nicer.
                 (list (string-append "Policy " polname " loaded.\n")))))

(define (helper-syn->xml syn)
  (define interns (syntax-e syn))
  
  ; (printf "CONVERTING: syn=~a interns=~a ~n" syn interns)
  
  (define first-intern (first interns)) 
  (define first-datum (syntax->datum first-intern))
  
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
    ; (printf "varvec: ~a~n" (rest interns))
     ; flatten, not append because some map results are lists, others not
     `(xml-make-identifiers-list (flatten (list ,@(map helper-syn->xml
                                                       (rest interns)))))]        
    
    [(equal? first-datum 'VARIABLE-DECL)
     ; strip the VARIABLE-TERM label w/ second
     (define the-var-str (symbol->string (second (syntax->datum (second interns)))))
     (define the-sort (syntax->string (third interns)))
     `(xml-make-variable-declaration ,the-var-str ,the-sort)]
    
    [(equal? first-datum 'ATOMIC-FORMULA)
     (define compound-predicate-list (syntax->datum (second interns)))
     (define term-list (third interns))    
     ;(printf "Atomic fmla: ~a ~a~n" compound-predicate-list term-list)
     `(xml-make-atomic-formula ',compound-predicate-list
                                 ,(helper-syn->xml term-list))]
    
    [(equal? first-datum 'TERM-LIST)
     (define term-xml (map helper-syn->xml (rest interns)))
      ;(printf "Term list: ~a~n" term-xml)
     `(list ,@term-xml)]
    
    [(equal? first-datum 'FUNCTION-TERM)
     (define func-name (symbol->string (syntax->datum (second interns))))
     (define sub-term-xml (map helper-syn->xml (syntax-e (third interns))))
    ; (printf "Function term: ~a ~a~n" func-name sub-term-xml)

     `(xml-make-function-term ,func-name (list ,@sub-term-xml))]
    
    [(equal? first-datum 'CONSTANT-TERM)     
     `(xml-make-constant-term ,(symbol->string (syntax->datum (second interns))))]
    [(equal? first-datum 'VARIABLE-TERM)
     `(xml-make-variable-term ,(symbol->string (syntax->datum (second interns))))]
    
    [(equal? first-datum 'EQUALS)
     (define term1-xml (helper-syn->xml (second interns)))
     (define term2-xml (helper-syn->xml (third interns)))     
     `(xml-make-equals-formula ,term1-xml ,term2-xml)]
    
    [(equal? first-datum 'ISA)
     (define the-var (helper-syn->xml (second interns))) 
     (define the-sort (syntax->string (third interns)))
     (define the-fmla (helper-syn->xml (fourth interns)))
     `(xml-make-isa-formula ,the-var ,the-sort ,the-fmla)]
    
    [(equal? first-datum 'CONDITION)
     (helper-syn->xml (second interns))]      
    
    
    [(equal? first-datum 'TRUE-CONDITION)
     '(xml-make-true-condition)]
    
    [(equal? first-datum 'TUPLING)
     `(xml-make-tupling)]
    [(equal? first-datum 'CEILINGS)
     ; Ceiling syntax for #lang margrave and m-let are ***REVERSED***:
     ; [Role 5]
     ; vs.
     ; CEILING 5 Role
     `(xml-make-ceilings '( ,@(map (compose xml-make-a-ceiling-from-pair reverse) (syntax->datum (second interns)))))]
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
    
    [(equal? first-datum 'FORALL)
     (define the-var (syntax->string (second interns)))
     (define the-sort (syntax->string (third interns)))
     (define the-fmla (helper-syn->xml (fourth interns)))
     `(xml-make-forall ,the-var ,the-sort ,the-fmla)]
    [(equal? first-datum 'EXISTS)
     (define the-var (syntax->string (second interns)))
     (define the-sort (syntax->string (third interns)))
     (define the-fmla (helper-syn->xml (fourth interns)))
     `(xml-make-exists ,the-var ,the-sort ,the-fmla)]
    
    [(equal? first-datum 'type)
     `(xml-make-type ,(syntax->string (second interns)))]
    [(equal? first-datum 'id)
     `(xml-make-id ,(syntax->string (second interns)))]
    [(equal? first-datum 'INCLUDE)
     ; List instead of quote since there's evaluation to be done inside
     `(xml-make-include (list ,@(map helper-syn->xml (rest interns))))]
    [else
     (printf "UNEXPECTED SYMBOL: ~a ~a ~n" first-intern first-datum)]))

; ***************************************************************************************
; Access the compiler and parser through these functions ONLY!

(define (parse-and-compile-port src in)
  (port-count-lines! in)  
  (compile-margrave-syntax ((parse src) (lambda () (lex in)))))

(define (parse-and-compile-read in)
  (port-count-lines! in)
  (compile-margrave-syntax ((parse "no src (read)") (lambda () (lex in)))))

(define (parse-and-compile-read-syntax in src line col pos)
  (port-count-lines! in)
  (compile-margrave-syntax ((parse src) (lambda () (lex in)))))

; ***************************************************************************************


; *************************************************
; *************************************************
; *************************************************
; Tests

(define (evaluate-parse src s)
  (define in (open-input-string s))
  (port-count-lines! in)
  ((parse src) (lambda() (lex in))))

(define (parse-and-compile s)
  (define in (open-input-string s))
  (port-count-lines! in)
  (compile-margrave-syntax ((parse "test no src") (lambda () (lex in)))))

; tests 

;(define (send-and-receive-xml foo #:syntax [bar ""]) (printf "~a ~a ~a ~n" foo bar (xexpr->string foo)))
;(define (send-and-receive-xml foo #:syntax [bar ""]) (printf "~a~n" (xexpr->string foo)))

;((eval (parse-and-compile "#LoAd policy Mypolicy = \"F:/msysgit/git/margrave/margrave/examples/conference1.p\"")))
; (parse-and-compile "#LoAd ios foo")
; (parse-and-compile "#load ios foo with x y")
; (parse-and-compile "#LoAd ios file1,file2,file3 in directory")
; (parse-and-compile "#LoAd ios file1,file2,file3 in directory with prefix suffix")
; (parse-and-compile "is poss? Myquery")
; (parse-and-compile "count Myquery")
; (parse-and-compile "let F[x, y, z] be true")

; Test var decs (and ordering), function (unary), constants, isa
; ((eval (parse-and-compile "let Myqry[z:A, y:B, x:C] be P.R(x, y, 'c, f('c)) or x = 'd and x : Sort1")))

; ((eval (parse-and-compile "show realized Myquery P.R(x, y, f('c, z))")))
; ((eval (parse-and-compile "show realized Myquery P.R(x, y, f('c, z)), P.R2(z, 'c) for cases IDB(x), P.R3(y)")))
; ((eval (parse-and-compile "show unrealized Myquery P.R(x, y, f('c, z)), P.R2(z, 'c) for cases IDB(x), P.R3(y)")))

;(parse-and-compile "show MyQuery")
;(parse-and-compile "show all MyQuery")
;((eval (parse-and-compile "reset Myquery")))
; (parse-and-compile "#quit")
; (parse-and-compile "#info")
; (parse-and-compile "#info Thingy")    


; TODO remaining:
;(parse-and-compile "show Myquery1 and Myquery2")
;(parse-and-compile "show Myquery1(x, y, z) and Myquery2")
; etc
