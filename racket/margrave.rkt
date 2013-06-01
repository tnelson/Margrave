;    Copyright (c) 2009-2012 Brown University and Worcester Polytechnic Institute.
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

; tn
; Modifications by TN and VS, Summer 2010
; Modifications by TN through 2012

; This module is responsible for starting and stopping the Java engine, sending
; commands to the engine, etc.

#lang racket

(require xml
         srfi/13
         srfi/1
         syntax/readerr
         racket/generator
         "compiler.rkt"
         "margrave-policy-vocab.rkt"
         "test-utils.rkt"

         ; Don't have two separate versions of Vocab floating around, or can get
         ; odd behavior, like syntax-case not matching syntax. The fact that this
         ; require is NOT safe implies a bug (in Margrave's macro) somewhere... - TN
         ;(for-syntax "margrave-policy-vocab.rkt")
         
         "helpers.rkt"
         "margrave-xml.rkt")


(provide stop-margrave-engine
         start-margrave-engine
         run-java-test-cases
         mtext
         pause-for-user
         
         m-load-policy
         m-let
         m-is-poss?
         m-show-scenario
         m-get-scenario
         m-reset-scenario-iterator
         m-count-scenarios
         m-show-realized       
         
         save-all-scenarios
         
         send-and-receive-xml
         
         (all-from-out "margrave-xml.rkt")
         (all-from-out "helpers.rkt")
         (all-from-out "margrave-policy-vocab.rkt")         
                           
         send-policy-to-engine
         send-theory-to-engine
         
         the-margrave-namespace
         margrave-home-path         
         resolve-java-path!
         
         ; Old
         load-xacml-policy
         load-sqs-policy
         make-applies-list
         make-matches-list         
         resolve-custom-vector-y
         resolve-custom-vector-n
         define-custom-vector
         
         m-policy-difference-query
         m-policy-rules-idbs/applies
         m-policy-rules-idbs/matches     
         m-policy-decisions
         m-policy-decisions-idbs
         rule-applies
         rule-matches
         m-policy-rule-names-from-pid
         
         Theory
         )


;****************************************************************
(define-namespace-anchor margrave-namespace-anchor)
(define the-margrave-namespace (namespace-anchor->namespace margrave-namespace-anchor))

(define margrave-version "3.1-internal-051413")

;****************************************************************
;;Java Connection

; We spawn the java engine via process*, not process. This means
; that there is no intermediate shell between Racket and Java.
; So we can place the java engine under the control of the current
; custodian: (Valid params are 'interrupt, 'kill, and #f.)
(current-subprocess-custodian-mode 'kill)

; Are we in windows or Unix/OSX?
(define windows? (equal? 'windows (system-path-convention-type)))
(define java-class-separator
  (if windows?
      ";"
      ":")) 
(define java-exe-name
  (if windows?
      "java.exe"
      "java"))

; Must be capitalized (unix, macosx)
(define path-env "PATH")

; Where is the java executable? Will be set below.
(define java-path #f)

; Do what the shell would do: search through the user's path
(define (resolve-java-path! [concrete-path #f])
  (if concrete-path
      (begin
        (printf "Using the java executable in the path given: ~a.~n" concrete-path)
        (set! java-path concrete-path))
      (begin
        ;(printf "Searching for java executable ...~n")
        (let* ([path-value (getenv path-env)]
               [env-paths (cons (path->string (current-directory)) (regexp-split java-class-separator path-value))]
               [paths-with-java (findf (lambda (a-path) ; find first match in list                                    
                                         (and (> (string-length a-path) 0)                                     
                                              (file-exists? (build-path a-path java-exe-name))))
                                       env-paths)]
               [the-path (or paths-with-java "")])
          (if (equal? the-path "")
              (printf "Could not find java executable in PATH. Margrave will be unable to run.~n")
              (printf "Using the java executable found in: ~a~n" the-path))
          (set! java-path the-path)))))

; Search for the java exe
(resolve-java-path!)
   

; Initial values
(define java-process-list #f)
(define input-port #f)
(define output-port #f)
(define process-id #f)
(define err-port #f)
(define ctrl-function #f)



; Default margrave-home:
; (1) If the margrave collection is installed, use that path.
; (2) Else, if the MARGRAVE_HOME environment variable exists, use it.
; (3) If not, use (current-directory).

(define default-margrave-home-path-env "MARGRAVE_HOME")
(define default-margrave-home-path 
  (or (safe-get-margrave-collection-path)
      (getenv default-margrave-home-path-env)
      (current-directory)))
      
; Home path is undefined until the engine is started.
(define margrave-home-path #f)

(define (build-classpath-param home-path)
  (string-append ;; gmarceau : string-join
   
   ; This would use class files instead of the jar:
   ;(path->string
   ; (build-path home-path
   ;             "bin"))
   
   (path->string
    (build-path home-path
                "lib"
                "margrave.jar"))
   
   ; Margrave requires these JAR files to run:
   java-class-separator
   (path->string
    (build-path home-path
                "lib"
                "kodkod.jar"))
   java-class-separator
   (path->string
    (build-path home-path
                "lib"
                "org.sat4j.core.jar"))
   java-class-separator
   (path->string
    (build-path home-path
                "lib"
                "sunxacml.jar"))
   java-class-separator
   (path->string
    (build-path home-path
                "lib"
                "commons-io-2.0.1.jar"))
   java-class-separator
   (path->string
    (build-path home-path
                "lib"
                "json.jar"))))

; If the list is not initialized, the engine was never started (or was closed cleanly).
(define (engine-needs-starting?)
  (not java-process-list))

; Home-path is the location of the margrave.rkt, read.rkt, etc. files.
(define/contract (start-margrave-engine #:margrave-path [home-path default-margrave-home-path]
                               #:jvm-params [user-jvm-params empty] 
                               #:margrave-params [user-margrave-params empty])
  [->* () 
       (#:margrave-path (or/c path? string?) #:jvm-params (listof string?) #:margrave-params (listof string?))
       boolean?]
  
  ; If the engine isn't running (either uninitialized, or it died before we called this)
  (cond [(or (not java-process-list) 
             (not (eq? (ctrl-function 'status) 'running)))
         (define vital-margrave-params
           (list "-cp"                 
                 ; PARAM: classpath
                 (build-classpath-param home-path)))
         
         ; Class name comes AFTER jvm params and BEFORE margrave params
         (define margrave-params 
           (append vital-margrave-params user-jvm-params (list  "edu.wpi.margrave.MCommunicator") user-margrave-params))
         
         ;(printf "~a~n" margrave-params)        
         ; (display (cons (path->string (build-path java-path java-exe-name)) margrave-params))
         (printf "--------------------------------------------------~n")
         (printf "Starting Margrave at: ~a~n    JVM params: ~a~n    Margrave params: ~a~n"
                 home-path user-jvm-params user-margrave-params)
         (printf "Welcome to Margrave version ~a.~n" margrave-version)
         (printf "--------------------------------------------------~n")
         ;; (match-define (list ip op p-id err-p ctrl-fn) gmarceau
         ;(printf "~v~n" (cons (path->string (build-path java-path java-exe-name)) margrave-params))
         (set! java-process-list (apply process* (cons (path->string (build-path java-path java-exe-name)) margrave-params)))
         (set! input-port (first java-process-list))
         (set! output-port (second java-process-list))
         (set! process-id (third java-process-list))
         (set! err-port (fourth java-process-list))
         (set! ctrl-function (fifth java-process-list))
         (set! margrave-home-path home-path)                     
         
         (wait-for-ready-response)]
        [else #f]))

(define (wait-for-ready-response)
  ;(printf "Waiting for engine...~n")  
  (with-handlers ([exn? (lambda (e) 
                          (printf "Engine did not reply properly. Closing Engine.~n")
                          (define curr-status (ctrl-function 'status))                          
                          (when (or (equal? curr-status 'done-error)
                                    (equal? curr-status 'done-ok))
                            (printf "~a~n" (list->string (get-the-rest-in-port err-port))))
                          
                          (cleanup-margrave-engine)
                          #f)])
    (define result-xml (read-xml/document input-port))
    (when (not (response-is-success? result-xml))
      (printf "Received a fatal error from the Margrave engine on startup. Details:~n~v~n" result-xml))
    #t))

(define (cleanup-margrave-engine)
  ; Close ports politely (as requested in Racket docs)
  (close-input-port input-port)
  (close-output-port output-port)
  (close-input-port err-port)  
  
  ; allow restart of the engine
  (set! java-process-list #f)
  (set! input-port #f)
  (set! output-port #f)
  (set! process-id #f)
  (set! err-port #f)
  (set! ctrl-function #f)
  (set! margrave-home-path #f)
  
  ;Clean out caches
  (hash-remove-all cached-policies)
  (hash-remove-all cached-theories)
  (hash-remove-all cached-prior-queries))

(define (stop-margrave-engine)
  (or (not java-process-list)
      (begin
        (send-and-receive-xml (xml-make-quit))
        (ctrl-function 'kill)  ; may not be necessary
        
        (flush-output output-port)    
        (cleanup-margrave-engine)
        #t)))


; ***************************************************************************************
; User Functions
; ***************************************************************************************

;; IMPORTANT
; When adding new user functions, be certain that all string arguments are converted to
; lower case!

; mtext
; string (arbitrary number which will be automatically concatenated) -> document or #f
; parses and compiles the string command into XML, executes it,
; and returns the result document.
(define (mtext . cmd)
    
  (define cmd-func-syntax (parse-and-compile (string-append* cmd)))
  (define cmd-closures (eval cmd-func-syntax the-margrave-namespace))
  (define response-docs (if (list? cmd-closures)
                            (map (lambda (x) (x)) cmd-closures)
                            (cmd-closures)))
    
  ; DEBUG
  ;(printf "CMD-FUNC-SYNTAX: ~a ~n" cmd-func-syntax)
  ;(printf "RESPONSE-DOCS: ~a~n" response-docs)
  
  ; Return the XML document or list of replies
  (match response-docs
    [(list one) one]
    [(list items ...) items]
    [else response-docs]))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Default handler for responses: throw user errors if needed
; xml doc ---> xml doc
(define (default-response-handler src-syntax xml-response)
  #| gmarceau
  (match xml-response
    [(? response-is-exception?)
     ...]
    [(? response-is-error?)
     ...]
    [else ...]))
|#
  
  ; Syntax and Reader errors both highlight a location you give. We
  ; need that to highlight the original Margrave command.  
  (define (local-report-error)
    (if (syntax? src-syntax)
        (raise-read-error 
          (response->string xml-response)
          (syntax-source src-syntax)
          (syntax-line src-syntax)
          (syntax-column src-syntax)
          (syntax-position src-syntax)
          (syntax-span src-syntax))
        (raise-user-error (response->string xml-response))))
  
;  (match-define (list src line col pos span) (or src-loc-list
;                                                 '(#f #f #f #f #f)))
  
  (cond [(response-is-exception? xml-response)         
         (local-report-error)
          xml-response]

        [(response-is-error? xml-response)
         (local-report-error)         
         xml-response]
        
        [else xml-response]))

(define (get-the-rest-in-port aport)
  (define c (read-char aport))
  (cond [(eof-object? c) empty]
        [else (cons c (get-the-rest-in-port aport))]))    

; m
; xexpr, func -> document or #f
; Sends the given XML to java. Returns #f if the engine has not been started.
; Optional response handler func may change the result XML, throw exceptions, etc.
(define (send-and-receive-xml cmd-xexpr 
                              #:handler [response-handler-func default-response-handler]
                              #:syntax [src-syntax #f])
  
  (cond [(not java-process-list)                
         (raise-user-error "Could not send Margrave command because engine was not started. Call the start-margrave-engine function first.")
         #f]  
        [(port-closed? output-port)
         (raise-user-error "Could not send Margrave command. The port to the Java engine was closed. Please restart Margrave.")
         #f]
        [(equal? (ctrl-function 'status) 'done-error)
         (raise-user-error "Could not send Margrave command. The Java engine closed with an error. Please restart Margrave.")
         #f]
        [(equal? (ctrl-function 'status) 'done-ok)
         (raise-user-error "Could not send Margrave command. The Java engine was closed. Please restart Margrave.")
         #f]
        [else
                  
                           
         ; TODO: Java expects a fully-terminated <MARGRAVE-COMMAND> element. That is, it _MUST_ see
         ; </MARGRAVE-COMMAND> at the end of each command. This is a kludge that should be fixed, but 
         ; for now add an empty child element to force the correct termination. 
         ; ****** DO NOT REMOVE THIS ******
         (define cmd-xexpr-with-safety `(MARGRAVE-COMMAND ,(second cmd-xexpr) ,@(rest (rest cmd-xexpr)) (EMPTY ())))
         
         (define cmd-string (xexpr->string cmd-xexpr-with-safety))
         
         ; Send the command XML (DO NOT COMMENT THIS OUT)
         ; ******************************************
         ;(write-bytes-avail cmd-xexpr output-port)
                  
         (copy-port (open-input-string cmd-string) output-port)
        ; (printf "Sent XML to engine: ~a~n" cmd-string)           
         ; ******************************************
                  
         ; If the input port (stdout from engine) is closed or uninitialized, don't try to send.
         (cond [(or (false? input-port) (port-closed? input-port))
                
                (printf "The Margrave engine closed before it could be sent a command.")      
                 
                 ; !!! TODO: Throw exception here. Should stop even in the middle of a load-policy.
                 ; !!! TODO: Once that is done, it'l be safe to call cleanup below. (Right now, it's
                 ;           spamming with "The engine is not started..."
                 ;(cleanup-margrave-engine)
                                          
                #f]
               [else
                
                ; port is still open!             
                (define result-xml (read-xml/document input-port))
                
                ;(printf "~v~n" result-xml)                
                
                (define extra-err-data (get-response-extra-err result-xml))
                (define extra-out-data (get-response-extra-out result-xml))
                
                (when (> (string-length extra-out-data) 0)
                  (printf "~n~nAdditional output from Java engine:~n ~a~n" extra-out-data))
                
                (when (> (string-length extra-err-data) 0)
                  (printf "~n~nErrors from Java engine:~n ~a~n" extra-err-data))
                
                ; Parse the reply and return the document struct
                ; Pass to handler first to see if any special handling is needed (e.g. throwing errors)
                (response-handler-func src-syntax result-xml)])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; policy-id policy-file-name -> bool
(define (m-load-policy id fn)    
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started. Unable to load policy."))
      
  (define func-sexpr (create-policy-loader 
                      id 
                      (if (path? fn)
                          (path->string fn)
                          fn)
                      #'fn))
  (define load-func (eval func-sexpr the-margrave-namespace))
  (load-func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract
  (m-is-poss? qryid)
  [-> string? boolean?]
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started."))

  (define the-xml (xml-make-is-possible-command qryid))
  (define xml-response (send-and-receive-xml the-xml)) 
  (xml-bool-response->bool xml-response))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract
  (m-count-scenarios qryid)
  [-> string? (or/c number? #f)]
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started."))

  (define the-xml (xml-make-count-command qryid))
  (define xml-response (send-and-receive-xml the-xml)) 
  (define result (string->number (xml-string-response->string xml-response)))
  ; Reset the iterator:
  (m-reset-scenario-iterator qryid)
  (cond [(> result -1) result]
        [else #f]))

; Send this m-theory to the Java engine.
(define/contract
  (send-theory-to-engine athy [from-path ""])
  [-> m-theory? boolean?]  
  
  ; Don't just use the theory's name. See comments before m-theory->key.
  (when (hash-has-key? cached-theories (m-theory->key athy))    
    (error (format "The engine already knows about a theory named ~v in path ~v." (m-theory-name athy) (m-theory-path athy))))
  
  (for-each (lambda (an-xexpr) (unless (send-and-receive-xml an-xexpr)
                                 (error (format "Transfer of theory ~v to engine failed." (m-theory->key athy)))))
            (m-theory->xexprs athy))
  
  (hash-set! cached-theories (m-theory->key athy) athy)
  #t)

; Send this m-policy to the Java engine, without needing an intermediate .p file
(define/contract
  (send-policy-to-engine apol)
  [-> m-policy? boolean?]  
  (when (hash-has-key? cached-policies (m-policy-id apol))
    (error (format "The engine already knows about a policy with the id ~v." (m-policy-id apol))))
  
  (unless (hash-has-key? cached-theories (m-theory->key (m-policy-theory apol)))
    (send-theory-to-engine (m-policy-theory apol)))    
  
  (for-each (lambda (an-xexpr) (unless (send-and-receive-xml an-xexpr)
                                 (error (format "Transfer of policy ~v to engine failed." (m-policy-id apol)))))
            (m-policy->xexprs apol))
    
  (hash-set! cached-policies (m-policy-id apol) apol)
  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract
  (m-show-scenario qryid #:include [include-list empty])
  [->* [string?]
       [#:include (listof m-formula?)]
       string?]
  
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started."))
  
  (define include-xml (xml-make-include (map m-formula->xexpr include-list)))
  (define the-xml (xml-make-get-command (xml-make-type "NEXT") qryid (list include-xml)))  
  (define xml-response (send-and-receive-xml the-xml))
  
  ; May be unsat, so don't just call pretty-print-model
  (response->string (document-element xml-response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract 
  (m-show-realized qryid candidates cases)
  [string? (listof m-formula?) (listof m-formula?) . -> . (or/c hash? list?)]
  
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started."))
    
  (define the-candidates-xexpr-list (map m-formula->xexpr candidates))
  (define the-cases-xexpr-list 
    (cond [(empty? cases) empty]
          [else (list (xml-make-forcases (map m-formula->xexpr cases)))]))
    
  (define the-xml (xml-make-show-realized-command qryid (append 
                                                         the-candidates-xexpr-list
                                                         the-cases-xexpr-list)))
  (define xml-response (send-and-receive-xml the-xml))
  
  (cond
    [(empty? cases)
     (xml-set-response->list (document-element xml-response))]
    [else
     (xml-map-response->map (document-element xml-response))]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract 
  (m-reset-scenario-iterator qryid)
  [string? . -> . boolean?]  
  
  (define the-xml (xml-make-reset-command qryid))    
  (define xml-response (send-and-receive-xml the-xml))  
  (equal? "success" (get-response-type xml-response)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/contract
  (m-get-scenario qryid #:include [include-list empty])
  [->* [string?]
       [#:include (listof m-formula?)]
       (or/c m-unsat? m-scenario?)]

  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started."))
  
  (define include-xml (xml-make-include (map m-formula->xexpr include-list)))
  (define the-xml (xml-make-get-command (xml-make-type "NEXT") qryid (list include-xml)))
  (define xml-response (send-and-receive-xml the-xml))
  
  (xml->scenario (document-element xml-response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; let MyQry [x : A, y : B] be r(x) and q(y) ...
; (m-let MyQry '([s Subject] [a Action] [r Resource]) '(and ([MyPol permit] s a r) (Write a)))        
; sexpr-vars can be empty, but must be given.

(define/contract
  (m-let qryid sexpr-vars sexpr-fmla #:under [under-list empty] #:debug [debug-level 0] #:ceiling [ceilings-list empty])
  [->* (string? list? (or/c symbol? list?))
       (#:under list? #:debug integer? #:ceiling list?)       
       (or/c void? boolean?)]
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started. Unable to load policy."))

  (when (hash-has-key? cached-prior-queries qryid)
    (raise-user-error (format "Unable to create query. The query name ~v is already in use." qryid)))
  
  (define (handle-var-dec-sexpr->xexpr sexpr)  
    (unless (and (list? sexpr) (= (length sexpr) 2))
      (raise-user-error (format "Variable declaration was not of the proper form: ~v. Expected [varname sortid]." sexpr)))
    (xml-make-variable-declaration (symbol->string (first sexpr))
                                   (symbol->string (second sexpr))))

  (define free-vars-xml (map handle-var-dec-sexpr->xexpr sexpr-vars))
  (define query-condition-xml (m-formula->xexpr sexpr-fmla))  
  (define processed-under-list (map (compose xml-make-policy-identifier ->string) under-list))
  (define query-options (list
                         (xml-make-under processed-under-list)
                         (xml-make-debug debug-level)
                         (xml-make-ceilings (map xml-make-a-ceiling-from-pair ceilings-list))))
    
  ; Flatten is safe because sexpr-vars is a list of 2-element lists of symbols
  (define vars-environment (apply hash (flatten sexpr-vars)))
      
  (define uber-vocab (get-uber-vocab-for-formula sexpr-fmla #:under under-list))
  (define idb-arity (map (lambda (decl) (->string (second decl))) sexpr-vars))  
  
  ; Confirm that the formula is well-sorted BEFORE saving the query under the given name...
  (m-formula-is-well-sorted?/err uber-vocab sexpr-fmla vars-environment)
  
  (hash-set! cached-prior-queries qryid (m-prior-query qryid uber-vocab (hash qryid idb-arity)))  
  ;(printf "hash: ~v ~v~n" qryid (hash-ref cached-prior-queries qryid))    
  
  (define the-xml
     (xml-make-explore-command 
      qryid
      free-vars-xml
      (list query-condition-xml)           
      query-options))
  
  ;(printf "~a~n" the-xml)
    
  (define xml-response (send-and-receive-xml the-xml))  
  (unless (equal? "explore-result" (get-response-type xml-response))
    #f))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; xacml-policy-filename -> MPolicy
; Loads an XACML policy 
(define (load-xacml-policy pol-id fn #:syntax [src-syntax #f])
  (file-exists?/error fn src-syntax (format "Could not find XACML file: ~a" fn))
  (file-exists?/error fn src-syntax (format "Could not find SQS file: ~a" fn))
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started. Unable to load policy."))
  (when (hash-has-key? cached-policies pol-id)
    (error (format "The engine already knows about a policy with the id ~v." pol-id)))
  
  (define response (send-and-receive-xml 
                    (xml-make-load-xacml pol-id 
                                         fn
                                         (path->string (build-path (safe-get-margrave-collection-path) "xacml20.xsd")))))
  (when (response-is-success? response)
    ; Expect an extended reply
    (define polexpr (read (open-input-string (success->policy-sexpr response))))
    (define thyexpr (read (open-input-string (success->theory-sexpr response))))    
    ;(printf "~v~n" pol-id)
    (define thethy  (time (eval thyexpr the-margrave-namespace)))
    (define thepolfunc  (time (eval polexpr the-margrave-namespace)))
    (define thepol  (time (thepolfunc fn pol-id src-syntax thethy))  )  
    (hash-set! cached-theories (m-theory->key thethy) thethy)
    (hash-set! cached-policies pol-id thepol))
  (response->string response))

; sqs-policy-filename -> MPolicy
; Loads an SQS
(define (load-sqs-policy pol-id fn #:syntax [src-syntax #f])
  (file-exists?/error fn src-syntax (format "Could not find SQS file: ~a" fn))
  (when (engine-needs-starting?)
    (raise-user-error "The Java engine is not started. Unable to load policy."))
  (when (hash-has-key? cached-policies pol-id)
    (error (format "The engine already knows about a policy with the id ~v." pol-id)))

  (define response (send-and-receive-xml (xml-make-load-sqs pol-id fn)))
  (when (response-is-success? response)
    ; Expect an extended reply
    (define polexpr (read (open-input-string (success->policy-sexpr response))))
    (define thyexpr (read (open-input-string (success->theory-sexpr response))))    
    (define thethy (eval thyexpr the-margrave-namespace))
    (define thepolfunc (eval polexpr the-margrave-namespace))
    (define thepol (thepolfunc fn pol-id src-syntax thethy))
    (hash-set! cached-theories (m-theory->key thethy) thethy)
    (hash-set! cached-policies pol-id thepol))
  (response->string response))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Take a rule-name list and create a list of _applies (or _matches) IDB names from it. 
; Optional policy name will be appended to the beginning.
; If the rule name contains a colon, it will be quoted.
(define (make-idb-list-with-suffix rlist suffix polname)
  (let ([polprefix (if (equal? polname "")
                       ""
                       (string-append polname ":"))])
    (for/list ([rulename rlist])
              (if (string-contains rulename ":")
                  #| gmarceau (format "~a\"~a\"" polprefix rulename suffix) |#

                  (string-append polprefix "\"" rulename suffix "\"")
                  (string-append polprefix rulename suffix)))))


(define (make-applies-list rlist (polname ""))
  (make-idb-list-with-suffix rlist "_applies" polname))

(define (make-matches-list rlist (polname ""))
  (make-idb-list-with-suffix rlist "_matches" polname))


; **************************************************************

; read from a port until until nothing is left. This WILL block.
(define (read-until-eof buffer target-port) 
  (let ([next-char (read-char target-port)])
    (when (not (equal? next-char eof))
      (write-string (string next-char) buffer)
      (read-until-eof buffer target-port))))

; To run tests:
(define (run-java-test-cases (home-path default-margrave-home-path) (user-jvm-params empty))
  (let* ([error-buffer (open-output-string)]
         [ vital-margrave-params
           (list "-cp"
                 
                 ; PARAM: classpath
                 (build-classpath-param home-path))]
         ; Class name comes AFTER jvm params and BEFORE margrave params
         [margrave-params (append vital-margrave-params user-jvm-params (list  "edu.wpi.margrave.MJavaTests"))])
    
    ;(printf "~a~n" margrave-params)        
    ;(display (cons (string-append java-path java-exe-name)  margrave-params))
    (printf "--------------------------------------------------~n")
    (printf "Running Margrave's Java test library...~n    Margrave path was: ~a~n    Java path was: ~a~nJVM params: ~a~n"
            home-path java-path user-jvm-params)
    (printf "--------------------------------------------------~n")
    
    (define test-process-list (apply process* (cons (path->string (build-path java-path java-exe-name)) margrave-params)))
    
    ; Keep waiting until the JVM closes on its own (tests will print results via stdout)  
    (read-until-eof error-buffer (first test-process-list))
    (printf "~a~n" (get-output-string error-buffer))    
    #t))


(define (pause-for-user) 
  (printf "======================== Hit enter to continue. ========================~n~n")

  (define (clear-port-helper)            
    (when (char-ready?)
      (read-char)))

  (clear-port-helper)
  (read-line)
  (clear-port-helper))




; ********************************************************
; Helper functions
; ********************************************************

; -------------------------
(define (resolve-custom-vector-y polid vecid vector-syntax)
  ;(printf "~n~n~a ~a ~a ~a~n" polid vecid polline polcol)
  (define polid-str (->string polid))
  
  ; Only allow req for now. (Later, DEFINE VECTOR command)
  (when (not (symbol=? vecid 'req))
    (raise-syntax-error 
          '|Margrave Error:|
          (format "~a was an unknown vector ID.~n" vecid)
          vector-syntax))
  
  ; Get request vector for this policy id
  (define info-result (send-and-receive-xml (xml-make-info-id-command polid-str) #:syntax vector-syntax))
  (xml-policy-info->req-vector info-result))

(define custom-vector-environment (make-hash))

(define (resolve-custom-vector-n vecid vector-syntax)    
  (unless (hash-has-key? custom-vector-environment vecid)
    (raise-syntax-error 
     '|Margrave Error:|
     (format "~a was not declared via DEFVEC.~n" vecid)
     vector-syntax))
  (hash-ref custom-vector-environment vecid))

(define (define-custom-vector vecid contents)
;  (printf "defvec: ~a ~a ~n" vecid contents)
  (hash-set! custom-vector-environment vecid contents)
  (format "Custom vector <~a> defined." vecid))

; Helpers for scenario-saving
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-scenarios-to-file qid fileport brief include)
  [string? port? boolean? list? . -> . any/c]
  (define result (m-get-scenario qid #:include include))  
  (when (m-scenario? result)  
    (display (m-scenario->string result #:brief brief) fileport)
    (display "\n\n" fileport)
    (display-scenarios-to-file qid fileport brief include)))

(define (save-all-scenarios qid #:brief [brief #f] #:include [include empty])   
  [->* (string?)
       (#:brief boolean?)
       any/c]
  (m-reset-scenario-iterator qid) ; start at beginning of scenario-stream
  (define out (open-output-file (string-append "scenarios-" qid ".txt") #:exists 'replace))
  (display (format "*** Found ~v scenarios for query with ID: ~v. ***~n~n" (m-count-scenarios qid) qid) out)
  (display-scenarios-to-file qid out brief include)
  (m-reset-scenario-iterator qid) ; return to beginning of stream
  (close-output-port out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Functions to get IDB names for rules. For use with #:include. 

; m-policy-rules-apply
; m-policy-rules-match
; m-policy-decisions

;#:include `( ,@(m-policy-rules-apply '(s a r))
;             ,@(m-policy-rules-match '(s a r)))

;;;;;;;;;;;;;;;;
; We need the policy's registered name for use in 
; query sexprs. So accept the name, not m-policy itself.
; If you have an m-policy, just register it under some name
; and use that name instead!

(define/contract (get-policy/error pid)
  [string? . -> . m-policy?]
  (cond [(not (hash-has-key? cached-policies pid))
         (error (format "No policy found registered with name: ~a~n" pid))]
        [else (hash-ref cached-policies pid)]))

(define (m-policy-rule-names-from-pid pid)
  (define pol (get-policy/error pid))
  (m-policy-rule-names pol))

(define/contract (m-policy-rules-idbs/suffix pid suffix varvec)
 [string? string? (listof m-term?) . -> . any/c]    
  (map (lambda (rname) `( [,pid ,(string-append rname suffix)] ,@varvec))
       (m-policy-rule-names-from-pid pid)))

(define (rule-applies rule-id)
  (->symbol (string-append (->string rule-id) "_applies")))
(define (rule-matches rule-id)
  (->symbol (string-append (->string rule-id) "_matches")))


(define/contract (m-policy-rules-idbs/matches pid varvec)
  [string? (listof m-term?) . -> . any/c]  
  (m-policy-rules-idbs/suffix pid "_matches" varvec))

(define/contract (m-policy-rules-idbs/applies pid varvec)
  [string? (listof m-term?) . -> . any/c]  
  (m-policy-rules-idbs/suffix pid "_applies" varvec))

(define/contract (m-policy-decisions pol-or-pid)
  [(or/c string? m-policy?) . -> . (listof string?)]  
  (define pol (cond [(m-policy? pol-or-pid) pol-or-pid]
                    [else (get-policy/error pol-or-pid)])) 
  (define rules (hash-values (m-policy-rules pol)))
  (remove-duplicates (map m-rule-decision rules)))

(define/contract (m-policy-decisions-idbs pid varvec)
  [string? (listof m-term?) . -> . any/c]  
  (map (lambda (rname) `([,pid ,(->symbol rname)] ,@varvec))
       (m-policy-decisions pid)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Change impact sugar. returns same as m-let; sets up a stock change-impact
; query for 2 policies.

(define/contract (m-policy-difference-query qid p1id p2id 
                                            #:debug [debug-level 0]
                                            #:ceiling [ceilings-list empty]
                                            #:variables [req-vector #f])
 [->* (string? string? string?)
      (#:debug integer? #:ceiling list? #:variables (or/c false? (listof symbol?)))
      (or/c void? boolean?)] 
    
  (define pol1 (get-policy/error p1id))
  (define pol2 (get-policy/error p2id))
  (define decstrings1 (m-policy-decisions pol1))
  (define decstrings2 (m-policy-decisions pol2))
  
  ; MUST HAVE: the same decisions; 
  ;            the same request vector across all decisions.
    
  
  (unless (equal-unordered? decstrings1 decstrings2)
    (error (format "To use m-policy-different-query, the policies must have identical decision-sets." )))
  (when (empty? decstrings1)
    (error (format "To use m-policy-different-query, the policies must each have at least one decision." )))
  
  (define vec1 (hash-ref (m-policy-idbs pol1) (first decstrings1)))
  (define vec2 (hash-ref (m-policy-idbs pol2) (first decstrings2)))
  
  ; If user provides a request vector, use it. Otherwise, construct a <v0, ..., vk> tuple of variable ids.
  (define varvector 
    (cond
      [req-vector req-vector]
      [else (map (lambda (i) (string->symbol (string-append "v" (->string i)))) 
                 (build-list (length vec1) values))]))
  
  (unless (equal? (length vec1) (length varvector))
    (error (format "The request variables provided: ~v did not have the expected arity: ~v~n" varvector (length vec1))))
  
  (define fvars (zip varvector vec1))
  
  (define (for-dec1 decname)
    `(and ([,p1id ,decname] ,@varvector) 
          (not ([,p2id ,decname] ,@varvector))))
  (define (for-dec2 decname)
    `(and ([,p2id ,decname] ,@varvector) 
          (not ([,p1id ,decname] ,@varvector))))
  
  ; p1 yields A and p2 does not or
  ; p2 yields A and p1 does not or
  ; p1 yields B and p2 does not or ... 
  (define fmla `(or ,@(map for-dec1 decstrings1)
                    ,@(map for-dec2 decstrings2)))
  
  ;(printf "mpdq: ~v~n~v~n~v~n~v~n~v~n~v~n" decstrings1 vec1 vec2 fvars fmla (map for-dec1 decstrings1))    
  (m-let qid fvars fmla
         #:debug debug-level #:ceiling ceilings-list))
