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

; tn
; Modifications by TN and VS, Summer 2010

#lang racket

(require xml
         srfi/13
         "margrave-xml.rkt"
         "parser-compiler.rkt"
         "margrave-policy-vocab.rkt")

(provide stop-margrave-engine
         start-margrave-engine
         mtext
         pause-for-user
         load-policy
         
         xml-explore-result->id
         xml-set-response->list
         xml-list-response->list
         xml-map-response->map
         xml-bool-response->bool
         pretty-print-response-xml
         display-response
         response->string
         get-qualified-rule-list
         get-rule-list
         time-since-last
         make-applies-list
         make-matches-list)
;****************************************************************
;;Java Connection

; We spawn the java engine via process*, not process. This means
; that there is no intermediate shell between Racket and Java.
; So we can place the java engine under the control of the current
; custodian: (Valid params are 'interrupt, 'kill, and #f.)
(current-subprocess-custodian-mode 'interrupt)

; Are we in windows or Unix/OSX?
(define windows? (equal? 'windows (system-path-convention-type)))
(define java-class-separator
  (if windows?
      ";"
      ":")) 

; Where is the java executable?
; Do what the shell would do: search through the user's path
(define java-path
  (begin
    (printf "Searching for java.exe ...~n")
    (let* ([path-value (getenv "path")]
           [env-paths (cons (path->string (current-directory)) (regexp-split java-class-separator path-value))]
           [paths-with-java (filter (lambda (a-path)
                                      ;(printf "    searching in: ~a~n" a-path)
                                      (and (> (string-length a-path) 0)                                     
                                           (file-exists? (build-path a-path "java.exe"))))
                                    env-paths)]
           [the-path (if (empty? paths-with-java)
                         ""
                         (first paths-with-java))])
      (if (equal? the-path "")
          (printf "Could not find java.exe in PATH. Margrave will be unable to run.~n")
          (printf "Using the java.exe in: ~a~n" the-path))
      the-path)))

   

; Initial values
(define java-process-list #f)
(define input-port #f)
(define output-port #f)
(define process-id #f)
(define err-port #f)
(define ctrl-function #f)
(define margrave-home-path (current-directory))

; Home-path is the location of the margrave.rkt, read.rkt, etc. files.
; If not passed, will use (current-directory).
(define (start-margrave-engine (home-path margrave-home-path) (user-jvm-params empty) (user-margrave-params empty))
  (if (eq? java-process-list #f)
      (let* ([ vital-margrave-params
               (list "-cp"
                  
                     ; PARAM: classpath
                     (string-append
                   
                   ;For testing, use the .class files instead of the .jar:
                   (path->string
                    (build-path home-path
                                "bin"))
                   #;(path->string
                      (build-path home-path
                                  "bin"
                                  "margrave.jar"))
                   
                   ; Margrave requires these JAR files to run:
                   java-class-separator
                   (path->string
                    (build-path home-path
                                "bin"
                                "kodkod.jar"))
                   java-class-separator
                   (path->string
                    (build-path home-path
                                "bin"
                                "org.sat4j.core.jar"))
                   java-class-separator
                   (path->string
                    (build-path home-path
                                "bin"
                                "sunxacml.jar"))
                   java-class-separator
                   (path->string
                    (build-path home-path
                                "bin"
                                "json.jar"))))]
             ; Class name comes AFTER jvm params and BEFORE margrave params
             [margrave-params (append vital-margrave-params user-jvm-params (list  "edu.wpi.margrave.MCommunicator") user-margrave-params)])

        ;(printf "~a~n" margrave-params)        
        ;(display (cons (string-append java-path "java.exe")  margrave-params))
        (printf "--------------------------------------------------~n")
        (printf "Starting Margrave's Java engine...~n    Margrave path was: ~a~n    Java path was: ~a~nJVM params: ~a~nMargrave params: ~a~n"
                home-path java-path user-jvm-params user-margrave-params)
        (printf "--------------------------------------------------~n")
        (set! java-process-list (apply process* (cons (path->string (build-path java-path "java.exe")) margrave-params)))
        (set! input-port (first java-process-list))
        (set! output-port (second java-process-list))
        (set! process-id (third java-process-list))
        (set! err-port (fourth java-process-list))
        (set! ctrl-function (fifth java-process-list))
        (set! margrave-home-path home-path)
        #t)
      #f))

(define (cleanup-margrave-engine)
  (close-input-port input-port)
  (close-output-port output-port)
  (close-input-port err-port)  
  ; allow restart of the engine
  (set! java-process-list #f)
  (set! input-port #f)
  (set! output-port #f)
  (set! process-id #f)
  (set! err-port #f)
  (set! ctrl-function #f))

(define (stop-margrave-engine)
  (if (eq? java-process-list #f)
      #f
      (begin
        (send-and-receive-xml "<MARGRAVE-COMMAND type=\"QUIT\" />")
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
(define mtext
  (lambda cmd    
    ; May be a semicolon-separated script of commands
    (let* ([cmd-func-syntax (parse-and-compile (apply string-append cmd))]
           [cmd-closure (eval cmd-func-syntax)]
           [response-docs (cmd-closure)])  
          
      ; Return the XML document or list of replies
      (if (> (length response-docs) 1)
          response-docs
          (first response-docs)))))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Default handler for responses: throw user errors if needed
; xml doc ---> xml doc
(define (default-response-handler xml-response)
  (cond [(response-is-exception? xml-response) 
         (raise-user-error 'margrave-error "~a~n"
                           (pretty-print-response-xml xml-response) )
         xml-response]
        
        [(response-is-error? xml-response)
         (raise-user-error 'margrave-error "~a~n"
                           (pretty-print-response-xml xml-response))
         xml-response]
        
        [else xml-response]))


; m
; XML string, func -> document or #f
; Sends the given XML to java. Returns #f if the engine has not been started.
; Uses *buffered* string ports to avoid overhead due to excessive concatenation.
; Optional response handler func may change the result XML, throw exceptions, etc.
(define (send-and-receive-xml cmd (response-handler-func default-response-handler))
  (if (equal? java-process-list #f) 
      (begin
        (printf "Could not send Margrave command because engine was not started. Call the start-margrave-engine function first.~n")
        #f)
      (begin 
        ; DEBUG: Comment out to disable printing XML commands as they are sent
        ;(printf "M SENDING XML: ~a;~n" cmd)
        
        ; Send the command XML (DO NOT COMMENT THIS OUT)
        ; ******************************************
        (display (string-append cmd ";") output-port)
        (flush-output output-port)        
        ; ******************************************
        
        ; Deal with the result
        (let ([command-buffer (open-output-string)]
              [error-buffer (open-output-string)]) 
          (local ((define (flush-error)  ; read until nothing is left. This WILL block.
                    (let ([next-char (read-char err-port)])                                                
                      (when (not (equal? next-char eof))
                        (begin
                          (write-string (string next-char) error-buffer)
                          (flush-error)))))
                  
                  (define (finish-error)
                    (when (char-ready? err-port)  ; If there is a character waiting, read it.
                      (let ([next-char (read-char err-port)])
                        (when (not (equal? next-char eof))
                          (begin
                            (write-string (string next-char) error-buffer)
                            (finish-error))))))
                  
                  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  ; version before change to polling. this version will freeze up if
                  ; too much err-port data is received while blocked waiting for input-port
                  
               #|   (define (fetch-result)
                    ; Before checking for results on the input-port
                    ; check for error bytes waiting.
                    (finish-error)
                                        
                    ; Now read from the input port (This blocks)
                    (let ([next-char (read-char input-port)])                      
                      (cond [(equal? next-char #\nul)
                             ; End of command's response. Finish any error data that may be waiting.
                             (finish-error)
                             #t]
                            [(equal? next-char eof)
                             ; Port closed. Read error until eof.
                             (flush-error)
                             #f]
                            [else 
                             ; In progress. Keep reading.                             
                             (write-string (string next-char) command-buffer)
                             
                             ; DEBUG: Uncomment this to print each char as it is received.
                             ;(display next-char)
                             
                             (fetch-result)]))) |#
                  ;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  
                  ; TEMPORARY: this fetch-result uses polling; won't block and possibly freeze
                  ; if err port sends while it waits for input port.
                  ; TODO: use threads or custom ports to do this without polling
                  (define (fetch-result)
                    (if (char-ready? input-port) 
                        (let ([next-char (read-char input-port)])                      
                          (cond [(equal? next-char #\nul)
                                 ; End of command's response. Finish any error data that may be waiting.
                                 (finish-error)
                                 #t]
                                [(equal? next-char eof)
                                 ; Port closed. Read error until eof.
                                 (flush-error)
                                 #f]
                                [else 
                                  ; In progress. Keep reading.  
                                 (write-string (string next-char) command-buffer)                                                                  
                                 (fetch-result)]))
                        (begin
                          (finish-error)
                          (fetch-result))))
                  
                  )
            
            ; Populate the buffered ports                        
            ; Handle the results
            (let* ([port-status (fetch-result)]
                   [result (get-output-string command-buffer)]
                   [error-str (get-output-string error-buffer)])
              (when (> (string-length error-str) 0)
                (printf "~n**************************************************~nAdditional information from Java engine:~n ~a~n**************************************************~n" error-str))
              (if (equal? port-status #t)
                  
                  (begin
                    ; DEBUG: Comment out this line to stop printing the XML
                    ;(printf "~a~n" result)                    
                    
                    ; Parse the reply and return the document struct
                    ; Pass to handler first to see if any special handling is needed (e.g. throwing errors)
                    (response-handler-func (read-xml (open-input-string result))))
                  
                  (begin
                    ; Got eof, the port has been cloed.
                    (printf "Margrave engine has closed. EOF reached. No document to return.")      
                    
                    ; !!! TODO: Throw exception here. Should stop even in the middle of a load-policy.
                    ; !!! TODO: Once that is done, it'l be safe to call cleanup below. (Right now, it's
                    ;           spamming with "The engine is not started..."
                    ;(cleanup-margrave-engine)
                    
                    #f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; policy-file-name -> policy-id
; Note: rather than load with case-sensitivity turned on, all input strings need to be passed
; to the backend in lower-case.
(define (load-policy fn)
  
  (let* ([pol-result-list (evaluate-policy fn)]
         [polname (first pol-result-list)]
         [vocabname (second pol-result-list)]
         
         ; Java will handle creation of the vocab if it hasn't already been created.
         
         ; Eval and invoke the function to create the vocab
         [vocab-results ((eval (third pol-result-list)))]
         
         ; Eval and invoke the function to create the policy
         [policy-results ((eval (fourth pol-result-list)))])
        
    polname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; !!!
; Need to support these once more. Commands exist, need to route them in java. - TN

; xacml-policy-filename -> MPolicy
; Loads an XACML policy 
;(define (load-xacml-policy fn)
;  ((generic-java-method '|readXACML|) (java-null <MPolicy>) (->jstring fn) (->jstring (string-append my-directory "xacml20.xsd"))))

; sqs-policy-filename -> MPolicy
; Loads an XACML policy 
;(define (load-sqs-policy fn)
;  ((generic-java-method '|loadSQS|) (java-null <MPolicy>) (->jstring fn) ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Functions to support easier query string creation

; get-idbname-list
(define (get-rule-list pol (decision ""))
  (if (equal? decision "")      
      (xml-list-response->list (document-element (mtext (string-append "GET RULES IN " pol))))
      (xml-list-response->list (document-element (mtext (string-append "GET RULES IN " pol " WITH DECISION " decision))))))

; get-qualified-idbname-list
; Same as get-idbname-list but includes policy name prefix
(define (get-qualified-rule-list pol (decision ""))
  (if (equal? decision "")      
      (xml-list-response->list (document-element (mtext (string-append "GET QUALIFIED RULES IN " pol))))
      (xml-list-response->list (document-element (mtext (string-append "GET QUALIFIED RULES IN " pol " WITH DECISION " decision))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Take a rule list and create a list of _applies (or _matches) IDB names from it. 
; Optional policy name will be appended to the beginning.
; If the rule name contains a colon, it will be quoted.
(define (make-idb-list-with-suffix rlist suffix polname)
  (let ([polprefix (if (equal? polname "")
                       ""
                       (string-append polname ":"))])
    (map (lambda (rulename)
           (if (equal? (string-contains rulename ":") #f)
               (string-append polprefix rulename suffix)
               (string-append polprefix "\"" rulename suffix "\"")))
         rlist)))

(define (make-applies-list rlist (polname ""))
  (make-idb-list-with-suffix rlist "_applies" polname))

(define (make-matches-list rlist (polname ""))
  (make-idb-list-with-suffix rlist "_matches" polname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; [not yet in]
; get-decision-for-idbname
; Policy String -> String
; Given an idbname, policy will report its decision if a rule, or the empty string otherwise
;(define (get-decision-for-rule-idbname policy idbname)
;  (mtext (string-append "GET DECISION FOR " policy " " idbname)))

; rules-with-higher-priority
; Policy String -> List
; Returns a list of rule idb names who have higher priority than the given rule.
; (This doesn't consider whether an overlap is possible, just the rule-ordering
;  given by combining algs.) Names are qualified with policyname:.
; Only works for Leaves, not Sets.
(define (rule-idbs-with-higher-priority pol rulename)
  (mtext (string-append "GET HIGHER PRIORITY THAN " pol " " rulename)))

; **************************************************************
; Test case procedures

(define (test desc s1 s2)
  (if (eqv? s1 s2)
      (display (string-append desc ": Passed."))
      (display (string-append desc ": FAILED!")))
  (newline))

; !!! Not provided. Should it be? Can get count at size via COUNT... - TN
;(define (test-model desc qry size exp_sols exp_ceiling)
;  (if (->boolean 
;       (m (string-append "RUN TEST CASE " qry " " size " " exp_sols " " exp_ceiling)))
;      (display (string-append desc ": Passed."))
;      (display (string-append desc ": FAILED!")))
;  (newline))

(define (pause-for-user) 
  (printf "======================== Hit enter to continue. ========================~n~n")
  (read-char))

(define (display-response the-response)
  (printf "~a~n" (pretty-print-response-xml the-response)))

; Better function name. May make it do more later, but for now just a wrapper.
(define (response->string the-response)
  (pretty-print-response-xml the-response))


; ********************************************************
; Helper functions
; ********************************************************

; Easy timer function
; Initial value
(define tick-tock #f)
(define (time-since-last)
  (if (eq? tick-tock #f)
      (begin 
        (set! tick-tock (current-inexact-milliseconds))
        #f)
      (let ([ms-to-return (- (current-inexact-milliseconds) tick-tock)]) 
        (set! tick-tock (current-inexact-milliseconds))
        ms-to-return)))


;(define (string-endswith str end)
;  (if (> (string-length end) (string-length str))
;      #f
;      (string=? end
;                (substring str
;                           (- (string-length str) (string-length end))
;                           (string-length str)))))

; Strip everything up to and including the last :
;(define (unqualified-part idbname)
;  (last (regexp-split ":" idbname)))

;(define (unqualified-non-applied-part idbname)
;  (unqualified-part (if (string-endswith idbname "_applies")
;                        (substring idbname 0 (- (string-length idbname) 8))
;                        idbname)))

; kludge: in general there may be brackets in the idb name. For our example, there aren't. (see todo above re: structured data)
;(define (cleanup-idb-list thelist)
;  (map (lambda (idbname) 
;         (second (regexp-split ":" (first (regexp-split "\\[" idbname))))) 
;       thelist))
;(define (cleanup-idb-list-no-applies thelist)
;  (map (lambda (idbname) 
;         (second (regexp-split ":" (first (regexp-split "_applies\\[" idbname))))) 
;       thelist))
