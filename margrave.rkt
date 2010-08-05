;    Copyright © 2009-2010 Brown University and Worcester Polytechnic Institute.
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

(require xml "margrave-xml.rkt" "parser-compiler.rkt" "margrave-policy-vocab.rkt")

(provide stop-margrave-engine
         start-margrave-engine
         mtext
         m
         mm
         mmtext
         pause-for-user
         load-policy)
;****************************************************************
;;Java Connection

(define windows? (equal? 'windows (system-path-convention-type)))

(define java-class-separator
  (if windows?
      ";"
      ":"))    

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
(define (start-margrave-engine (home-path margrave-home-path))
  (if (eq? java-process-list #f)
      (let ([ margrave-command-line
              (string-append
               "java -cp "
               
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
                            "java_cup.jar"))
               java-class-separator
               (path->string
                (build-path home-path
                            "bin"
                            "json.jar"))
               
               
               ; Run this class:
               " edu.wpi.margrave.MCommunicator")])
        
        ;(printf "~a ~a ~a~n" home-path margrave-home-path margrave-command-line)
        (set! java-process-list (process margrave-command-line))
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
        (m "<MARGRAVE-COMMAND type=\"QUIT\" />")
        ;Should be this: (m (evalxml "QUIT"))
        (ctrl-function 'kill)  ; may not be necessary
        
        (flush-output output-port)    
        (cleanup-margrave-engine)
        #t))) 


; exit-handler doesn't get called when exiting DrRacket or when hitting Run, only when explicitly calling (exit x)
; (exit:insert-on-callback) doesn't work for some reason either
;Kill process on exit
; (exit:insert-on-callback stop-margrave-engine)

; ***************************************************************************************
; User Functions
; ***************************************************************************************

;; IMPORTANT
; When adding new user functions, be certain that all string arguments are converted to
; lower case!

; mtext
; string -> document or #f
; parses and compiles the string command into XML, executes it,
; pretty prints the results, and then returns the result document.
(define (mtext cmd)
    (let ((response-doc (m (evalxml cmd))))
      (pretty-print-response-xml response-doc)
      response-doc))


; mmtext
; string or list of string -> list of (document or #f)
; Like mtext, but accepts lists of commands and returns a list of results.
(define (mmtext cmds)
  (mm (map evalxml cmds)))
  

; mm
; XML string or list of XML string -> list of (document or #f)
(define (mm cmds)
  (if (list? cmds)
      ; Execute each in sequence.
      (map (lambda (cmd) (pretty-print-response-xml (m cmd)))
                cmds)
      ; Execute the single command; return a singleton list.
      (list (pretty-print-response-xml  (m cmds)))))


; m
; XML string -> document or #f
; Sends the given XML to java. Returns #f if the engine has not been started.
; Uses *buffered* string ports to avoid overhead due to excessive concatenation.
(define (m cmd)
  (if (equal? java-process-list #f) 
      (begin
        (printf "Could not send Margrave command because engine was not started. Call the start-margrave-engine function first.~n")
        #f)
      (begin 
        ; Comment out to disable printing XML commands as they are sent
         (printf "M SENDING XML: ~a;~n" cmd)
        
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
                        (write-string (string next-char) error-buffer)
                        (finish-error))))
                  
                  (define (fetch-result)
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
                             (fetch-result)]))))
            
            ; Populate the buffered ports            
            
            ; Handle the results
            (let* ([port-status (fetch-result)]
                   [result (get-output-string command-buffer)]
                   [error-str (get-output-string error-buffer)])
              (when (> (string-length error-str) 0)
                (printf "~n**************************************************~nAdditional ERROR information received:~n ~a~n**************************************************~n" error-str))
              (if (equal? port-status #t)
                  
                  (begin
                    ; Comment out this line to stop printing the XML
                    (printf "~a~n" result)                    
                    
                    ; Parse the reply and return the document struct
                    (read-xml (open-input-string result)))
                  
                  (begin
                    ; Got eof, the port has been cloed.
                    (printf "Margrave engine has closed. EOF reached. No document to return.")      
                    
                    ; !!! TODO: Throw exception here. Should stop even in the middle of a load-policy.
                    ; !!! TODO: Once that is done, it'l be safe to call cleanup below. (Right now, it's
                    ;           spamming with "The engine is not started..."
                    ;(cleanup-margrave-engine)
                    
                    #f))))))))



; policy-file-name -> policy-id
; This function is used because a raw (load x) call will return void, not the object desired.
; Note: rather than load with case-sensitivity turned on, all input strings need to be passed
; to the backend in lower-case.
(define (load-policy fn)
  
  ; !!! TODO Check whether case-sensitivity problems remain in DrRacket
  
  ;  (case-sensitive #t)
  ;  (display (read (open-input-file fn))) (newline)
  ;  (case-sensitive #f)  
  ; (display "*** ") (display fn) (newline)
  ;; Macro returns a func 
  ;; Potential security issues here, calling eval on arbitrary code that we "promise" is an
  ;; innocent policy definition. Is there a fix for this?
  ; (case-sensitive #t)
  (let* ([pol-result-list (evaluate-policy fn)]
         [polname (first pol-result-list)])
    
    (mm (third pol-result-list))
    (mm (fourth pol-result-list))
    polname))

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







; !!! This is now an argument to the java invocation. pass "debug" after the class name to activate it - TN
;(define (parser-debug b)
;  (m (string-append "DEBUG PARSER " myMargrave " " b)))



; !!! We now have sat4j-specific code. Not sure if it can be extended to minisat. Maybe.
; !!!  -- the new code was worthwhile (huge speed up for populated/unpopulated). - TN

; Functions to support easier query string creation

; get-existential-request-prefix
;(define (get-existential-request-prefix pol)
;  (m (string-append "GET EXISTENTIAL REQUEST PREFIX " pol)))

; get-request-prefix-closing
;(define (get-request-prefix-closing pol)
;  (m (string-append "GET REQUEST PREFIX CLOSING " pol)))

; get-request-var-vector
;(define (get-request-var-vector pol)
;  (m (string-append "GET REQUEST VAR VECTOR " pol)))

; get-idbname-list
(define (get-idbname-list pol)
  (mtext (string-append "GET RULES IN  " pol)))

; get-qualified-idbname-list
; Same as get-idbname-list but includes policy name prefix
(define (get-qualified-idbname-list pol)
  (mtext (string-append "GET QUALIFIED RULES IN " pol)))

; get-request-var-list
;(define (get-request-var-list pol)
;  (m (string-append "GET REQUEST VAR LIST " pol)))

; get-decision-for-idbname
; Policy String -> String
; Given an idbname, policy will report its decision if a rule, or the empty string otherwise
(define (get-decision-for-rule-idbname policy idbname)
  (mtext (string-append "GET DECISION FOR " policy " " idbname)))

; rules-with-higher-priority
; Policy String -> List
; Returns a list of rule idb names who have higher priority than the given rule.
; (This doesn't consider whether an overlap is possible, just the rule-ordering
;  given by combining algs.) Names are qualified with policyname:.
;; TODO: Only works for Leaves, not Sets so far.
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


