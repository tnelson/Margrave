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


; This module loads Margrave and starts the REPL, then initializes the
; Java engine automatically. When creating a Margrave distribution 
; *without* DrRacket, do Racket -> Create Executable from *this*
; module. 

; If you have DrRacket installed, there is no reason to use
; this module. - tn

#lang racket

; Need to require everything that generated syntax from lang margrave could require
; otherwise it will not be included in the executable that raco exe generates.
(require margrave/margrave
         margrave/margrave-ios
         margrave/lang/reader)

;****************************************************************
(define-namespace-anchor repl-namespace-anchor)
(define margrave-repl-namespace (namespace-anchor->namespace repl-namespace-anchor))
;****************************************************************

; Much of this code is devoted to making sure the Java engine exits.

; (exit) kills the Java engine in DrRacket, but *NOT* in 
; Racket and GRacket. So we use the exit-handler parameter.

(define orig-exit-handler (exit-handler)) 

(define (margrave-repl-exit-handler n)
  ; If stop-margrave-engine fails, don't gum up the exit.
  (with-handlers ([(lambda (e) #t) 
                   (lambda (e) (printf "Unable to close Margrave's Java engine. Caught exception:~n  ~a~n." e))])
    (stop-margrave-engine))  
  (orig-exit-handler n))

;****************************************************************

(define (load-necessary-scripts)
  ; A script has #lang margrave. Use read-syntax-m, not read-syntax-m-single
  (define args (vector->list (current-command-line-arguments)))
  
  (when (> (length args) 0)
    (printf "Loading ~a Margrave scripts...~n" (length args)))
  
  (define (load-margrave-script filename)
    (printf "  Loading Margrave script: ~a~n" filename)
    
    ; If one of the filenames is bad, close out gracefully
    (with-handlers ([(lambda (e) #t) (lambda (e) 
                                       (printf "Error loading script: ~a.~nClosing Margrave. Please try again with the correct script name.~a Error was: ~a.~n" filename e) 
                                       (stop-margrave-engine)
                                       (exit))])
      (parameterize ([current-namespace margrave-repl-namespace]               
                     [exit-handler margrave-repl-exit-handler])
        (namespace-require filename))))
  
  (for-each load-margrave-script args))


;****************************************************************

(define orig-print (current-print))

(define (margrave-repl-print proc)
  ;(printf "print: ~a~n" proc)
  ; Dev note: We get 7 #<void>s at the beginning of the repl. Why?
  ; (Doesn't seem to be due to the reader, no debug info for the voids).
  (if (procedure? proc)                                       
      (let ()                                     
        (define a-result (proc))
        (when (not (void? a-result))
          (display-response a-result)))
      (orig-print proc)))

(define (margrave-repl-exception-handler e)
  (printf "------------------------------------------------------------~n")
  (printf "~a~n" e)
  (printf "------------------------------------------------------------~n")
  (cond [(exn:fail:user? e) ((error-escape-handler))] ; user error
        [(exn:fail:read? e) ((error-escape-handler))] ; syntax error
        [(exn:fail:filesystem? e) ((error-escape-handler))] ; file not found, etc.
        [else (begin
                (stop-margrave-engine)
                (exit))]))


(define (start-margrave-repl)
  ; Do NOT parameterize current-eval, since we eval within. 
  ; Instead, parameterize current-read-interaction and current-print
    
  ; A command does not have #lang margrave. Use read-syntax-m-single
  ; Funcs will come back as (lambda ...) not #procedure. Need to invoke below.
  (parameterize ([current-namespace margrave-repl-namespace]               
                 [current-read-interaction read-syntax-m-single]  
                 [exit-handler margrave-repl-exit-handler]
                 [current-print margrave-repl-print])
    (read-eval-print-loop)))

;****************************************************************
;****************************************************************
;****************************************************************

; Defaults to the value of MARGRAVE_HOME environment var.
; If no such var, uses current directory.
(start-margrave-engine)

; Tell them how to exit.
(printf "~nWelcome to Margrave Lite. To exit, type QUIT; at the command prompt.~n~n")

; Load a script for each command line arg
(load-necessary-scripts)

; Make sure the Java engine gets terminated on an error.
; Make sure that user errors don't terminate the repl.
(call-with-exception-handler margrave-repl-exception-handler
                             start-margrave-repl)

; When REPL terminates, stop the engine (just in case)
(stop-margrave-engine)

; user error is causing margrave to exit STILL
; ahhh -- of course, it isn't force exiting, it's just propagating up to OUTSIDE the start-margrave-repl call in order to find this handler.
