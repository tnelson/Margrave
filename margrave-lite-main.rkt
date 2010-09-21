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


; (Keep exit handler in case of error)
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
  
  (define (load-margrave-script filename)
    (printf "Loading Margrave script: ~a~n" filename)
    (parameterize ([current-namespace margrave-repl-namespace]               
                   [exit-handler margrave-repl-exit-handler])
      (namespace-require filename)))
  
  (for-each load-margrave-script args))


(define (start-margrave-repl)
  ; Do NOT parameterize current-eval, since we eval within. 
  ; Instead, parameterize current-read-interaction and current-print
    
  ; A command does not have #lang margrave. Use read-syntax-m-single
  (parameterize ([current-namespace margrave-repl-namespace]               
                 [current-read-interaction read-syntax-m-single]  
                 [exit-handler margrave-repl-exit-handler]
                 [current-print (lambda (proc)
                                  (define a-result (proc))
                                  (when (not (void? a-result))
                                    (display-response a-result)))])
    (read-eval-print-loop)))

;****************************************************************

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
               
;****************************************************************
;****************************************************************
;****************************************************************

; Defaults to the value of MARGRAVE_HOME environment var.
; If no such var, uses current directory.
(start-margrave-engine)

; Tell them how to exit.
(printf "~nWelcome to Margrave Lite. To exit, type QUIT; at the command prompt.~n~n")

; Make sure the Java engine gets terminated on an error.
; Make sure that user errors don't terminate the repl.
(call-with-exception-handler margrave-repl-exception-handler
                             start-margrave-repl)

; When REPL terminates, stop the engine (just in case)
(stop-margrave-engine)

; user error is causing margrave to exit STILL
; ahhh -- of course, it isn't force exiting, it's just propagating up to OUTSIDE the start-margrave-repl call in order to find this handler.
