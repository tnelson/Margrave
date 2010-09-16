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

(require "margrave.rkt"
         "margrave-ios.rkt"
         racket/enter)


;****************************************************************
(define-namespace-anchor repl-namespace-anchor)
(define margrave-repl-namespace (namespace-anchor->namespace repl-namespace-anchor))

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

; For the REPL, we don't support #lang and require, etc. in scripts.
; load won't work out-of-box (c.f. sec 15.3 of Racket docs)

; Using THIS namespace instead of make-base-empty-namespace
; because we want the user to have access to stuff like
; margrave-home-path and module-tests for this module.
(define (run-lite filename)
  (parameterize ([current-namespace margrave-repl-namespace])
  ;(parameterize ([current-namespace (make-base-empty-namespace)])
    (load filename)
    
    ; Using load for now.
    ; Advantage: can re-run a "script" as many times as desired
    ; Disadvantage: Annoying requirement to leave out #lang and require in "lite" scripts.
    
    ;(if (relative-path? filename)
    ;    (namespace-require filename)
    ;    (namespace-require `(file ,filename)))
    ))

;****************************************************************
;****************************************************************
;****************************************************************
; TESTS

(define (module-tests)
  ;(run-lite (path->string (build-path margrave-home-path "examples" "full" "full-ios-demo.rkt"))) 
  (run-lite (build-path margrave-home-path "examples" "lite" "lite-ios-demo.rkt"))
       )


;****************************************************************
;****************************************************************
;****************************************************************

; Defaults to the value of MARGRAVE_HOME environment var.
; If no such var, uses current directory.
(start-margrave-engine)

; Tell them how to exit.
(printf "~nWelcome to Margrave Lite. To exit, type (exit) at the command prompt.~n~n")

; Just calling read-eval-print-loop results in an error (no #%app ...)
; Works if we give it a namespace
(parameterize ([current-namespace margrave-repl-namespace]
               [exit-handler margrave-repl-exit-handler])
  (read-eval-print-loop))



