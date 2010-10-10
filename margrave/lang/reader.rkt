; Copyright © 2009-2010 Brown University and Worcester Polytechnic Institute.
;
; This file is part of Margrave.

; Margrave is free software: you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Margrave is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public License
; along with Margrave. If not, see <http://www.gnu.org/licenses/>.

; This module describes language of Margrave commands.
; For Margrave embedded in Racket code, see margrave/racket.

#lang s-exp syntax/module-reader
#:language 'margrave

; Define our own readers
#:read read-m
#:read-syntax read-syntax-m

; Yes, read the whole body with one call of the reader
#:whole-body-readers? #t

; #:info is for *SOURCE* manipulation
; key: kind of info requested
; default value if symbol is not recognized
; default-filtering function that takes the first two and returns a result
#:info (lambda (key defval default)
         (case key           
           [(color-lexer)
            (dynamic-require 'syntax-color/default-lexer 'default-lexer)]           
           [else (default key defval)]))

; #:language-info is for *COMPILE-TIME* manipulation
#:language-info '#(margrave/language-info language-info #f)


(require racket
         syntax/strip-context         
         margrave/compiler)

(provide read-syntax-m
         read-m
         read-syntax-m-single
         read-m-single) 
 
; **********************************************************

(define (read-m in)
  (syntax->datum
   (read-syntax-m #f in)))

; **********************************************************
(define (make-syntax-for-one-func f)
  (define result-syntax (with-syntax ( [the-func-syntax f])
                         (strip-context       
                          #'the-func-syntax)))
  ;(printf "Making syntax for f=~a~nSyntax is: ~a~n." f result-syntax)
  result-syntax)
  
(define (parse-helper src in func-list-so-far)
  (if (eof-object? (peek-char in))      
      func-list-so-far
      (parse-helper src in (cons (make-syntax-for-one-func 
                                  (parse-and-compile-port src in))
                                 func-list-so-far))))

; **********************************************************

(define (read-syntax-m src in)
  (error-print-source-location #f) ; Don't say "unsaved-editor34228"   
  
  ; Parse one. Deal with it. Parse the next. Deal with it...  
  ; Built the list in reverse
  (define func-list (reverse (parse-helper src in empty)))
  
  ;(printf "Reading: ~a ~a ~n" src in)
  
  ; DEBUG
  ;(printf "Func list: ~a~n" func-list)
    
  (define result-syntax 
    (with-syntax ([syntax-func-list `(list ,@func-list)]
                  [read-syntax-m-single read-syntax-m-single])
      (strip-context 
       #`( (require margrave/margrave
                    margrave/margrave-ios ; for parse-and-load-ios
                    racket/generator) 
           
           ;(provide margrave-results)  
           
           ; Don't show a #t. Could be confusing in #lang margrave
           (define start-result 
             (start-margrave-engine #:margrave-params '("-log")))
           
           (define (handle-func a-func)          
             (define a-result (a-func))
             ;(printf "handle-func: ~a ~a~n" a-func a-result)
             (when (not (void? a-result))
               (display-response a-result))
             a-result)
           
           ; voids may be inserted as no-ops (see parser)
           ;; !!! todo: isn't there a func that maps and filters at the same time?
           (define margrave-results 
             (filter (lambda (v) (not (void? v)))
                     (map handle-func syntax-func-list)))
           ))))
  ; DEBUG  
  ;(printf "Result syntax ~a~n~n" result-syntax) 
  
  result-syntax)

; **********************************************************

; Single-command readers. Used by the REPL.

(define (read-syntax-m-single src in)
 ;(printf "-----------> In read-syntax-m-single. src=~a. in=~a.~n" src in)
  
  ; REPL will keep calling this over and over.
  ; Check to make sure there is a character waiting.
  ; If there isn't one, stop looking for one until triggered again.
  (if (char-ready? in)
      (begin
       ; (printf "First 100 characters: ~a~n" (peek-string 100 0 in))
        (make-syntax-for-one-func (parse-and-compile-port src in)))
      eof))

(define (read-m-single in)
  (syntax->datum
   (read-syntax-m-single #f in)))

