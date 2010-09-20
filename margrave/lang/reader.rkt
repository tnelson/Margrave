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

racket

; Define our own readers
#:read read-syntax
#:read-syntax read-syntax-m

; Yes, read the whole body with one call of the reader
#:whole-body-readers? #t

; key: kind of info requested
; default value if symbol is not recognized
; default-filtering function that takes the first two and returns a result
#:info (lambda (key defval default)
         (case key           
           [(color-lexer)
            (dynamic-require 'syntax-color/default-lexer 'default-lexer)]
           [else (default key defval)]))


(require racket
         syntax/strip-context         
         margrave/compiler)

(provide read-syntax-m)

; **********************************************************

(define (read-m in)
  (syntax->datum
   (read-syntax-m #f in)))

; **********************************************************


(define (read-syntax-m src in)
  (error-print-source-location #t)   
  
  ; Parse one. Deal with it. Parse the next. Deal with it...
  
  (define (make-syntax-for-one-func f)
   ; (printf "Making syntax for f: ~a~n" f)
    (with-syntax ( [the-func-syntax f])
      (strip-context       
       #'the-func-syntax))) ; '(lambda ... ) at this stage. invoked in syntax below
  
  (define (parse-helper func-list-so-far)
    (if (eof-object? (peek-char in))      
        func-list-so-far
        (parse-helper (cons (make-syntax-for-one-func 
                             (parse-and-compile-port src in))
                            func-list-so-far))))
  
  ; Built the list in reverse
  (define func-list (reverse (parse-helper empty)))
  
  ; DEBUG
  ;(printf "Func list: ~a~n" func-list)
  
  
  (define result-syntax 
    (with-syntax ([syntax-func-list `(list ,@func-list)])
      (strip-context 
       #`( (require margrave/margrave
                    margrave/margrave-ios ; for parse-and-load-ios
                    racket/generator) 
           
           (provide margrave-results)  
           
           (start-margrave-engine #:margrave-params '("-log")) 
           
           (define (handle-func a-func)          
             (define a-result (a-func))
             (display-response a-result)
             a-result)
           
           (define margrave-results 
             (map handle-func syntax-func-list))))))
  ; DEBUG  
  (printf "Result syntax ~a~n~n" result-syntax) 
  
  result-syntax)
