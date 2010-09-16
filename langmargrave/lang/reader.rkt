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

#lang s-exp syntax/module-reader
; Doesn't work. Can't find module, etc. But racket seems fine?
;margrave/lang

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
         "parser-compiler.rkt")

; **********************************************************

(define (read-m in)
  (syntax->datum
   (read-syntax-m #f in)))

; **********************************************************


(define (read-syntax-m src in)
  (error-print-source-location #t)
      
  (printf "IN read-syntax-m~n~a~n" in)
  
  (define compiled-func-syntax (parse-and-compile-port src in))
  
  ;DEBUG
  (printf "~a ~n" compiled-func-syntax)
  
  (with-syntax ( [results-closure-syntax compiled-func-syntax])
    (strip-context       
     ;#'(module anything racket
     #' ( 
     (require "margrave.rkt"
                  racket/generator) 
         (provide margrave-results)
         
         (start-margrave-engine)             
         (define unprocessed-results (results-closure-syntax))
         
         ; Engine is left running so that caller can use the results.
         
         ; Make the results available
         (define margrave-results            
           (if (list? unprocessed-results)
               (flatten unprocessed-results)
               unprocessed-results))
         
         ; Print the results
         ;(for-each display-response margrave-results)
         ))))

