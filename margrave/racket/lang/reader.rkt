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

; This language is Racket with embedded Margrave commands.

#lang s-exp syntax/module-reader
#:language 'margrave/racket

; Define our own readers
#:read read-m-r
#:read-syntax read-syntax-m-r

; Yes, read the whole body with one call of the reader
#:whole-body-readers? #t

; #:info is for *SOURCE* manipulation
; key: kind of info requested
; default value if symbol is not recognized
; default-filtering function that takes the first two and returns a result
;#:info (lambda (key defval default)
;         (case key           
;           [(color-lexer)
;            (dynamic-require 'syntax-color/default-lexer 'default-lexer)]           
;           [else (default key defval)]))

; #:language-info is for *COMPILE-TIME* manipulation
;#:language-info '#(margrave/racket/language-info language-info #f)


(require racket
         syntax/strip-context         
         (only-in margrave/lang/reader 
                  read-syntax-m-single
                  read-m-single))

(provide read-syntax-m-r
         read-m-r) 
 
; **********************************************************

(define (read-m-r in)
  (printf "In read-m-r")
  
  (syntax->datum
   (read-syntax-m-r #f in)))

; **********************************************************

(define (read-syntax-m-r src in)
  (error-print-source-location #t)   
  
  (printf "In read-syntax-m-r~n")
  
  ; Add a read-table and call the normal Racket reader.
  (define read-results 
     (parameterize ([current-readtable (make-readtable (current-readtable) #\M 'dispatch-macro m-r-reader) ])
       (read-syntax src in)))
  
  (printf "~a~n" read-results)
  
  ; caller expects LIST of syntax
  (with-syntax ([the-read-results read-results])
    (strip-context 
     #'(the-read-results))))

; **********************************************************

; Structure taken from docs section 12.9.1

(define m-r-reader
  (case-lambda
    [(ch port)
     ; `read' mode
     (read-m-single)]
    [(ch port src line col pos)
     ; `read-syntax' mode
     (read-syntax-m-single src port)]))