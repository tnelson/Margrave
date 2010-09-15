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

#lang racket


(require syntax/strip-context
         "parser-compiler.rkt")

(provide (rename-out [read-m read]
                     [read-syntax-m read-syntax]))

; **********************************************************

(define (read-m in)
  (syntax->datum
   (read-syntax-m #f in)))

; **********************************************************

(define (read-syntax-m src in)
  (error-print-source-location #t)
  
  (define lowercase-input (string-downcase (port->string in)))
  (define compiled-func-syntax (parse-and-compile lowercase-input))

  ;DEBUG
  ; (printf "~a ~a ~n" lowercase-input compiled-func-syntax)
  
  (with-syntax ( [results-closure-syntax compiled-func-syntax])
    (strip-context       
     #'(module anything racket
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

