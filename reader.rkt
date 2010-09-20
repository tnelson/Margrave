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
         margrave/parser-compiler)

; **********************************************************

(define (read-m in)
  (syntax->datum
   (read-syntax-m #f in)))

; **********************************************************


(define (read-syntax-m src in)
  (error-print-source-location #t)   
  
  (define compiled-func-syntax (parse-and-compile-port src in)) 
  
  ;DEBUG
  ;(printf "~a ~n" compiled-func-syntax)
  
  (with-syntax ( [results-closure-syntax compiled-func-syntax])
    (strip-context       
     #'((require margrave/margrave
                 margrave/margrave-ios ; for parse-and-load-ios
                 racket/generator) 
        
        (provide margrave-results)            
        
        ; -------------------------
        (define (resolve-custom-vector polid vecid polline polcol)
          ;(printf "~n~n~a ~a ~a ~a~n" polid vecid polline polcol)
          (define polid-str (symbol->string/safe polid))
          
          ; Only allow req for now. (Later, DEFINE VECTOR command)
          (when (not (symbol=? vecid 'req))
            (raise-user-error 'margrave-language-error "~a was an unknown vector ID.~n" vecid))
          
          ; Get request vector for this policy id
          (define info-result (send-and-receive-xml (xml-make-info-id-command polid-str)))
          (xml-policy-info->req-vector info-result))
        
        
        ; !!!!! TODO !!!!!!!!!!!!!
        ; if pol doesn't exist, standard send-and-receive will throw an error. need to catch or define a new response handler
        
        ; -------------------------        
        
        
        
        (start-margrave-engine #:margrave-params '("-log")) 
        
        ; May have a list of functions or a single one.

        (define (handle-func a-func)          
          (define a-result (a-func))
          (display-response a-result)
          a-result)
        
        (define margrave-results (if (list? results-closure-syntax)
                                     (map handle-func results-closure-syntax)
                                     (handle-func results-closure-syntax)))
                                        
        
        ; Engine is left running so that caller can use the results.       
        
        ; Make the results available
     ;   (define margrave-results            
      ;    (if (list? unprocessed-results)
      ;        (filter (lambda (x) (not (void? x))) (flatten unprocessed-results))
      ;        unprocessed-results))
        
        ; Print the results
        ;(for-each display-response margrave-results)
        ))))

