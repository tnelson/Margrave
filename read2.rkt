#lang racket

#|
         (define command-delimiter ";")
         
         ;delim and str are both strings
         ;Returns the string up to the first instance of delim (not including delim)
         ;If no instance of delim, returns the entire string
         (define (read-till delim str)
           (local [(define (helper s string-to-return)
                     (cond [(equal? s "") string-to-return]
                           [(equal? (substring s 0 (string-length delim)) delim) string-to-return]
                           [else (helper (substring s 1) 
                                         (string-append string-to-return (substring s 0 1)))]))]
             (helper str "")))
         
         (define (whitespace? s)
           (if (equal? s "")
               true
               (and (equal? (substring s 0 1) " ")
                    (whitespace? (substring s 1)))))
         
         ;Takes a string and returns a list of strings which are substrings of the original string, delimited by semicolons
         (define (separate-commands s)
           (cond [(whitespace? s) empty]
                 [else (let ((command (read-till command-delimiter s)))
                         (if (eq? (string-length command) (string-length s))
                             (cons command empty)
                             (cons command (separate-commands (substring s (+ 1 (string-length command)))))))]))
         
         ;Process one command string
         (define (process-string s)
           (pretty-print-response-xml (m (evalxml s))))         
|#

(require syntax/strip-context)

;(require (for-syntax "margrave.rkt"))

(provide (rename-out [read-m read]
                     [read-syntax-m read-syntax]))


; **********************************************************

(define (read-m in)
  (syntax->datum
   (read-syntax-m #f in)))


; !!! TODO
; Why can you not access the port directly? This is an ugly work-around!
; (port->string then open-input-string)
; Commented out original lines for comparison

(define (read-syntax-m src in)
  (port-count-lines! in)
    
  ;(with-syntax ([in-port in])
    (with-syntax ( [str (port->string in)])
      (strip-context
     #'(module anything racket
         (require "margrave-xml.rkt" xml "margrave.rkt" "parser-compiler.rkt")         
                                 
         (begin
           ; port->xml may return a _list_ of xml commands to send. Execute them all separately.
           (start-margrave-engine)
;           (let ((list-of-results (mm (port->xml 'in-port))))
           (let ((list-of-results (mm (port->xml (open-input-string 'str)))))
             (stop-margrave-engine)
             list-of-results))))))

