#lang racket

(require syntax/strip-context)

;(require (for-syntax "margrave.rkt"))

(provide (rename-out [read-m read]
                     [read-syntax-m read-syntax]))


; **********************************************************

(define (read-m in)
  (syntax->datum
   (read-syntax-m #f in)))

(define (read-syntax-m src in)
  (port-count-lines! in)
  (with-syntax ([string (port->string in)])
    (strip-context
     #'(module anything racket
         (require "margrave-xml.rkt" xml "margrave.rkt" "parser-compiler.rkt")         
         
         ; Commands are semi-colon separated
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
         
         (begin
           (start-margrave-engine)
           (let ((list-of-xml (map (lambda(s) (begin (display (string-append "\nProcessing Command: " s "\n"))
                                                     (process-string  s)))
                                   (separate-commands string))))
             (stop-margrave-engine)
             list-of-xml))))))
