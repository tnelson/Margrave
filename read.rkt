#lang racket

(require (for-syntax racket syntax/stx parser-tools/yacc parser-tools/lex (prefix-in : parser-tools/lex-sre) xml))
(require (for-syntax "margrave-xml.rkt" "parser-compiler.rkt" "margrave.rkt"))
;(require (for-syntax "margrave.rkt"))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [top-level #%module-begin]))

(define-for-syntax (process-command-list command-list)
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
          [else (let ((command (read-till "$" s)))
                  (if (eq? (string-length command) (string-length s))
                      (cons command empty)
                      (cons command (separate-commands (substring s (+ 1 (string-length command)))))))]))
  
  ;Process one command string
  (define (process-string s)
    (pretty-print-info-xml (document-element (m (evalxml "source-name" s)))))
  
  (begin
    (start-margrave-engine)
    (let ((list-of-xml (map (lambda(s) (begin (display (string-append "\nProcessing Command: " s "\n"))
                                              (process-string  s)))
                            (separate-commands (foldr (lambda(symbol rest)
                                                        (string-append (symbol->string symbol) " " rest))
                                                      ""
                                                      command-list)))))
      (stop-margrave-engine)
      list-of-xml))
  )

(define-syntax (top-level body-exprs)
  (syntax-case body-exprs () ;; _ is %module-begin
    [(_ bodies ...)
     (with-syntax
         ([(processed-bodies ...)
           ;syntax->datum #'(bodies ...) reads in wherever this macro is loader, and returns a list of symbols, where each symbol is a string in the file separated by whitespace
           (process-command-list (syntax->datum #'(bodies ...)))]) 
       (syntax (#%module-begin processed-bodies ...)))]))
