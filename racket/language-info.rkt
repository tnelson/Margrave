#lang racket

(provide language-info)

; For use with #:language-info in the reader
(define (language-info data)
  (lambda (key default)
    (case key           
      [(configure-runtime)    
       ; Note: LIST of vectors... allows for multiple config functions to be run
      '(#(margrave/runtime-config configure #f))]
      [(drracket:submit-predicate)
       margrave-repl-submit-predicate]
      [else default])))


; **********************************************************

; ALWAYS pass to Margrave's reader
(define (margrave-repl-submit-predicate interactions-port only-whitespace)
  #t)
