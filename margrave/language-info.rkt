#lang racket

(provide language-info)

; For use with #:language-info in the reader
(define (language-info data)
  (lambda (key default)
    (case key           
      [(configure-runtime)    
       ; Note: LIST of vectors... allows for multiple config functions to be run
      '(#(margrave/runtime-config configure #f))]
      [else default])))