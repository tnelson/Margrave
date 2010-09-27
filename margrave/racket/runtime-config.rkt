#lang racket

(require margrave/racket/lang/reader
         margrave/margrave)

(provide configure)

(define (configure data)
  (current-read-interaction m-r-repl-read-interaction)
  (current-print m-r-repl-print))

(define (m-r-repl-read-interaction src in)
  (read-syntax-m-r-single src in))

(define orig-print (current-print))
(define (m-r-repl-print result)
  (when (not (eof-object? result))                                       
             (orig-print result)))