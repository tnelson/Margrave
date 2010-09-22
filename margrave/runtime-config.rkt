#lang racket

(require margrave/lang/reader
         margrave/margrave)

(provide configure)

(define (configure data)
;  (current-read-interaction read-syntax-m-single))
  (current-read-interaction even-read)
  (current-print margrave-repl-print))


; TN: taken from datalog collection.
; Why is it getting "stuck" without an EOF?
; Freezes in the lexer...

; XXX This is almost certainly wrong.
(define (even-read src ip)
  (begin0
    (read-syntax-m-single src ip)
    (current-read-interaction odd-read)))
(define (odd-read src ip)
  (current-read-interaction even-read)
  eof)

(define orig-print (current-print))
(define (margrave-repl-print proc)
  ;(printf "print: ~a~n" proc)
  ; Dev note: We get 7 #<void>s at the beginning of the repl. Why?
  ; (Doesn't seem to be due to the reader, no debug info for the voids).
  (if (procedure? proc)                                       
      (let ()                                     
        (define a-result (proc))
        (when (not (void? a-result))
          (display-response a-result)))
      (orig-print proc)))