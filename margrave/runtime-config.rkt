#lang racket

(require margrave/lang/reader
         margrave/margrave)

(provide configure)

(define (configure data)
 ; (printf "In configure~nCPR was: ~a~n" (current-prompt-read))
  (current-read-interaction margrave-repl-read-interaction)
  (current-prompt-read margrave-repl-prompt-read)
  (current-print margrave-repl-print))


; TN: taken from datalog collection.
; TN: I opted to use char-ready? instead, but keeping this code just in case.
#|
; XXX This is almost certainly wrong.
(define (even-read src ip)  
  (begin0
    (read-syntax-m-single src ip)
    (current-read-interaction odd-read)))
(define (odd-read src ip)
  (current-read-interaction even-read)
  eof)
|#

(define (margrave-repl-read-interaction src in)
 ; (printf "In read interaction...")
  (read-syntax-m-single src in))

(define orig-print (current-print))
(define (margrave-repl-print proc)
  ;(printf "print: ~a~n" proc)
  ; Dev note: We get 7 #<void>s at the beginning of the repl. Why?
  ; (Doesn't seem to be due to the reader, no debug info for the voids).
   ; (printf "In m-r-print~nCPR was: ~a~n"(current-prompt-read))
  (if (procedure? proc)                                       
      (let ()                                     
        (define a-result (proc))
        (when (not (void? a-result))
          (display-response a-result)))
      (orig-print proc)))

(define (margrave-repl-prompt-read)
  (display "Margrave> ") (flush-output)
  (let ([in (current-input-port)])
    ((current-read-interaction) (object-name in) in)))