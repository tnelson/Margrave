#lang racket

(require scribble/manual
         scribble/core
         (for-syntax racket/port
                     racket/list
                     scribble/manual
                     scribble/core))
(provide multiline-racketblock
         multiline-racketinput)


; The key here is the use of wrapping and then unwrapping via syntax->datum and syntax, which
;   lets us use the pattern variable the-string.
; @racketblock["foo" "bar"] will NOT insert newlines. 
; @racketblock["foo"
; "bar"] will. So we need to add the linebreak and indentation explicitly.
(define-syntax (multiline-racketblock syn)
  (syntax-case syn ()
    [(_ the-string)
     (let ()
       (define syntax-result 
         #`(racketblock 
            #,@(foldr append '()(map (lambda (x) (list `#, (racketresultfont (tt ,x))
                                                       `#, (linebreak) 
                                                       `#, (hspace 2))) 
                                     (port->lines (open-input-string (syntax->datum #'the-string)))))))
       ;(printf "~a~n" syntax-result)
       syntax-result)]))

(define-syntax (multiline-racketinput syn)
  (syntax-case syn ()
    [(_ the-string)
     (let ()
       (define syntax-result 
         #`(racketinput 
            #,@(foldr append '()(map (lambda (x) (list `#, (tt ,x)
                                                       `#, (linebreak) 
                                                       `#, (hspace 2))) 
                                     (port->lines (open-input-string (syntax->datum #'the-string)))))))
       ;(printf "~a~n" syntax-result)
       syntax-result)]))