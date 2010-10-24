#lang racket

(require scribble/manual
         scribble/core
         (for-syntax racket/port
                     racket/list
                     scribble/manual
                     scribble/core))

(provide multiline-racketblock
         multiline-racketblock-noresult
         multiline-racketinput
         inline-result)


; The key here is the use of wrapping and then unwrapping via syntax->datum and syntax, which
;   lets us use the pattern variable the-string.
; @racketblock["foo" "bar"] will NOT insert newlines. 
; @racketblock["foo"
; "bar"] will. So we need to add the linebreak and indentation explicitly.

; Also, wrap in literal before passing to tt, or else decode-content will convert
; --- to a long dash.


(define-syntax (multiline-racketblock syn)
  (syntax-case syn ()
    [(_ the-string)
     #`(racketblock 
        #,@(foldr append '()(map (lambda (x) (list `#, (racketresultfont (tt (literal ,x)))
                                                   `#, (linebreak) 
                                                   `#, (hspace 2))) 
                                 (port->lines (open-input-string (syntax->datum #'the-string))))))]))

(define-syntax (multiline-racketinput syn)
  (syntax-case syn ()
    [(_ the-string)
     #`(racketinput 
        #,@(foldr append '()(map (lambda (x) (list `#, (tt ,x)
                                                   `#, (linebreak) 
                                                   `#, (hspace 2))) 
                                 (port->lines (open-input-string (syntax->datum #'the-string))))))]))

(define-syntax (multiline-racketblock-noresult syn)
  (syntax-case syn ()
    [(_ the-string)
     #`(racketblock 
        #,@(foldr append '()(map (lambda (x) (list `#, (tt (literal ,x))
                                                   `#, (linebreak) 
                                                   `#, (hspace 2))) 
                                 (port->lines (open-input-string (syntax->datum #'the-string))))))]))

(define-syntax (inline-result syn)
  (syntax-case syn ()
    [(_ the-string)
     #`(racketresultfont (tt (literal #,(syntax->datum #'the-string))))]))

;(multiline-racketblock "foo\nbar\nbaz")