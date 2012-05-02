#lang racket

(require "../margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../")
(m-load-policy "mypol1" "conference1.p")
(m-load-policy "mypol2" "conference2.p")

;(define (string-contains? str phrase)
;  (cond [(< (string-length str) (string-length phrase)) false]
;        [else (or (equal? (substring str 0 (string-length phrase)) phrase)
;                  (string-contains? (substring str 1) phrase))]))

(define (exn-contains-message msg)
  (lambda (e) (and (exn? e) 
                   (string-contains? (exn-message e) msg))))


; error! "r" used as the wrong sort.
; The formula '((mypol1 permit) s r r) was not well-sorted. The term 'r was of type 'Resource, but expected to be of type 'Action: r.
(check-exn 
 (exn-contains-message "was not well-sorted. The term 'r was of type 'Resource") 
 (lambda () (m-let "Qx1" '([s Subject] [a Action] [r Resource]) 
                   '(and ([mypol1 permit] s r r)))))

; Error! NOTAVALIDSORT isn't a valid sort
; Unknown type: 'NOTAVALIDSORT. Valid types were:

(check-exn 
 (exn-contains-message "Unknown type: 'NOTAVALIDSORT. Valid types were:")
 (lambda () (m-let "Qx1" '([s Subject] [a Action] [r Resource]) 
                   '(and ([mypol1 permit] s a r) (NOTAVALIDSORT a)))))