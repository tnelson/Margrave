#lang racket

(require "../../margrave.rkt")

;(start-margrave-engine #:margrave-params '("-log")
;                       #:margrave-path "F:\\msysgit\\git\\margrave\\margrave"
;                       #:jvm-params '("-Xmx512m"))
(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "M:\\RktMargrave\\margrave"
                       #:jvm-params '("-Xmx512m"))

;(m-load-policy "mypol" "F:\\msysgit\\git\\Margrave\\margrave\\examples\\benchmark\\benchmark.p")
(m-load-policy "mypol" "M:\\RktMargrave\\margrave\\examples\\benchmark\\benchmark.p")

(time (m-let "Q" '([x1 T1] [x2 T2] [x3 T3] [x4 T4]) 
                   '([mypol permit] x1 x2 x3 x4)))   
(time (m-is-poss? "Q"))

; Those calls have the overhead of building all the formulas, etc.
; Also note: cpu time will be low since the cpu time is spent in a different process (java).

(define num-iterations 20)
(define timers
  (for/list ([i (build-list num-iterations add1)])
    (define-values (result1 t1a t1b t1c) (time-apply m-let 
                                                     (list (string-append "Q" (number->string i))
                                                           '([x1 T1] [x2 T2] [x3 T3] [x4 T4]) 
                                                           '(and ([mypol permit] x1 x2 x3 x4)))))
    (define-values (result2 t2a t2b t2c) (time-apply m-is-poss? (list (string-append "Q" (number->string i)))))
    (list t1b t2b)))

(define let-timers (map first timers))
(define poss-timers (map second timers))
(define let-time-avg (/ (apply + let-timers) num-iterations))
(define poss-time-avg (/ (apply + poss-timers) num-iterations))

let-time-avg
poss-time-avg
