; Semi-stateful example due to Paul Anderson
; TN

#lang racket

(require "../../margrave/margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../../margrave")

(m-load-policy "pa-state" "pa-state.p")

(m-let "Q1" '([req Request])
       '([pa-state permit] req)
       #:ceiling '([Server 2]
                    [Time 5]
                    [Request 5]
                    [Port 2]
                    [User 3]
                    [univ 20]))
(m-get "Q1")

; To move to test cases: forbid this use of isa.
;(m-let "Q1" '([req Request])
;       '(and ([pa-state permit] req)
;             (isa (nextTime (requestTime req)) 
;                  Time
;                  ([pa-state permit] req))))

