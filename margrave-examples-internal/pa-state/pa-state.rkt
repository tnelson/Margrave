; Semi-stateful example due to Paul Anderson
; TN

#lang racket

(require "../../margrave/margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../../margrave")

(m-load-policy "pa-state" "pa-state.p")

; If the request at time t is permitted, 
; then the next state reflects the action.
; Otherwise, nothing changes.
(m-let "StateStuff" '()       
         '(forall t ReqTime (and 
                             
                             ;; TODO: what happens if request is DENIED?
                             
                             ; Permitted RSetPort
                             (implies (and ([pa-state permit] (requestAtTime t))
                                           (RSetPort (requestAtTime t)))
                                      (= (portAtTime (nextTime t) 
                                                     (requestServer (requestAtTime t)))
                                         (requestPort (requestAtTime t))))
                             
                             ; RSetPort won't change external
                             ; and
                             ; all other server's ports remain unchanged.
                             (implies (RSetPort (requestAtTime t))
                                       (and (= (externalAtTime (nextTime t))
                                               (externalAtTime t))
                                            (forall s Server 
                                                    (implies (not (= s (requestServer (requestAtTime t))))
                                                             (= (portAtTime (nextTime t) s)
                                                                (portAtTime t s))))))

                             ; RSetExternal won't change port for ANY server
                             (implies (RSetExternal (requestAtTime t))
                                      (forall s Server 
                                              (= (portAtTime (nextTime t) s)
                                                 (portAtTime t s))))

                             ; Permitted RSetExternal
                             (implies  (and ([pa-state permit] (requestAtTime t))
                                            (RSetExternal (requestAtTime t)))
                                       (= (externalAtTime (nextTime t))
                                          (requestServer (requestAtTime t))))))
         #:under '("pa-state"))
         
; ^^^ !!! TODO: Not well-sorted!
; since the isa's scope doesn't extend to the consequent. May need to change.
; KLUDGE: make requestPort take any request.
                  

(m-let "Q1" '()
       '(and ([StateStuff] )
             ([pa-state permit] (requestAtTime 'state0))
             (not (= (portAtTime 'state0 'machine3)
                     (portAtTime 'state1 'machine3))))
       #:under '("pa-state")
       #:ceiling '([Server 2]
                   [Time 6]
                   [Request 5]
                   [Port 2]
                   [User 3]
                   [univ 18]))
(m-get "Q1")
(display (m-show "Q1" #:include '(([pa-state permit] (requestAtTime 'state0)))))

;(m-is-poss? "Q1")

; To move to test cases: forbid this use of isa.
;(m-let "Q1" '([req Request])
;       '(and ([pa-state permit] req)
;             (isa (nextTime (requestTime req)) 
;                  Time
;                  ([pa-state permit] req))))

