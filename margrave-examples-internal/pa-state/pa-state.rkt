; Semi-stateful example due to Paul Anderson
; TN

#lang racket

(require "../../margrave/margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../../margrave")

(m-load-policy "pa-state" "pa-state.p")

; Framing Conditions:
(m-let "Framing" '() 
       '(forall t ReqTime (and 
                           
                           ; If a request is denied, nothing changes from tick to tick.
                           (implies ([pa-state deny]  t) 
                                    (and 
                                     (= (externalAtTime (nextTime t))
                                        (externalAtTime t))
                                     (forall s Server 
                                             (= (portAtTime (nextTime t) s)
                                                (portAtTime t s)))))
                           
                           
                           
                           ; Permitted RSetPort sets the port
                           (implies (and ([pa-state permit] t)
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
                           
                           ; Permitted RSetExternal sets the external webserver
                           (implies  (and ([pa-state permit] t)
                                          (RSetExternal (requestAtTime t)))
                                     (= (externalAtTime (nextTime t))
                                        (requestServer (requestAtTime t))))))
       #:under '("pa-state"))

; The above is not well-sorted if requestPort: RSetPort -> Port.
; For now, kludge and say requestPort: Request -> Port.
; In future, should be able to say (isa (requestAtTime t) RSetPort fmla)
; = (forsome freshvar RSetPort (and (= freshvar (requestAtTime t)) fmla/[(requestAtTime t) -> freshvar]))
; TODO ^^^


; TODO: move to test cases --- check for term use like this in query
; term types must propagate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; State0's request is permitted.
; The external webserver is machine3 at state0.
; The request changes the port on machine3.
(m-let "Q1" '()
       '(and ([Framing] )
             ([pa-state permit] 'state0)
             (not (= (portAtTime 'state0 'machine3)
                     (portAtTime 'state1 'machine3)))
             (= (externalAtTime 'state0) 'machine3))       
       #:ceiling '([Server 2]
                   [Time 6]
                   [Request 5]
                   ;[Time 21]
                   ;[Request 20]
                   ;[Time 31]
                   ;[Request 30]
                   [Port 2]
                   [User 3]
                   [univ 18]
                   ;[univ 48]
                   ;[univ 58]
                   ))
(time (check-true (m-is-poss? "Q1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Can the wrong person set the external web-server port?
(m-let "Q2" '()
       '(and ([Framing] )
             ([pa-state permit] 'state0)
             (not (= (portAtTime 'state0 'machine3)
                     (portAtTime 'state1 'machine3)))
             (= (externalAtTime 'state0) 'machine3)
             (= 'mrAdmin (requestUser (requestAtTime 'state0))))      
       #:ceiling '([Server 2]
                   [Time 6]
                   [Request 5]
                   ;[Time 21]
                   ;[Request 20]
                   ;[Time 31]
                   ;[Request 30]
                   [Port 2]
                   [User 3]
                   [univ 18]
                   ;[univ 48]
                   ;[univ 58]
                   ))
(time (check-false (m-is-poss? "Q2")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Paul Anderson's basic example: 2 users colluding to do something that
; "should be" the domain of another person. Sequence:

(m-let "PA1" '()
       '(and ([Framing] )
             ; 1. M says: machine 4 is the external web server
             (RSetExternal (requestAtTime 'state0))
             (= 'mrManager (requestUser (requestAtTime 'state0)))
             (= 'machine4 (requestServer (requestAtTime 'state0)))
             ; 2. S says: machine 4 port is 1234
             (RSetPort (requestAtTime 'state1))
             (= 'mrSecurity (requestUser (requestAtTime 'state1)))
             (= 'machine4 (requestServer (requestAtTime 'state1)))
             (= 'port1234 (requestPort (requestAtTime 'state1)))
             ; 3. A says: machine 3 port is 6789
             (RSetPort (requestAtTime 'state2))
             (= 'mrAdmin (requestUser (requestAtTime 'state2)))
             (= 'machine3 (requestServer (requestAtTime 'state2)))
             (= 'port6789 (requestPort (requestAtTime 'state2)))
             ; 4. M says: machine 3 is the external web server
             (RSetExternal (requestAtTime 'state3))
             (= 'mrManager (requestUser (requestAtTime 'state3)))
             (= 'machine3 (requestServer (requestAtTime 'state3)))             
             
             )       
       #:ceiling '([Server 2]
                   [Time 6]
                   [Request 5]
                   [Port 2]
                   [User 3]
                   [univ 18]))
(time (check-true (m-is-poss? "PA1")))

; > Hmm. Each sequential change validates, but the final result means that we are running
; > a web server with a port which was not approved by Mr Security.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Paul Anderson's second basic example. Same as above except for state2:
; 3. A says: machine 3 port is 1234
(m-let "PA2" '()
       '(and ([Framing] )
             ; 1. M says: machine 4 is the external web server
             (RSetExternal (requestAtTime 'state0))
             (= 'mrManager (requestUser (requestAtTime 'state0)))
             (= 'machine4 (requestServer (requestAtTime 'state0)))
             ; 2. S says: machine 4 port is 1234
             (RSetPort (requestAtTime 'state1))
             (= 'mrSecurity (requestUser (requestAtTime 'state1)))
             (= 'machine4 (requestServer (requestAtTime 'state1)))
             (= 'port1234 (requestPort (requestAtTime 'state1)))
             ; 3. A says: machine 3 port is 1234
             (RSetPort (requestAtTime 'state2))
             (= 'mrAdmin (requestUser (requestAtTime 'state2)))
             (= 'machine3 (requestServer (requestAtTime 'state2)))
             (= 'port1234 (requestPort (requestAtTime 'state2)))
             ; 4. M says: machine 3 is the external web server
             (RSetExternal (requestAtTime 'state3))
             (= 'mrManager (requestUser (requestAtTime 'state3)))
             (= 'machine3 (requestServer (requestAtTime 'state3)))             
             
             )       
       #:ceiling '([Server 2]
                   [Time 6]
                   [Request 5]
                   [Port 2]
                   [User 3]
                   [univ 18]))
(time (check-true (m-is-poss? "PA2")))
; > Hmmm. So the new web server (3) would have the "right" port number
; > to satisfy Mr Security, so we might reasonably allow this to happen.
; > But now who is "responsible" for this value? 

; > Mr Admin originally specified it, so maybe he should be "responsible", but he
; > is no longer allowed to change it if he decides it is no longer appropriate.
; > Mr Security "approved" it in some sense, but he never set it explicitly for this
; > machine.