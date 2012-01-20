; Semi-stateful example due to Paul Anderson
; TN


(Theory pa-state
        (Vocab pa-state               
               (Constants                
                ('mrSecurity User)
                ('mrAdmin User)
                ('mrManager User)
                                
                ('state0 ReqTime)
                ('state1 ReqTime)
                ('state2 ReqTime)
                ('state3 ReqTime)
                ('state4 ReqTime)
                
                ;('state5 ReqTime)
                ;('state6 ReqTime)
                ;('state7 ReqTime)
                ;('state8 ReqTime)
                ;('state9 ReqTime)                
                ;('state10 ReqTime)
                ;('state11 ReqTime)
                ;('state12 ReqTime)
                ;('state13 ReqTime)
                ;('state14 ReqTime)
                ;('state15 ReqTime)
                ;('state16 ReqTime)
                ;('state17 ReqTime)
                ;('state18 ReqTime)
                ;('state19 ReqTime)                
                
                ;('state20 ReqTime)
                ;('state21 ReqTime)
                ;('state22 ReqTime)
                ;('state23 ReqTime)
                ;('state24 ReqTime)
                ;('state25 ReqTime)
                ;('state26 ReqTime)
                ;('state27 ReqTime)
                ;('state28 ReqTime)
                ;('state29 ReqTime)
                
                ('end EndTime)
                
                ('machine4 Server)
                ('machine3 Server)
                
                ('port1234 Port)
                ('port6789 Port))
                
               ; We will still have infinitary sig due to 
               ; nextTime: RT -> T
               ; and abstract T means there is a potential Skolem coercion from T to RT.
               
               (Functions 
                ; Final state has no successor
                (nextTime ReqTime Time)               
                
                ; Final state has no request
                (requestAtTime ReqTime Request)
                
                ; All times have an external webserver and a port for each server
                (externalAtTime Time Server)                                
                (portAtTime Time Server Port)
                                
                ; Each request has a user and a server.
                ; Set port requests have a port, too.
                (requestUser Request User)
                (requestServer Request Server
                               )
                ;(requestPort RSetPort Port)                 
                ; KLUDGE for well-sortedness. See .rkt file.
                (requestPort Request Port)
                )                 
               
               (Predicates                 
                (before Time Time))
               
                (Types                 
                 User                
                 Server                 
                 Port
                 
                 ; Need a stopping-state where the axioms don't force ANOTHER state to exist.
                 (Time > ReqTime EndTime)
                 
                 ; Representing the unique request that appears at each state. 
                 (Request > RSetPort RSetExternal)
                 ))
                                                                     
        (Axioms
         ; TODO Note: No way to say abstract w/o constants... 
         ; (that is, if something has both subsorts and constants, that it's covered by the subsorts alone)
         ; abstract implies both                 
         
         (abstract Request)
         (abstract Port)
         (abstract Server)
         (abstract User)
         (abstract Time)
         (abstract ReqTime)
         (abstract EndTime)
         
         (constants-neq-all ReqTime)
         (constants-neq-all User)
         (constants-neq-all Server)
         (constants-neq-all Port)
         
         ; < time
         ; We have no transitive closure in Margrave, so we cannot define the transitive closure of nextTime. 
         ; Instead, define manually for the 5 states:
         (formula (and (before 'state0 'state1)
                       (before 'state0 'state2)
                       (before 'state0 'state3)
                       (before 'state0 'state4)
                       (before 'state0 'end)
                       (before 'state1 'state2)
                       (before 'state2 'state3)
                       (before 'state3 'state4)
                       (before 'state4 'end)
                       (before 'state2 'state3)
                       (before 'state2 'state4)
                       (before 'state2 'end)                       
                       (before 'state3 'state4)
                       (before 'state3 'end)
                       (before 'state4 'end)))
         
         
         ; Time:
         (formula (= (nextTime 'state0) 'state1))
         (formula (= (nextTime 'state1) 'state2))
         (formula (= (nextTime 'state2) 'state3))                  
         (formula (= (nextTime 'state3) 'state4))
         
        ; (formula (= (nextTime 'state4) 'state5))
        ; (formula (= (nextTime 'state5) 'state6))
        ; (formula (= (nextTime 'state6) 'state7))
        ; (formula (= (nextTime 'state7) 'state8))
        ; (formula (= (nextTime 'state8) 'state9))
        ; (formula (= (nextTime 'state9) 'state10))
        ; (formula (= (nextTime 'state10) 'state11))
        ; (formula (= (nextTime 'state11) 'state12))
        ; (formula (= (nextTime 'state12) 'state13))
        ; (formula (= (nextTime 'state13) 'state14))
        ; (formula (= (nextTime 'state14) 'state15))
        ; (formula (= (nextTime 'state15) 'state16))
        ; (formula (= (nextTime 'state16) 'state17))
        ; (formula (= (nextTime 'state17) 'state18))
        ; (formula (= (nextTime 'state18) 'state19))         
        ; (formula (= (nextTime 'state19) 'state20))
        ; (formula (= (nextTime 'state20) 'state21))
        ; (formula (= (nextTime 'state21) 'state22))
        ; (formula (= (nextTime 'state22) 'state23))
        ; (formula (= (nextTime 'state23) 'state24))
        ; (formula (= (nextTime 'state24) 'state25))
        ; (formula (= (nextTime 'state25) 'state26))
        ; (formula (= (nextTime 'state26) 'state27))
        ; (formula (= (nextTime 'state27) 'state28))
        ; (formula (= (nextTime 'state28) 'state29))
        ; (formula (= (nextTime 'state29) 'end))
         
         ;(formula (= (nextTime 'state19) 'end))
         
         (formula (= (nextTime 'state4) 'end))                           
         )) 
