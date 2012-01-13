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
                (requestServer Request Server)
                ;(requestPort RSetPort Port) ; made all Requests instead of just RSetPort for well-sortedness
                (requestPort Request Port)
                )                 
               
                (Types                 
                 User                
                 Server                 
                 Port
                 
                 ; Need a stopping-state where the axioms don't force ANOTHER state to exist.
                 (Time > ReqTime EndTime)
                 
                 ; Need a "request", representing the unique request 
                 ; appearing at each state. 
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
         
         ; Time:
         (formula (= (nextTime 'state0) 'state1))
         (formula (= (nextTime 'state1) 'state2))
         (formula (= (nextTime 'state2) 'state3))
         (formula (= (nextTime 'state3) 'state4))
         (formula (= (nextTime 'state4) 'end))                           
         
         ; Additional axioms go in the query!
         )) 
