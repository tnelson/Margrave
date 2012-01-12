; Semi-stateful example due to Paul Anderson
; TN


(Theory pa-state
        (Vocab pa-state
               
               (Constants                
                ('mrSecurity User)
                ('mrAdmin User)
                ('mrManager User)
                                
                ('initialState Time)
                
                ('machine4 Server)
                ('machine3 Server)
                
                ('port1234 Port)
                ('port6789 Port))
                
               ; Since we have a successor function, we lose completeness.
               (Functions 
                (nextTime Time Time)
                
                (externalAtTime Time Server)                
                (requestAtTime Time Request)
                (portAtTime Time Server Port)
                                
                (requestUser Request User)
                (requestServer Request Server)
                (requestPort RSetPort Port))
                               
                (Types
                 
                 User                
                 Server
                 Time
                 Port
                 
                 ; Need a "request", representing the unique request 
                 ; appearing at each state. 
                 (Request > RSetPort RSetExternal)))
                                                                     
        (Axioms
         (abstract Request)
         (abstract Port)
         (abstract Server)
         (abstract User)
         
         ; The initial state is initial.
         (formula (forall t Time (not (= 'initialState (nextTime t)))))
         
         ; If a 'setExternal action is permitted at time t, the next state respects the action.
         (formula (forall t Time (and 
                                  (implies (RSetPort (requestAtTime t))
                                           (= (portAtTime (nextTime t) (requestServer (requestAtTime t)))
                                              (requestPort (requestAtTime t))))
                                  (implies (RSetExternal (requestAtTime t))
                                           (= (externalAtTime (nextTime t))
                                              (requestServer (requestAtTime t)))))))
         
         ; ^^^ !!! TODO: This may not be well-sorted...
         ; since the isa's scope doesn't extend to the consequent. May need to change.
         
         ; ^^^ !!! TODO: (isa t Type) only takes variables t. Need to expand to take terms.
         
         )) 
