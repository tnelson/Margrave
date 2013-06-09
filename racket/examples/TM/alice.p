(Policy uses AbadiBinder
        (Variables 
         (p Person)
         (d Door))
        (Rules    
          ; Propagation of context
          (AliceTrustsBob = (canOpen p d) :- (Bob.canOpen p d))
          
          ; Charlie < Bob < Alice
          ; Charlie has a local dependency on IDBs, check that Alice can have one too.
          (AliceProfessor = (isProfessor p) :- (WPI.isProfessor p))
          (AliceProfsCanOpen = (canOpen p d) :- (isProfessor p) (Door d))))

