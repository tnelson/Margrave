(Policy uses AbadiBinder
        (Variables 
         (Variable p Person)
         (Variable d Door))
        (Rules                     
          (AliceTrustsBob = (canOpen p d) :- (Bob.canOpen p d))))

