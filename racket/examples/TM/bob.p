(Policy uses AbadiBinder
        (Variables 
         (Variable p Person)
         (Variable d Door))
        (Rules                     
          (BobTrustsCharlie = (canOpen p d) :- (Charlie.canOpen p d))))

