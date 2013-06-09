(Policy uses AbadiBinder
        (Variables 
         (p Person)
         (d Door))
        (Rules                     
          (BobTrustsCharlie = (canOpen p d) :- (Charlie.canOpen p d))))

