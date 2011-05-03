(Policy uses AbadiBinder
        (Variables 
         (Variable p Person)
         (Variable d Door))
        (Rules                     
          (CharliePermitsEveryone = (canOpen p d) :- true)))

