(Policy uses AbadiBinder
        (Variables 
         (Variable p Person)
         (Variable d Door))
        (Rules                     
          (CharliePermitsAdmins = (canOpen p d) :- (admin p) (Door d))
          (CharlieAdminsAreManagers = (admin p) :- (Manager p)))) 

