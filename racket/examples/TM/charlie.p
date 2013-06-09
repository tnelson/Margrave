(Policy uses AbadiBinder
        (Variables 
         (p Person)
         (d Door))
        (Rules                     
          (CharliePermitsAdmins = (canOpen p d) :- (admin p) (Door d))
          (CharlieAdminsAreManagers = (admin p) :- (Manager p)))) 

