(Policy badpol-missing-types uses badvoc-missing-types
        (Target )

; Note the use of different variable names across rules. This is temporarily required. 

        (Rules 
  	  (TollFreeCall = (TollFree src dest) :- (GetExchange src e1) (GetExchange dest e1))
	  (TollCall = (Toll src dest) :- (GetExchange src e2) (GetExchange dest e3) (!= e2 e3))
	  (RefuseCall1 = (Refuse src dest) :- (OutOfService src))
          (RefuseCall2 = (Refuse src dest) :- (OutOfService dest))
          (RefuseCall3 = (Refuse src dest) :- (= src dest))
        )
        (RComb O Refuse Toll TollFree)
        (PComb O Refuse Toll TollFree)
	(Children ))

