(Policy uses partialfunction
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
  	  (paperNoConflict = (permit s a r) :- (isa r Paper (and (not (conflicted s r)) (ReadPaper a))))
	  (paperAssigned = (permit s a r) :- (isa r Paper (and (assigned s r) (ReadPaper a))))
	  (paperConflict = (deny s a r) :- (isa r Paper (and (conflicted s r) (ReadPaper a)))))
        (RComb (fa permit deny)))

