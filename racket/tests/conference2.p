(Policy uses conferencenoconstant
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
  	  (paperNoConflict = (permit s a r) :- (Paper r) (not (conflicted s r)) (ReadPaper a))
	  (paperAssigned = (permit s a r) :- (Paper r) (assigned s r) (ReadPaper a))
	  (paperConflict = (deny s a r) :- (Paper r) (conflicted s r) (ReadPaper a)))
        (RComb (fa permit deny)))

