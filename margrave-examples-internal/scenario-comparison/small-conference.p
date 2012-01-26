(Policy uses small-conference
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
  	  (paperNoConflict = (permit s a r) :- (and (not (conflicted s r)) (ReadPaper a) (Paper r)))
	  (paperAssigned = (permit s a r) :- (and (assigned s r) (ReadPaper a) (Paper r)))
	  (paperConflict = (deny s a r) :- (and (conflicted s r) (ReadPaper a) (Paper r))))
        (RComb (fa permit deny)))

