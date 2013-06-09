(Policy uses Conference
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
  	  (PaperNoConflict = (permit s a r) :- (and (not (conflicted s r)) (ReadPaper a) (Paper r)))
	  (PaperAssigned = (permit s a r) :- (and (assigned s r) (ReadPaper a) (Paper r)))
	  (PaperConflict = (deny s a r) :- (and (conflicted s r) (ReadPaper a) (Paper r))))
        (RComb (fa permit deny)))

