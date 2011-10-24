(Policy uses Conference
        (Variables 
         (Variable s Subject)
         (Variable a Action)
         (Variable r Resource))
        (Rules 
  	  (PaperNoConflict = (permit s a r) :- (and (not (conflicted s r)) (readPaper a) (paper r)))
	  (PaperAssigned = (permit s a r) :- (and (assigned s r) (readPaper a) (paper r)))
	  (PaperConflict = (deny s a r) :- (and (conflicted s r) (readPaper a) (paper r))))
        (RComb (fa permit deny)))

