(Policy uses conference
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
  	  (papernoconflict = (permit s a r) :- (and (not (conflicted s r)) (ReadPaper a) (Paper r)))
	  (paperassigned = (permit s a r) :- (and (assigned s r) (ReadPaper a) (Paper r)))
	  (paperconflict = (deny s a r) :- (and (conflicted s r) (ReadPaper a) (Paper r))))
        (RComb (fa permit deny)))

