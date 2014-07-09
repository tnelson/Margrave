(Policy uses conference
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
         (papernoconflict = (permit s a r) :- (Paper r) (not (conflicted s r)) (ReadPaper a))
         (paperassigned = (permit s a r) :- (Paper r) (assigned s r) (ReadPaper a))	  
         (catchall = (deny s a r) :- true))
        (RComb (fa permit deny)))

