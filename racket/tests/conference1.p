(Policy uses conference
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
  	  (papernoconflict = (permit s a r) :- (isa r Paper (and (not (conflicted s r)) (ReadPaper a))))
	  (paperassigned = (permit s a r) :- (isa r Paper (and (assigned s r) (ReadPaper a))))
	  (paperconflict = (deny s a r) :-  (isa r Paper (and (conflicted s r) (ReadPaper a)))))
        (RComb (fa permit deny)))

