(Policy uses conference
        (Variables 
         (s Subject)
         (a Action)
         (r Resource))
        (Rules 
          (tier1rule = (permit s a r) :- (trustworthy s a r) (competent s a r))
  	  (tier2rule1 = (trustworthy s a r) :- (Paper r) (not (conflicted s r)) (ReadPaper a))
          (tier2rule2 = (competent s a r) :- (Paper r) (ReadPaper a) (reviewer s))          
          (otherwise = (deny s a r) :- true))
        (RComb (fa permit deny)))

