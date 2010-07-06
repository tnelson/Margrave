(Policy ConferencePolicy1 uses conferencepolicy
        (Target )
        (Rules 
  	  (PaperNoConflict = (Permit s a r) :- (!Conflicted s r) (ReadPaper a) (Paper r))
	  (PaperAssigned = (Permit s a r) :- (Assigned s r) (ReadPaper a) (Paper r))
	  (PaperConflict = (Deny s a r) :- (Conflicted s r) (ReadPaper a) (Paper r))
        )
        (RComb FAC)
        (PComb FAC)
	(Children ))

