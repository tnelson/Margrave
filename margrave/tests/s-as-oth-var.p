(Policy s-as-oth-varp uses s-as-oth-var
        (Target )
        (Rules 
  	  (PaperNoConflict = (Permit x y z) :- (!Conflicted x z) (ReadPaper y) (Paper z))
	  (PaperAssigned = (Permit x y z) :- (Assigned x z) (ReadPaper y) (Paper z))
	  (PaperConflict = (Deny x y z) :- (Conflicted x z) (ReadPaper y) (Paper z))
        )
        (RComb FAC)
        (PComb FAC)
	(Children ))

