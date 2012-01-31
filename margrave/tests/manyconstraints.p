(Policy uses manyconstraints
        (Variables          
         (x Object))
        (Rules 
	  (rule1 = (permit x) :- (Emu x))
	  (rule2 = (deny x) :- (Computer x)))
          ; policy is not total!
        (RComb (fa permit deny)))

