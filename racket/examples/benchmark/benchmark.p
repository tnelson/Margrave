(Policy uses BenchT
        (Variables 
         (Variable x1 T1)
         (Variable x2 T2)
         (Variable x3 T3)
         (Variable x4 T4))
        (Rules 
  	  (R1 = (permit x1 x2 x3 x4) :- (p1 x1 x2 x3 x4) (p2 x1 x2 x3 x4))
	  (R2 = (permit x1 x2 x3 x4) :- (p3 x1 x2 x3 x4) (not (p4 x1 x2 x3 x4)))
          
	  (RLastResort = (deny x1 x2 x3 x4) :- true))
        (RComb (fa permit deny)))
