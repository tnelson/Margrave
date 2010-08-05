(Policy SampleConfigPolicy uses SampleConfig
        (Target )
        (Rules 
	;  (RuleTomcatInstalled = (Valid c) :- (!Tomcat c))
	;  (RuleTomcatNotInstalled = (Valid c) :- (Tomcat c) (Tomcator1 c))
	  (RuleDefault = (Valid c) :- true)
        )
        (RComb FAC)
        (PComb FAC)
	(Children ))
