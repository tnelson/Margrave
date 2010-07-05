(Policy IOut uses IOutACL 
	(Target )

	(Rules

         (RuleFTP = (Accept ipsrc ipdest portsrc portdest pro) :- (ftpclient ipsrc) (ftpserver ipdest) (port21 portdest) (tcp pro))

         (RuleX = (Drop ipsrc ipdest portsrc portdest pro) :- true)
                )
        
	(RComb FAC)
	(PComb FAC)

	(Children ))

