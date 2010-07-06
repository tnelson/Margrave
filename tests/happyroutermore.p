;ip access-list extended more-secure
;remark allow web server to respond to all hosts on 10 net
;permit tcp host 192.168.1.200 eq www 10.1.1.0 0.0.0.255
;permit tcp host 192.168.1.200 eq 443 10.1.1.0 0.0.0.255
;
;remark allow PC full access to make requests to less secure network
;permit tcp host 192.168.1.201 10.1.1.0 0.0.0.255

(Policy HappyRouterMore uses happyrouteracl
	(Target )

	(Rules

         ; Allow responses from internal webserver to external network
         (Rule1 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipdest) (webserver ipsrc) (http portsrc) (tcp pro))
         (Rule2 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipdest) (webserver ipsrc) (https portsrc) (tcp pro))
 
         ; Allow full access from PC to external network.
         (Rule3 = (Accept ipsrc ipdest portsrc portdest pro) :- (10network ipdest) (pc ipsrc) (tcp pro))

         (RuleX = (Drop ipsrc ipdest portsrc portdest pro) :- true)
                )
        
	(RComb FAC)
	(PComb FAC)

	(Children ))

