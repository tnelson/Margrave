(Policy AWFW uses FirewallPolicy
	(Target )
	(Rules

         ; Is there an active connection?
         (ConnectionStatefulFirewall1 = (Accept ipsrc ipdest portsrc portdest) :- (Connection ipsrc ipdest portsrc portdest))
         (ConnectionStatefulFirewall2 = (Accept ipsrc ipdest portsrc portdest) :- (Connection ipdest ipsrc portdest portsrc))

         
         ; Service -> ports mapping depends on definitions in the firewall.
         ; For instance, in MWZ paper, the security problem is caused by a misdefinition of the nntp_services service. 
        
         (Rule1 = (Accept ipsrc ipdest portsrc portdest) :- (Zoonet ipsrc) (one ipdest) (https portdest))
         (Rule2 = (Accept ipsrc ipdest portsrc portdest) :- (Zoonet ipsrc) (one ipdest) (ssh portdest))

         ; Is "all_tcp" any traffic?
         ; May need TCP vs. UDP in the vocab to be correct...
         (Rule3 = (Accept ipsrc ipdest portsrc portdest) :- (one ipsrc))
         
         ; Def of "traceroute" service?
         (Rule4 = (Accept ipsrc ipdest portsrc portdest) :- (one ipsrc) (traceroute portdest))
         
         ; Due to def. of service in paper...
         (Rule5a = (Accept ipsrc ipdest portsrc portdest) :- (two ipdest) (nntp portdest))
         (Rule5b = (Accept ipsrc ipdest portsrc portdest) :- (two ipdest) (nntp portsrc))
         
         ; This rule always applies
         (Rule6 = (Drop ipsrc ipdest portsrc portdest) :- (IPAddress ipsrc))
                )
                
	(RComb FAC)
	(PComb FAC)
	(Children ))
