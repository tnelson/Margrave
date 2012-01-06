(Policy uses talkfirewallpolicy
        (Variables
         (interf Interface)
         (ipsrc IPAddress)
         (ipdest IPAddress)
         (portsrc Port)
         (portdest Port)
         (pro Protocol)
         (newip IPAddress))
	(Rules
                           
         ; No NAT performed by outside firewall.
         (RuleX = (translate interf ipsrc ipdest portsrc portdest pro newip) :- 
                (= ipsrc newip)) )
        
        ; Firewall policy: first rule applicable takes effect.
	(RComb (fa translate deny)))
