(Policy InboundNAT_FW1 uses TalkFirewallPolicy
	(Target )
	(Rules
                           
         ; No NAT performed by outside firewall.
         (RuleX = (Translate interf ipsrc ipdest portsrc portdest pro tempnatip) :- 
                (= ipsrc tempnatip)) )
        
        ; Firewall policy: first rule applicable takes effect.
	(RComb FAC)
	(PComb FAC)

        (Children ))
