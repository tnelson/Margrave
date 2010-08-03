; NAT performed by inside firewall

(Policy InboundNAT_FW2 uses TalkFirewallPolicy
	(Target )
	(Rules
         
         ; Static NAT on outgoing traffic (crossing into DMZ from internal network)
         (Rule1 = (Translate interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw2int interf) (fw2static tempnatip))
                 
        ; This rule exists to make sure that ONLY rule 1 is applied to fw2int,
        ; and can never fall through to RuleX.
         (Rule2 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw2int interf ))

 
         ; Otherwise, NAT has no effect
         (RuleX = (Translate interf ipsrc ipdest portsrc portdest pro tempnatip) :- 
                (= ipsrc tempnatip)) )
        
        ; Firewall policy: first rule applicable takes effect.
	(RComb FAC)
	(PComb FAC)

        (Children ))
