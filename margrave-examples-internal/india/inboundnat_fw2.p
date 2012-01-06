; NAT performed by inside firewall

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
         
         ; Static NAT on outgoing traffic (crossing into DMZ from internal network)
         (Rule1 = (translate interf ipsrc ipdest portsrc portdest pro newip) :-
                (= 'fw2int interf) (= 'fw2static newip))
                 
        ; This rule exists to make sure that ONLY rule 1 is applied to fw2int,
        ; and can never fall through to RuleX.
         (Rule2 = (deny interf ipsrc ipdest portsrc portdest pro newip) :-
                (= 'fw2int interf))
 
         ; Otherwise, NAT has no effect
         (RuleX = (translate interf ipsrc ipdest portsrc portdest pro newip) :- 
                (= ipsrc newip)) )
        
	(RComb (fa translate deny)))
