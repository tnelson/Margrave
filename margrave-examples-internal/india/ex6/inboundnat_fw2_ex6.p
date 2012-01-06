; NAT performed by inside firewall

(Policy uses talkfirewallpolicy6
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
                (= 'fw2int interf) (= 'managerpc ipsrc) (= 'fw2static_mgr newip))
                 
         (Rule2 = (translate interf ipsrc ipdest portsrc portdest pro newip) :-
                (= 'fw2int interf) (= 'contractorpc ipsrc) (= 'fw2static_con newip))
         (Rule3 = (translate interf ipsrc ipdest portsrc portdest pro newip) :-
                (= 'fw2int interf) (= 'employeepc ipsrc) (= 'fw2static_emp newip))



        ; This rule exists to make sure that ONLY the above rules applied to fw2int,
        ; and can never fall through to RuleX.
         (Rule4 = (deny interf ipsrc ipdest portsrc portdest pro newip) :-
                (= 'fw2int interf ))

 
         ; Otherwise, NAT has no effect
         (RuleX = (translate interf ipsrc ipdest portsrc portdest pro newip) :- 
                (= ipsrc newip)) )
        
        ; Firewall policy: first rule applicable takes effect.
	(RComb (fa translate deny)))
