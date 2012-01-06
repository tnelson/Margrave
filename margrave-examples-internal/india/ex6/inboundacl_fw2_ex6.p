; ACL for the firewall facing the internal network



(Policy uses talkfirewallpolicy6
        (Variables
         (interf Interface)
         (ipsrc IPAddress)
         (ipdest IPAddress)
         (portsrc Port)
         (portdest Port)
         (pro Protocol))
        (Rules

         
         (Rule1 = (deny interf ipsrc ipdest portsrc portdest pro) :-
                (= 'fw2dmz interf))

         (Rule2 = (deny interf ipsrc ipdest portsrc portdest pro ) :-
                (= 'fw2int interf) (= 'mailserver ipdest) (= 'port25 portdest) (= 'tcp pro) (= 'contractorpc ipsrc))

         (Rule3 = (accept interf ipsrc ipdest portsrc portdest pro ) :-         
                (= 'fw2int interf) (= 'mailserver ipdest) (= 'port25 portdest) (= 'tcp pro))



         (Rule4 = (accept interf ipsrc ipdest portsrc portdest pro ) :-
                (= 'fw2int interf) (= 'port80 portdest) (= 'tcp pro))

         (Rule5 = (deny interf ipsrc ipdest portsrc portdest pro ) :-
                true) )

        ; Firewall policy: first rule applicable takes effect.
        (RComb (fa accept deny)))

