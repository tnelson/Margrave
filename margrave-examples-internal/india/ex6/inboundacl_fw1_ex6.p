;ACL for firewall facing the outside world

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
                        (= 'fw1dmz interf) (BlacklistedIPs ipdest))
         (Rule2 = (deny interf ipsrc ipdest portsrc portdest pro) :-
                        (= 'fw1ext interf) (BlacklistedIPs ipsrc))
         (Rule3 = (deny interf ipsrc ipdest portsrc portdest pro) :-
                        (= 'fw1dmz interf) (= 'port23 portdest))



         (Rule4 = (accept interf ipsrc ipdest portsrc portdest pro) :-
                (= 'fw1ext interf) (= 'mailserver ipdest) (= 'port25 portdest) (= 'tcp pro))

         (Rule5 = (accept interf ipsrc ipdest portsrc portdest pro) :-
                (= 'fw1ext interf) (= 'webserver ipdest) (= 'port80 portdest) (= 'tcp pro))


         (Rule6 = (accept interf ipsrc ipdest portsrc portdest pro) :-
                (= 'fw1dmz interf) (= 'fw2static_mgr ipsrc) (= 'port80 portdest) (= 'outsideips ipdest) (= 'tcp pro))
         
         (Rule7 = (deny interf ipsrc ipdest portsrc portdest pro) :-
                true) )

        ; Firewall policy: first rule applicable takes effect.
        (RComb (fa accept deny)))

