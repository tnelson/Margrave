; ACL for the firewall facing the internal network

;1 -> DENY if: interface=fw2_dmz
;2 -> DENY if: interface=fw2_internal, ipdest=mailserver,
;              portdest=25, protocol=TCP, ipsrc in contractorPCs
;3 -> ACCEPT if: interface=fw2_internal, ipdest=mailserver, 
;              portdest=25, protocol=tcp  
;4 -> ACCEPT if: interface=fw2_internal, portdest=80, protocol=tcp
;5 -> otherwise, DENY


(Policy InboundACL_FW2 uses TalkFirewallPolicy
        (Target )
        (Rules

         
         (Rule1 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw2dmz interf))

         (Rule2 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw2int interf) (mailserver ipdest) (port25 portdest) (tcp pro) (contractorpc ipsrc))

         (Rule3 = (Accept interf ipsrc ipdest portsrc portdest pro tempnatip) :-         
                (fw2int interf) (mailserver ipdest) (port25 portdest) (tcp pro))




         (Rule4 = (Accept interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw2int interf) (port80 portdest) (tcp pro))

         (Rule5 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                true) )

        ; Firewall policy: first rule applicable takes effect.
        (RComb FAC)
        (PComb FAC)

        (Children ))

