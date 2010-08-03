;ACL for firewall facing the outside world
; AFTER example 4 (the first bug "fix")

;1 -> DENY if: interface=fw1_dmz, ipdest in blacklisted_ips
;2 -> DENY if: interface=fw1_external, ipsrc in blacklisted_ips
;3 -> DENY if: interface=fw1_dmz, portdest=23
;4 -> ACCEPT if: interface=fw1_external, ipdest=mailserver, 
;              portdest=25, protocol=tcp  
;5 -> ACCEPT if: interface=fw1_external, ipdest=webserver, 
;              portdest=80, protocol=tcp  
;6 -> ACCEPT if: interface=fw1_dmz, ipdest=any outside, portdest=80,
;              protocol=tcp, ipsrc=fw2static
;7 -> otherwise, DENY

(Policy InboundACL_FW1_New uses TalkFirewallPolicy
        (Target )
        (Rules
        
         (Rule1 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                        (fw1dmz interf) (blacklistedip ipdest))
         (Rule2 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                        (fw1ext interf) (blacklistedip ipsrc))
         (Rule3 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                        (fw1dmz interf) (port23 portdest))



         (Rule4 = (Accept interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw1ext interf) (mailserver ipdest) (port25 portdest) (tcp pro))

         (Rule5 = (Accept interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw1ext interf) (webserver ipdest) (port80 portdest) (tcp pro))

         ; bug "fix"
         ; employeePC packet will already have been subjected to NAT by FW2.
         (Rule6 = (Accept interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                (fw1dmz interf) (fw2static ipsrc) (port80 portdest) (outsideips ipdest) (tcp pro))
         
         (Rule7 = (Deny interf ipsrc ipdest portsrc portdest pro tempnatip) :-
                true) )

        ; Firewall policy: first rule applicable takes effect.
        (RComb FAC)
        (PComb FAC)

        (Children ))

