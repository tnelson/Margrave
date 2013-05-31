; ACL for the firewall facing the internal network
; AFTER second bug-fix (~ex 5)

;1 -> DENY if: interface=fw2_dmz
;2 -> DENY if: interface=fw2_internal, ipdest=mailserver,
;              portdest=25, protocol=TCP, ipsrc in contractorPCs
;3 -> ACCEPT if: interface=fw2_internal, ipdest=mailserver, 
;              portdest=25, protocol=tcp  
;4 -> ACCEPT if: interface=fw2_internal, portdest=80, protocol=tcp,
;                **ipsrc=managerPC**
;** 5 -> ACCEPT if: interface=fw2_internal, ipdest=webserver,
;               portdest=80, protocol=tcp**  
;6 -> otherwise, DENY

(Policy uses talkfirewallpolicy
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

         (Rule2 = (deny interf ipsrc ipdest portsrc portdest pro) :-
                (= 'fw2int interf) (= 'mailserver ipdest) (= 'port25 portdest) (= 'tcp pro) (= 'contractorpc ipsrc))

         (Rule3 = (accept interf ipsrc ipdest portsrc portdest pro) :-         
                (= 'fw2int interf) (= 'mailserver ipdest) (= 'port25 portdest) (= 'tcp pro))


         (Rule4 = (accept interf ipsrc ipdest portsrc portdest pro) :-
                (= 'fw2int interf) (= 'port80 portdest) (= 'tcp pro) (= 'managerpc ipsrc))

         (Rule5 = (accept interf ipsrc ipdest portsrc portdest pro) :-
                (= 'fw2int interf) (= 'port80 portdest) (= 'tcp pro) (= 'webserver ipdest))

         (Rule6 = (deny interf ipsrc ipdest portsrc portdest pro) :-
                true) )

        ; Firewall policy: first rule applicable takes effect.
        (RComb (fa accept deny)))

