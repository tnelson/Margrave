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
;              protocol=tcp, **ipsrc=fw2static**
;7 -> otherwise, DENY


Policy( uses(talkfirewallpolicy),
        Variables(
         interf(Interface),
         ipsrc(IPAddress),
         ipdest(IPAddress),
         portsrc(Port),
         portdest(Port),
         pro(Protocol)),
                          
        Rules(
        
         rule1( deny(interf, ipsrc, ipdest, portsrc, portdest, pro),
                        =($fw1dmz, interf), BlacklistedIPs(ipdest)),
         rule2( deny(interf, ipsrc, ipdest, portsrc, portdest, pro),
                        =($fw1ext, interf), BlacklistedIPs(ipsrc)),
         rule3( deny(interf, ipsrc, ipdest, portsrc, portdest, pro),
                        =($fw1dmz, interf), =($port23, portdest)),


         rule4( accept(interf, ipsrc, ipdest, portsrc, portdest, pro),
                =($fw1ext, interf), =($mailserver, ipdest),
                =($port25, portdest), =($tcp, pro)),

         rule5( accept(interf, ipsrc, ipdest, portsrc, portdest, pro),
                =($fw1ext, interf), =($webserver, ipdest),
                =($port80, portdest), =($tcp, pro)),

         ; bug "fix"
         ; managerPC packet will already have been subjected to NAT by FW2.
         ; Clearly we should just allow the static ip through! Right? ...         
         rule6( accept(interf, ipsrc, ipdest, portsrc, portdest, pro),
                =($fw1dmz, interf), =($fw2static, ipsrc),
                =($port80, portdest), OutsideIPs(ipdest), =($tcp, pro)),
         
         rule7( deny(interf, ipsrc, ipdest, portsrc, portdest, pro),
                true) 
        ),

        ; Firewall policy: first rule applicable takes effect.
        RComb( fa(accept, deny)))
