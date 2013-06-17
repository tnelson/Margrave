;ACL for firewall facing the outside world

;1 -> DENY if: interface=fw1_dmz, da in blacklisted_ips
;2 -> DENY if: interface=fw1_external, sa in blacklisted_ips
;3 -> DENY if: interface=fw1_dmz, dp=23
;4 -> PERMIT if: interface=fw1_external, da=mailserver, 
;              dp=25, protocol=tcp  
;5 -> PERMIT if: interface=fw1_external, da=webserver, 
;              dp=80, protocol=tcp  
;6 -> PERMIT if: interface=fw1_dmz, da=any outside, dp=80,
;              protocol=tcp, sa=managerPC
;7 -> otherwise, DENY

Policy(uses(talkfirewallpolicy),
       Variables(
         interf(Interface),
         sa(IPAddress),
         da(IPAddress),
         sp(Port),
         dp(Port),
         pro(Protocol)),
                          
       Rules(
         rule1(deny(interf, sa, da, sp, dp, pro) :-
                    =($fw1dmz, interf), BlacklistedIPs(da)),
         rule2(deny(interf, sa, da, sp, dp, pro) :-
                    =($fw1ext, interf), BlacklistedIPs(sa)),
         rule3(deny(interf, sa, da, sp, dp, pro) :-
                    =($fw1dmz, interf), =($port23, dp)),

         rule4(permit(interf, sa, da, sp, dp, pro) :-
               =($fw1ext, interf), =($mailserver, da),
               =($port25, dp), =($tcp, pro)),

         rule5(permit(interf, sa, da, sp, dp, pro) :-
               =($fw1ext, interf), =($webserver, da),
               =($port80, dp), =($tcp, pro)),

         ; Bug in context of this network:
         ; managerPC packet will already have been subjected to NAT by FW2.
         rule6(permit(interf, sa, da, sp, dp, pro) :-
               =($fw1dmz, interf), =($managerpc, sa),
               =($port80, dp), OutsideIPs(da), =($tcp, pro)),
         
         rule7(deny(interf, sa, da, sp, dp, pro) :- true) 
         ),

       ; Firewall policy: first rule applicable takes effect.
       RComb(fa(permit, deny)))

