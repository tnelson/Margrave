;ACL for firewall facing the outside world
; AFTER example 4 (the first bug "fix")

;1 -> DENY if: interface=fw1_dmz, da in blacklisted_ips
;2 -> DENY if: interface=fw1_external, sa in blacklisted_ips
;3 -> DENY if: interface=fw1_dmz, dp=23
;4 -> PERMIT if: interface=fw1_external, da=mailserver, 
;              dp=25, protocol=tcp  
;5 -> PERMIT if: interface=fw1_external, da=webserver, 
;              dp=80, protocol=tcp  
;6 -> PERMIT if: interface=fw1_dmz, da=any outside, dp=80,
;              protocol=tcp, **sa=fw2static**
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

         ; bug "fix"
         ; managerPC packet will already have been subjected to NAT by FW2.
         ; Clearly we should just allow the static ip through! Right? ...         
         rule6(permit(interf, sa, da, sp, dp, pro) :-
               =($fw1dmz, interf), =($fw2static, sa),
               =($port80, dp), OutsideIPs(da), =($tcp, pro)),
         
         rule7(deny(interf, sa, da, sp, dp, pro) :- true) 
        ),

        ; Firewall policy: first rule applicable takes effect.
        RComb( fa(permit, deny)))
