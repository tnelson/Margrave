; ACL for the firewall facing the internal network
; AFTER second bug-fix (~ex 5)

;1 -> DENY if: interface=fw2_dmz
;2 -> DENY if: interface=fw2_internal, da=mailserver,
;              dp=25, protocol=TCP, sa in contractorPCs
;3 -> PERMIT if: interface=fw2_internal, da=mailserver, 
;              dp=25, protocol=tcp  
;4 -> PERMIT if: interface=fw2_internal, dp=80, protocol=tcp,
;                **sa=managerPC**
;** 5 -> PERMIT if: interface=fw2_internal, da=webserver,
;               dp=80, protocol=tcp**  
;6 -> otherwise, DENY

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
                    =($fw2dmz, interf)),

         rule2(deny(interf, sa, da, sp, dp, pro) :-
               =($fw2int, interf), =($mailserver, da),
               =($port25, dp), =($tcp, pro), =($contractorpc, sa)),

         rule3(permit(interf, sa, da, sp, dp, pro) :-
               =($fw2int, interf), =($mailserver, da),
               =($port25, dp), =($tcp, pro)),

         rule4(permit(interf, sa, da, sp, dp, pro) :-
               =($fw2int, interf), =($port80, dp),
               =($tcp, pro), =($managerpc, sa)),                                                   
         rule5(permit(interf, sa, da, sp, dp, pro) :-
               =($fw2int, interf), =($port80, dp),
               =($tcp, pro), =($webserver, da)),   
                                                   
         rule6(deny(interf, sa, da, sp, dp, pro) :- true)
       ),
         
       ; Firewall policy: first rule applicable takes effect.
       RComb(fa(permit, deny)))
