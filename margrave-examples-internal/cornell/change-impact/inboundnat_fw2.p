; NAT performed by inside firewall

Policy(uses(talkfirewallpolicy),
       Variables(
         interf(Interface),
         sa(IPAddress),
         da(IPAddress),
         sp(Port),
         dp(Port),
         pro(Protocol),
         newip(IPAddress)),

       Rules(         
         ; Static NAT on outgoing traffic (crossing into DMZ from internal network)
         rule1(translate(interf, sa, da, sp, dp, pro, newip) :-
               =($fw2int, interf), =($fw2static, newip)),
                 
         ; This rule exists to make sure that ONLY rule 1 is applied to fw2int,
         ; and can never fall through to RuleX.
         rule2(deny(interf, sa, da, sp, dp, pro, newip) :-
               =($fw2int, interf)),
 
         ; Otherwise, NAT has no effect
         ruleX(translate(interf, sa, da, sp, dp, pro, newip) :-
               =(sa, newip)) 
       ),
        
       RComb(fa(translate, deny)))
