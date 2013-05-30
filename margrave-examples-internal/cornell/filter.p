; A policy says what its vocabulary is, declares variables it
; uses (with types), gives a set of rules, and says how to
; resolve conflicts between those rules.

Policy( uses(filter),

       Variables(sa(IPAddress), da(IPAddress),
                 sp(Port),      dp(Port)),

       ; A rule has a name, a decision, and some conditions.
      
        Rules(
       ; Log whenever a packet is denied:
          rule1(log(sa, sp, da, dp), deny(sa, sp, da, dp)),

       ; Packets from workstation 2 to the server should be denied.
          rule2(deny(sa, sp, da, dp), ip10-1-1-2(sa), ip10-1-20-20(da)),
       
       ; Packets from other workstations to the server should be allowed.
          rule3(permit(sa, sp, da, dp), ip10-1-1-x(sa), ip10-1-20-20(da))


       ; TODO: more rules
       
             ),

       ; Because "log" is not listed here, it is free to apply
       ; regardless of permit or deny.
        RComb(fa(permit, deny)))