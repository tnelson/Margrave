; A policy says what its vocabulary is, declares variables it
; uses (with types), gives a set of rules, and says how to
; resolve conflicts between those rules.

; A rule has a name, a decision, and some conditions.
; rulename( decision-fmla :- condition, ...)

Policy(uses(filter),
       Variables(sa(IPAddress), da(IPAddress),
                 sp(Port),      dp(Port)),

       Rules(
         ; Packets from workstation 2 to the server should be denied.
         rule1(deny(sa, sp, da, dp) :- =(sa,$10.1.1.2), =(da,$10.1.20.20)),
       
         ; Packets from other workstations to the server should be allowed	.
         rule2(permit(sa, sp, da, dp) :- =(da,$10.1.20.20))
       ))
