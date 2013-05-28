#lang s-exp "../policy-p4p.rkt"

; uses(vname): done
; rules without :- or =: done

; things to fix:
; *** should be able to load via #lang margrave automatically, from standalone file!
Policy( uses(myvoc),
        Variables(s(Subject), a(Action)),        
        Rules( rule1(log(s, a), true),
               rule2(deny(s, a), r(s, a), not(q(s))) 
             ),
        RComb(fa(permit, deny )))

; *** terribad error message if R or Q are capitalized. why? because contract violation.
;     the macros expect a lowercase id for func/pred.

Theory(myvoc,
       Vocab(myvoc, 
             Types(),
             Predicates(),
             Constants(
               ),
             Functions()
            ),
       Axioms())


