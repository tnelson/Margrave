#lang s-exp "../policy-p4p.rkt"

; uses(vname): done
; rules without :- or =: done
; Note: constants can now be signified by $. Single-quote is deprecated.

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

; *** types with > will be problematic, need alternative

;; *** terribad error message if disjoint(sort, sort).

Theory(myvoc,
       Vocab(myvoc, 
             Types(A, B, C(D, E), E(F), Subject, Action),
             Predicates( r(Subject, Action), q(Subject), q2(Subject)),
             Constants( $c(A) ), 
             Functions( f(A, B),
                        g(B, C))
            ),
       Axioms( disjoint(q, q2),
               singleton(A),
               abstract(C),
               formula(true)))
