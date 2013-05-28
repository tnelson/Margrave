#lang s-exp "../policy-p4p.rkt"

; uses(vname): done
; rules without :- or =: done

; things to fix:
; *** should be able to load via #lang margrave automatically, from standalone file!
Policy( uses(myvoc),
        Variables(s(Subject), a(Action)),        
        Rules(rule1(permit(s, a), true)),
        RComb(fa(permit, deny)))


