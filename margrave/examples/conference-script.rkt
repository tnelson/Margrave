#lang racket

(require "../margrave.rkt")

;(start-margrave-engine #:margrave-params '("-log"))
(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "M:\\RktMargrave\\margrave")
;(m-load-policy "mypol" "F:\\msysgit\\git\\Margrave\\margrave\\examples\\conference1.p")
(m-load-policy "mypol1" "M:\\RktMargrave\\margrave\\examples\\conference1.p")

; basic
(m-let "Q1" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (ReadPaper a)))   
(m-is-poss? "Q1")

; exists
(m-let "Q2" '([s Subject]) 
       '(exists a Action (exists r Resource ([mypol1 permit] s a r))))
(m-is-poss? "Q2")

; forall
(m-let "Q3" '([s Subject]) 
       '(exists a Action (forall r Resource ([mypol1 permit] s a r))))
(m-is-poss? "Q3")

; isa test (true)
(m-let "Q4" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (isa a ReadPaper true)))   
(m-is-poss? "Q4")

; non (true) isa
(m-let "Q5" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (isa r Subject (Author r))))   
(m-is-poss? "Q5") ; false since Subject disj. Resource

; non (true) isa #2
(m-let "Q6" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (isa r Resource (Paper r))))   
(m-is-poss? "Q6") ; true, the isa is trivial

; NOTE: Names are coming out weird because we have downcasting in the
; policy. We're creating Paper#1, Paper#2, Paper#3 instead of Resource#1, etc.
; But we don't create more than 3 ever, so it's sound...
; This is a cosmetic thing that will be fixed when we switch to producing set bounds
; instead of numeric bounds.

; Numeric bounds are inferior for producing *lower-bounds*. Alloy gets them for _exact_ bounds.
; We don't know whether a leaf sort has an element because the element belongs there, or because
; it's been promoted there by downcasting!

; Reference to prior queries
;let Q7[s : Subject, a: Action, r : Resource] be Q1(s, a, r);
(m-let "Q7" '([s Subject] [a Action] [r Resource]) 
       '([Q1] s a r))   
(m-is-poss? "Q7") 


;let Q3[s : Subject, a: Action, r : Resource] be Q(s, a, f(r));
;
;let Q4[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r)));
;// !!! TODO : unbound leaf exception on show Q4. s/b better error. due to no sort A.
;
;let Q5[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r)));
;count Q5;
;show Q5;
;is poss? Q5;
;
;// But that will have a lot of vacuous solutions. If no actions...
;
;let Q6[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r))) and exists anAction : Action (anAction = anAction);
;count Q6;
;let Q7[s : Subject, r : Resource] be forall a2 : Action (Q(s, a2, f(r)))  and exists anAction : Action (anAction = anAction) CEILING 4;
;count Q7;
;
;let Q8[s : Subject ,a : Action,r : Resource] be Mypol.permit(s,a,r) and Mypol.deny(s,a,r);
;is poss? Q8;
;
;let Q9[s : Subject ,a : Action,r : Resource] be Mypol.permit(s,a,r) and s : Resource;
