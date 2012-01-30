#lang racket

(require "../margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../")
(m-load-policy "mypol1" "conference1.p")
(m-load-policy "mypol2" "conference2.p")

; basic
(m-let "Q1" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (ReadPaper a)))      
(check-true (m-is-poss? "Q1"))

; error! "r" used as the wrong sort.
(check-exn 
 exn:fail? 
 (lambda () (m-let "Qx1" '([s Subject] [a Action] [r Resource]) 
                   '(and ([mypol1 permit] s r r) (ReadPaper a)))))

; exists
(m-let "Q2" '([s Subject]) 
       '(exists a Action (exists r Resource ([mypol1 permit] s a r))))
(check-true (m-is-poss? "Q2"))
(check-true (string? (m-show "Q2"))) ; show for a model

; forall
(m-let "Q3" '([s Subject]) 
       '(exists a Action (forall r Resource ([mypol1 permit] s a r))))
(check-false (m-is-poss? "Q3")) ; false because Resource can't be empty; we have a constant in it.
(check-true (string? (m-show "Q3"))) ; show for unsat
(check-true (equal? 0 (m-count-scenarios "Q3")))

; forall (w/o constant)
(m-let "Q3a" '([s Subject]) 
       '(exists a Action (forall r Resource ([mypol2 permit] s a r))))
(check-true (m-is-poss? "Q3a")) ; true because of vacuous (Resource) satisfiability

; isa test (true)
(m-let "Q4" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (isa a ReadPaper true)))   
(check-true (m-is-poss? "Q4"))

; non (true) isa
(m-let "Q5" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (isa r Subject (author r))))   
(check-false (m-is-poss? "Q5")) ; false since Subject disj. Resource

; non (true) isa #2
(m-let "Q6" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (isa r Resource (Paper r))))   
(check-true (m-is-poss? "Q6")) ; true, the isa is trivial

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
(check-true (m-is-poss? "Q7")) ; true

; Prior queries + #:include in show
(define sQ7 (m-get "Q7" #:include '(([mypol1 permit] s a r))))
(check-true (and (m-scenario? sQ7)))
                 


; function in query + #:include; lower level sort as type for free var.
(m-let "Q8" '([s Subject] [a Action] [r Paper]) 
       '(and ([mypol1 permit] s a r) 
             (= (techreportfor r) (techreportfor r))))   
(check-false (m-unsat? (m-get "Q8" #:include '(([mypol1 permit] s a r)))))
; check #:include for non-decision names
(check-false (m-unsat? (m-get "Q8" #:include '(([mypol1 papernoconflict_matches] s a r)))))
(check-false (m-unsat? (m-get "Q8" #:include '(([mypol1 paperassigned_applies] s a r)))))
; check #:include for non-decision names vs. saved query prior
; check #:include for non-decision names
(check-false (m-unsat? (m-get "Q7" #:include '(([mypol1 papernoconflict_matches] s a r)))))

(check-exn 
 exn:fail? 
 (lambda () (m-let "Qx8" '([s Subject] [a Action] [r Resource]) 
                   '(and ([mypol1 permit] s a r) 
                         (= (functionthatdoesntexist r) (functionthatdoesntexist r))))))



; abstractness, under. should be false!
(m-let "Q9" '([r Resource]) 
       '(and (not (Paper r)) (not (Review r)) (not (TechReport r)))
       #:under '("mypol1"))
(check-false (m-is-poss? "Q9")) ; false, Resource is abstract

; m-count-scenarios returns #f if there are more scenarios than it can count.
; So test it on a query with a smaller satisfying set.
; Also test _matches and _applies for a policy rule
(m-let "Qrestrictedsize" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol2 permit] s a r)             
             (ReadPaper a)
             ([mypol2 paperAssigned_applies] s a r)
             (not ([mypol2 paperNoConflict_matches] s a r))
             (forall tr TechReport (forall tr2 TechReport (= tr tr2)))))
(define num-scenarios (m-count-scenarios "Qrestrictedsize"))
(check-true (and num-scenarios
                 (> num-scenarios 0))) 

; Another _matches and _applies test.
; We don't need the prior query struct to store the matches/applies IDBs.
(m-let "Q10" '([s Subject] [a Action] [r Resource]) 
       '(and ([Q1] s a r)
             ([mypol1 papernoconflict_applies] s a r)
             (not ([mypol1 papernoconflict_matches] s a r))))
(check-false (m-is-poss? "Q10")) ; false (can't apply without matching)

(check-exn exn:fail? 
           (lambda () (m-load-policy "loaderror1" "two-rules-with-same-name.p")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test that the engine can be stopped
(check-true (stop-margrave-engine))