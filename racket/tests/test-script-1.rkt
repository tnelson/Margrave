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
(check-true (string? (m-show-scenario "Q2"))) ; show for a model

; forall
(m-let "Q3" '([s Subject]) 
       '(exists a Action (forall r Resource ([mypol1 permit] s a r))))
(check-false (m-is-poss? "Q3")) ; false because Resource can't be empty; we have a constant in it.
(check-true (string? (m-show-scenario "Q3"))) ; show for unsat
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
(define sQ7 (m-get-scenario "Q7" #:include '(([mypol1 permit] s a r))))
(check-true (and (m-scenario? sQ7)))
                 


; function in query + #:include; lower level sort as type for free var.
(m-let "Q8" '([s Subject] [a Action] [r Paper]) 
       '(and ([mypol1 permit] s a r) 
             (= (techreportfor r) (techreportfor r))))   
(check-false (m-unsat? (m-get-scenario "Q8" #:include '(([mypol1 permit] s a r)))))
; check #:include for non-decision names
(check-false (m-unsat? (m-get-scenario "Q8" #:include '(([mypol1 papernoconflict_matches] s a r)))))
(check-false (m-unsat? (m-get-scenario "Q8" #:include '(([mypol1 paperassigned_applies] s a r)))))
; check #:include for non-decision names vs. saved query prior
; check #:include for non-decision names
(check-false (m-unsat? (m-get-scenario "Q7" #:include '(([mypol1 papernoconflict_matches] s a r)))))

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

; Make sure a policy can't declare the same rule twice.
(check-exn exn:fail? 
           (lambda () (m-load-policy "loaderror1" "two-rules-with-same-name.p")))


; Test partial functions
(m-load-policy "PFunc" "partialfunction.p")
; Functional:
(m-let "PFQ1" '() 
       '(exists t1 TechReport (exists t2 TechReport (exists p Paper (and
                                                                     (techreportfor p t1)
                                                                     (techreportfor p t2)
                                                                     (not (= t1 t2))))))
       #:under '("PFunc"))
(check-false (m-is-poss? "PFQ1"))
; Can be partial:
(m-let "PFQ2" '() 
       '(exists p Paper (forall tr TechReport (not (techreportfor p tr))))
       #:under '("PFunc"))
(check-true (m-is-poss? "PFQ2"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make sure vocabulary-combination preserves constraints in theories.
; (As a side effect: test all the constraint types.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(m-load-policy "pol-manyconstraints" "manyconstraints.p") 

(m-let "Qvocabcomboprior" '()
       'true
       #:under '("pol-manyconstraints"))

; disjointness between predicates
(m-let "Qvocabcombo1" '()
       '(and ([Qvocabcomboprior] ) 
             (exists x Object 
                     (and (pred1 x) (pred2 x))))
       #:under '("pol-manyconstraints"))
(check-false (m-is-poss? "Qvocabcombo1"))

; total-relation constraint sugar
(m-let "Qvocabcombo2" '()
       '(and ([Qvocabcomboprior] ) 
             (exists x Object 
                     (forall y Object
                             (not (binarypred1 x y)))))
       #:under '("pol-manyconstraints"))
(check-false (m-is-poss? "Qvocabcombo2"))

; abstract sort
; (Note: constants are counted like subsorts for purposes of 
; abstractness, hence the two equality checks.)
(m-let "Qvocabcombo3" '()
       '(and ([Qvocabcomboprior] ) 
             (exists x Object 
                     (and (not (Potato x))
                          (not (Computer x))
                          (not (Car x))
                          (not (Emu x))
                          (not (= x 'obj1))
                          (not (= x 'obj2)))))
       #:under '("pol-manyconstraints"))
(check-false (m-is-poss? "Qvocabcombo3"))

; Check atmostone, singleton, and nonempty
(m-let "Qvocabcombo4" '()
       '(and ([Qvocabcomboprior] ) 
             (or
              ; atmostone emu
              (exists emu1 Emu (exists emu2 Emu (not (= emu1 emu2))))
              ; nonempty car
              (forall car Car (not (= car car)))
              ; singleton potato
              (exists p1 Potato (exists p2 Potato (not (= p1 p2))))
              (forall p Potato (not (= p p)))
             ))
       #:under '("pol-manyconstraints"))
(check-false (m-is-poss? "Qvocabcombo4"))

; Check constants-neq-all
(m-let "Qvocabcombo-cneqall" '()
       '(and ([Qvocabcomboprior] ) 
             (= 'obj1 'obj2))
       #:under '("pol-manyconstraints"))
(check-false (m-is-poss? "Qvocabcombo-cneqall"))

; Check custom axiom (was: coco3 is empty)
(m-let "Qvocabcombo-custom1" '()
       '(and ([Qvocabcomboprior] ) 
             (exists c Computer (coco3 c)))
       #:under '("pol-manyconstraints"))
(check-false (m-is-poss? "Qvocabcombo-custom1"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Test show realized
(m-let "QSR1" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol1 permit] s a r) (ReadPaper a)))      
(check-true (member? "mypol1.permit(s, a, r)"
                     (m-show-realized "QSR1" '( ([mypol1 permit] s a r)) empty)))

; ISA cases
(check-equal? (m-show-realized "QSR1"                  
                               ; Candidates
                               '( ([mypol1 permit] s a r) )
                               ; Cases
                               '( (ReadPaper a) (SubmitReview a)))
              (hash-copy (hash "SubmitReview(a)" empty "ReadPaper(a)"  '("mypol1.permit(s, a, r)"))))
                 
; Test equality 
; TODO: support
;(m-show-scenario-realized "QSR1"                  
;                 ; Candidates
;                 '( (= s s) (= s r) )
;                 ; Cases
;                 '( ([mypol1 permit] s a r) ([mypol1 deny] s a r)))

; Functions? TODO support

; Test non-ISA
(check-equal? (m-show-realized "QSR1"                  
                 ; Candidates
                 '( (conflicted s r) (assigned s r))
                 ; Cases
                 '( ([mypol1 permit] s a r) ([mypol1 deny] s a r)))
              (hash-copy (hash "mypol1.deny(s, a, r)" empty "mypol1.permit(s, a, r)" '("conflicted(s, r)" "assigned(s, r)"))))


; TODO: Show realized should return structured data. Still returning the old-style strings.


;;;;;;;;;;;;;;;;;;;;;;;
; Test that IDBs can refer to other IDBs in a policy, so long as the rules are stratifiable.
(m-load-policy "p2tiers" "idbs-refer-to-idbs.p")
(m-let "Qtiers1" '([s Subject] [a Action] [r Resource]) '([p2tiers permit] s a r))
(let ([c (m-count-scenarios "Qtiers1")])
  (check-true (and c (> c 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Test change-impact sugar and rule/decision formula functions

(m-policy-difference-query "diff1" "mypol1" "mypol2")
(m-load-policy "mypol3" "conference1-modified.p")
(m-policy-difference-query "diff2" "mypol1" "mypol3")

(check-false (m-is-poss? "diff1"))

(check-true (m-scenario? (m-get-scenario "diff2"
                                         #:include (m-policy-decisions-idbs "mypol3" '(v0 v1 v2)))))
(check-true (m-scenario? (m-get-scenario "diff2"
                                         #:include (m-policy-rules-idbs/matches "mypol3" '(v0 v1 v2)))))
(check-true (m-scenario? (m-get-scenario "diff2"
                                         #:include (m-policy-rules-idbs/applies "mypol3" '(v0 v1 v2)))))
 
; !!! PROBLEM: TODO: if problem w/ policy text, says "unsaved editor" instead of file name. why?

(check-true (scenario-has-relation-or-sort
             (m-get-scenario "diff2"
                #:include (append 
                           (m-policy-decisions-idbs "mypol1" '(v0 v1 v2))
                           (m-policy-decisions-idbs "mypol3" '(v0 v1 v2))))
             "mypol3.deny"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test that the engine can be stopped
;(check-true (stop-margrave-engine))