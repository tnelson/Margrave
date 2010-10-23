; Copyright © 2009-2010 Brown University and Worcester Polytechnic Institute.
;
; This file is part of Margrave.

; Margrave is free software: you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Margrave is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public License
; along with Margrave. If not, see <http://www.gnu.org/licenses/>.

#lang racket

(require rackunit
         rackunit/text-ui
         margrave
         xml)

(require margrave/margrave-policy-vocab)

(define (string-contains? str phrase)
  (cond [(< (string-length str) (string-length phrase)) false]
        [else (or (equal? (substring str 0 (string-length phrase)) phrase)
                  (string-contains? (substring str 1) phrase))]))

(define (exn-contains-message msg)
  (lambda (e) (and (exn? e) 
                   (string-contains? (exn-message e) msg))))

(define-namespace-anchor anchor)  

(define (do-expand form)
  (parameterize ([current-namespace
                  (namespace-anchor->namespace anchor)])
    (expand-once form)))
(define (do-eval form)
  (parameterize ([current-namespace
                  (namespace-anchor->namespace anchor)])
    (eval form )))
  

(define vocab-errors
  (test-suite
   "Vocabulary error messages"
   (check-exn (exn-contains-message "")
              (lambda () (do-expand '(PolicyVocab ))) "empty vocab")
   (check-exn (exn-contains-message "Expected a name for the vocabulary")
              (lambda () (do-expand '(PolicyVocab (Types)))) "vocab without name")
   (check-exn (exn-contains-message "Decisions clause is missing")
              (lambda () (do-expand '(PolicyVocab myvocabname (Types)))) "vocab without decisions")
   (check-exn (exn-contains-message "ReqVariables clause is missing")
              (lambda () (do-expand '(PolicyVocab myvocabname (Types) (Decisions)))) "vocab without reqvariables")
   (check-exn (exn-contains-message "Top-level type declaration was not valid")
              (lambda () (do-expand '(PolicyVocab myvocabname (Types) (Decisions) (ReqVariables)))) "vocab: bad top-level types (a)")
   (check-exn (exn-contains-message "Top-level type declaration was not valid")
              (lambda () (do-expand '(PolicyVocab myvocabname (Types:) (Decisions) (ReqVariables)))) "vocab: bad top-level types (b)")
   (check-exn (exn-contains-message "Must have at least one decision")
              (lambda () (do-expand '(PolicyVocab myvocabname (Types : A) (Decisions) (ReqVariables)))) "vocab: no decisions")
   
   (check-exn (exn-contains-message "There must be at least one request field")
              (lambda () (do-expand '(PolicyVocab myvocabname (Types : A) (Decisions Permit Deny) (ReqVariables)))) "vocab: no request variables")
   (check-exn (exn-contains-message "Invalid request field declaration")
              (lambda () (do-expand '(PolicyVocab myvocabname (Types : A) (Decisions Permit Deny) (ReqVariables s a r)))) "vocab: invalid request variables")
   
   (check-not-exn (lambda () 
                    (do-expand '(PolicyVocab myvocabname (Types : Subject Action Resource)
                                               (Decisions Permit Deny) 
                                               (ReqVariables (s : Subject) (a : Action) (r : Resource))))) "vocab: ok 1")
   
   (check-not-exn (lambda () 
                    (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource Tool Potato))
                                             (Decisions Permit Deny) 
                                             (ReqVariables (s : Subject) (a : Action) (r : Resource))))) "vocab: ok 2")
   (check-not-exn (lambda () 
                    (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource : Tool Potato))
                                             (Decisions Permit Deny) 
                                             (ReqVariables (s : Subject) (a : Action) (r : Resource))))) "vocab: ok 3")
   (check-not-exn (lambda () 
                    (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                             (Decisions Permit Deny) 
                                             (ReqVariables (s : Subject) (a : Action) (r : Resource))))) "vocab: ok 4")
   (check-not-exn (lambda () 
                    (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                             (Decisions Permit Deny) 
                                             (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                             (OthVariables )))) "vocab: ok 5")
   (check-not-exn (lambda () 
                    (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                             (Decisions Permit Deny) 
                                             (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                             (OthVariables (e : Subject))))) "vocab: ok 6")
   (check-not-exn (lambda () 
                    (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer PotatoBattery) Potato))
                                             (Decisions Permit Deny) 
                                             (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                             (OthVariables (e : Subject))
                                             (Constraints (disjoint-all Resource)
                                                          (subset Potato PotatoBattery ))))) "vocab: ok 7")
   
   (check-exn (exn-contains-message "Invalid request field declaration")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer PotatoBattery) Potato))
                                         (Decisions Permit Deny) 
                                         (ReqVariables (s : Subject) (a : Action) blargh (r : Resource))
                                         (OthVariables (e : Subject))
                                         (Constraints (disjoint-all Resource)
                                                      (subset Potato PotatoBattery ))))) "vocab: bad request field only one bad: symbol")
   
   
   (check-exn (exn-contains-message "Invalid declaration. The type Toolx was not declared")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                             (Decisions Permit Deny) 
                                             (Predicates (Foo Toolx Margrave))
                                             (ReqVariables (s : Subject) (a : Action) (r : Resource)))))              
              "vocab: bad sort name in predicate declaration")
                
   
   
   
   (check-exn (exn-contains-message "Invalid declaration. The type oof was not declared")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                         (Decisions Permit Deny) 
                                         (Predicates (Foo Tool Margrave))
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                         (Constraints (disjoint-all oof)))))
              "vocab: bad sort name in constraint definition")
   
   (check-exn (exn-contains-message "Invalid declaration. The type oogie was not declared")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                             (Decisions Permit Deny) 
                                             (Predicates (Foo Tool Margrave))
                                             (ReqVariables (s : Subject) (a : Action) (r : oogie)))))
              "vocab: bad sort name in reqvar definition")
  
   (check-exn (exn-contains-message "Invalid declaration. The type oogie was not declared")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                         (Decisions Permit Deny) 
                                         (Predicates (Foo Tool Margrave))
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                            (OthVariables (e : oogie)))))
              "vocab: bad sort name in othvar definition")
   
   (check-exn (exn-contains-message "Invalid declaration. The predicate Bar was not declared.")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                         (Decisions Permit Deny) 
                                         (Predicates (Foo Tool Margrave))
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                         (Constraints (total-function Bar)))))
              "vocab: bad predicate name in total-function constraint")
   
   
   (check-exn (exn-contains-message "The predicate Foo was declared, but had arity 1 which was lower than 2.")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                         (Decisions Permit Deny) 
                                         (Predicates (Foo Tool))
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                         (Constraints (total-function Foo)))))
              "vocab: bad predicate arity (!=2) in total-function constraint")
   
   
   (check-not-exn 
    (lambda () 
      (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                               (Decisions Permit Deny) 
                               (Predicates (Foo Subject Action Resource Potato))
                               (ReqVariables (s : Subject) (a : Action) (r : Resource))
                               (Constraints (total-function Foo)))))
    "vocab: good in spite of >2 arity on functionally-constrained pred")
   
   (check-exn (exn-contains-message "The type Foozle was not declared.")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer) Potato))
                                         (Decisions Permit Deny) 
                                         (Predicates (Foo Tool Margrave Margrave))
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                         (Constraints (subset Subject Foozle)))))
              "vocab: bad sort name in subset constraint")
   
   
   (check-exn (exn-contains-message "Invalid request field declaration")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer PotatoBattery) Potato))
                                         (Decisions Permit Deny) 
                                         (ReqVariables (s : Subject) (a : Action) (blargh) (r : Resource))
                                         (OthVariables (e : Subject))
                                         (Constraints (disjoint-all Resource)
                                                      (subset Potato PotatoBattery ))))) "vocab: bad request field only one bad: symbol in parens")
   
   (check-exn (exn-contains-message "Invalid constraint declaration")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer PotatoBattery) Potato))
                                         (Decisions Permit Deny) 
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                         (OthVariables (e : Subject))
                                         (Constraints (disjoint-all Resource)
                                                      (badconstraint Resource)
                                                      (subset Potato PotatoBattery ))))) "vocab: invalid constraint badconstraint")

   (check-exn (exn-contains-message "More than one Decisions clause found")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer PotatoBattery) Potato))
                                         (Decisions Permit Deny) 
                                         (Decisions More) 
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                         (OthVariables (e : Subject))
                                         (Constraints (disjoint-all Resource)
                                                      (badconstraint Resource)
                                                      (subset Potato PotatoBattery ))))) "vocab: 2 decisions clauses")   
   
   (check-exn (exn-contains-message "More than one ReqVariables clause found")
              (lambda () 
                (do-expand '(PolicyVocab myvocabname (Types : Subject Action (Resource (Tool Margrave PotatoPeeler Hammer PotatoBattery) Potato))
                                         (Decisions Permit Deny) 
                                         (ReqVariables (s : Subject) (a : Action) (r : Resource))
                                         (ReqVariables (x : Subject))
                                         (OthVariables (e : Subject))
                                         (Constraints (disjoint-all Resource)
                                                      (badconstraint Resource)
                                                      (subset Potato PotatoBattery ))))) "vocab: 2 reqvariables clauses")  
   )) ; end of vocab error tests


(define query-errors
  (test-suite
   "Query error messages"
   #:before (lambda () 
              (start-margrave-engine)
              (mtext "load policy *margrave*/tests/conference1.p;")
              (mtext "load policy *margrave*/tests/s-as-oth-var.p;"))
   #:after (lambda () (stop-margrave-engine))
         
   (check-exn (exn-contains-message "Request (free) variables and ``other'' (bound) variables in all a query's vocabularies must never overlap.")
              (lambda () (mtext "explore conferencepolicy1:permit(s, a, r) and s-as-oth-varp:permit(s, a, r)")))
   
   (check-exn (exn-contains-message "The variable name s is already used by some part of this query, and could not be safely substituted.")
              (lambda ()                
                (mtext "explore conferencepolicy1:permit(s,a,r) publish r;")
                (mtext "explore last(s);")))
   
   
   ))

(define query-results
  (test-suite
   "Query results"
   
   #:before (lambda () (start-margrave-engine))
   #:after (lambda () (stop-margrave-engine))
   
   (test-case
    "total-relation"
    (mtext "load policy *margrave*/tests/totalrelation.p;")
    (mtext "EXPLORE assigned(s, r) and reviewer(s) and paper(r) publish s under totalrelationp")
    (mtext "rename last responsible")
    (mtext "EXPLORE reviewer(x) and not responsible(x) under totalrelationp")
    ; Test totality
    (test-command "IS POSSIBLE?" "false" "total-relation1")
    ; Test that it isn't functional
    (mtext "EXPLORE assigned(s, r1) and assigned(s, r2) and not r1 = r2 and reviewer(s) and paper(r1) and paper(r2) under totalrelationp")
    (test-command "IS POSSIBLE?" "true" "total-relation2"))
   
   
   
   ))


(define policy-errors
  (test-suite
   "Policy error messages"
   (check-exn (exn-contains-message "Empty policy specification not allowed")
              (lambda () (do-expand '(Policy ))) "empty policy")
   (check-exn (exn-contains-message "Policy must supply both its name and the name of the vocabulary it uses")
              (lambda () (do-expand '(Policy polname ))) "policy incomplete 1")
   (check-exn (exn-contains-message "Policy must supply both its name and the name of the vocabulary it uses. (e.g. Policy mypolicy uses myvocabulary ...)")
              (lambda () (do-expand '(Policy polname vocname))) "policy incomplete 2")
   (check-exn (exn-contains-message "RComb clause is missing")
              (lambda () (do-expand '(Policy polname uses vocname))) "policy: no rcomb")
   (check-exn (exn-contains-message "PComb clause is missing")
              (lambda () (do-expand '(Policy polname uses vocname (RComb) ))) "policy: no pcomb")
   
   (check-exn (exn-contains-message "Invalid rule-combination")
              (lambda () (do-expand '(Policy polname uses vocname (RComb ) (PComb )))) "policy: invalid rcomb")
   (check-exn (exn-contains-message "Invalid policy-combination")
              (lambda () (do-expand '(Policy polname uses vocname (RComb FAC) (PComb )))) "policy: invalid pcomb")
   (check-exn (exn-contains-message "Invalid rule")
              (lambda () (do-expand '(Policy polname uses vocabname (RComb FAC) (PComb FAC) (Rules (Rule1 = (Permit x y z) :- !(pred x y z)))))) "policy: invalid rule ! outside")
   (check-exn (exn-contains-message "Invalid rule")
              (lambda () (do-expand '(Policy polname uses vocabname (RComb FAC) (PComb FAC) (Rules (Rule1 = (!pred x y z)))))) "policy: invalid rule no head of rule")
   (check-exn (exn-contains-message "Invalid rule")
              (lambda () (do-expand '(Policy polname uses vocabname (RComb FAC) (PComb FAC) (Rules (Rule1 = (!pred x y z)))))) "policy: invalid rule no head of rule 2")
  
   ; true is a valid rule target
   (check-not-exn (lambda () (do-expand '(Policy polname uses vocabname (RComb FAC) (PComb FAC) (Rules (Rule1 = (Permit x y z) :- true))))) "policy ok 1")
   
   ;;;;;;;;;
   ;; Need more than one expansion to test child behavior. Start using eval after this point
   ;;;;;;;;;
   
   (check-exn (exn-contains-message "Empty policy specification not allowed")
              (lambda () (do-eval '(Policy polname uses vocabname (RComb FAC) (PComb FAC) (Rules (Rule1 = (Permit x y z) :- (!pred x y z))) (Children (Policy ))))) "empty child policy")

   ; Need to call the func to test this. Should be a compile-time check, probably...   
   (check-exn (exn-contains-message "All children must have the same vocabulary as the parent")
              (lambda () 
                (start-margrave-engine) ; for margrave-home-path
                (define tests-path (build-path margrave-home-path "tests" "conferencepolicy.v"))  
                ((do-eval '(Policy polname uses conferencepolicy
                                           (RComb FAC) (PComb FAC) 
                                           (Rules (Rule1 = (Permit x y z) :- (!pred x y z))) 
                                           (Children (Policy child1 uses phonepolicy (RComb FAC) (PComb FAC))))) tests-path #'foo)
                (stop-margrave-engine)) "policy: child with different vocab")
     
   (check-exn (exn-contains-message "All policies in a hierarchy must have distinct names")
              (lambda () 
                (start-margrave-engine) ; for margrave-home-path
                (define tests-path (build-path margrave-home-path "tests" "conferencepolicy.v"))  
                ((do-eval '(Policy polname uses conferencepolicy
                                   (RComb FAC) (PComb FAC) 
                                   (Rules (Rule1 = (Permit x y z) :- (!pred x y z))) 
                                   (Children (Policy polname uses conferencepolicy (RComb FAC) (PComb FAC))))) tests-path #'foo)
                (stop-margrave-engine)) "policy: child name shared with parent")
   
   (check-exn (exn-contains-message "Error: Unable to get Relation for unknown sort name: getexchangexxx")
              (lambda () 
                (start-margrave-engine) ; for margrave-home-path
                (mtext "LOAD POLICY *margrave*/tests/badphonepolicy.p")
                (define tests-path (build-path margrave-home-path "tests" "badphonepolicy.p"))  
                (stop-margrave-engine)) "policy: bad relation")
   
   
   
   
   
   
   
   )) ; end of policy error tests


;To run this: (run-tests pretty-print-tests)
(define pretty-print-tests
  (test-suite
   "Pretty Printing tests"
   
   (check-true (string-contains? (pretty-print-response-xml test-model) "firewall1:accept is true for: [ipsrc, ipdest, portsrc, portdest, pro]") "1")
   (check-true (string-contains? (pretty-print-response-xml test-model) "Margrave computed that 1") "2")
   (check-true (string-contains? (pretty-print-response-xml test-sys-info) "System Information:") "3")
   (check-true (string-contains? (pretty-print-response-xml test-sys-info) "Init: 49729280") "4")
   (check-true (string-contains? (pretty-print-response-xml test-coll-info) "This policy is a LEAF;") "5")
   (check-true (string-contains? (pretty-print-response-xml test-coll-info) "This is a policy named: conf1") "6")
   (check-true (string-contains? (pretty-print-response-xml test-vocab-info) "Vocabulary Information:") "7")
   (check-true (string-contains? (pretty-print-response-xml test-vocab-info) "Vocabulary Name: examplefw1") "8")
   (check-true (string-contains? (pretty-print-response-xml test-vocab-info) "DISJOINT") "8a")
   
   (check-true (string-contains? (pretty-print-response-xml test-error-response) "Margrave encountered an error") "9")
   (check-true (string-contains? (pretty-print-response-xml test-exception) "Unknown IDB Collection: firewall1") "10")
   (check-true (string-contains? (pretty-print-response-xml low-user-ceiling) "Margrave will not check over ceiling size 2 without your permission") "11")
   (check-true (string-contains? (pretty-print-response-xml negative-ceiling) "Margrave could not calculate") "12")
   
   ;Actual returned results
   (start-margrave-engine #:margrave-params '("-log"))
   (test-command "load policy *margrave*/tests/phone1.p"
                 "Phone1")
   (test-command "info phone1"
                 "refusecall2_matches")
   (stop-margrave-engine)))

;Uncomment any of these out to see what the pretty printing result is
;(display "MODEL: \n")
;(pretty-print-response-xml test-model)
;(display "\n\n\tSYSINFO: \n")
;(pretty-print-response-xml test-sys-info)
;(display "\n\n\tCOLLECTION INFO: \n")
;(pretty-print-response-xml test-coll-info)
;(display "\n\n\tVOCAB INFO: \n")
;(pretty-print-response-xml test-vocab-info)
;(display "\n\n\tError: \n")
;(pretty-print-response-xml test-error-response)
;(display "\n\n\tException: \n")
;(pretty-print-response-xml test-exception)
;(display "\n\n\tLow User Ceiling:: \n")
;(pretty-print-response-xml low-user-ceiling)
;(display "\n\n\tNegative Ceiling: \n")
;(pretty-print-response-xml negative-ceiling)

; *******************************************************************
; Testing helper functions
(define (test-command command-string test-string [msg "(no test name)"])
  (let ([response-string (response->string (mtext command-string))])
    (check-true (string-contains? response-string test-string) 
                (string-append msg ": " test-string " expected; saw: " response-string))))

(define (test-command-error command-string test-error [msg "(no test name)"])
  (check-exn (exn-contains-message test-error)
             (lambda () (mtext command-string)) msg))

; Test conference1.p
(define conf1-test
  (test-suite
   "Conference1.p test"
   (start-margrave-engine)
   ;(load-policy (build-path (current-directory) "tests" "conference1.p"))      
   (test-command "LOAD POLICY \"*MARGRAVE*/tests/conference1.p\"" "ConferencePolicy1")
   (test-command "RENAME ConferencePolicy1 conf1" "false")
   
   ;7 Solutions      
      
   (test-command "EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r)"
                 "Query created successfully." "1")
   (test-command "GET ONE" "SOLUTION FOUND at size = 3" "2")
   (test-command "GET NEXT" "SOLUTION FOUND at size = 3" "3")
   (test-command "GET NEXT" "SOLUTION FOUND at size = 3" "4")
   (test-command "GET NEXT" "SOLUTION FOUND at size = 3" "5")
   (test-command "GET NEXT" "SOLUTION FOUND at size = 3" "6")
   (test-command "GET NEXT" "SOLUTION FOUND at size = 3" "7")
   (test-command "GET NEXT" "SOLUTION FOUND at size = 3" "8")
   (test-command "GET NEXT" "No more solutions"  "9")
   (test-command "COUNT" "7"  "10")
   (test-command "IS POSSIBLE?" "true"  "11")
   
   (test-command "EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r) PUBLISH s, a,r" 
                 "Query created successfully."  "12")
   (test-command "IS POSSIBLE?" "true"  "13")
      
   ;(mtext "RENAME conf1 conf1")
   (test-command "EXPLORE readpaper(a) and paper(r) and not conf1:permit(s, a, r)" 
                 "Query created successfully." "14")
   (test-command "GET CEILING 0" "4" "15")
   
   (test-command "EXPLORE readpaper(a) and paper(r) and subject(s) and not conf1:permit(s, a, r)"
                 "Query created successfully."  "16")
   (test-command "GET CEILING 0" "3"  "17")
   
   (test-command "LOAD policy *margrave*/tests/conference2.p" "ConferencePolicy2" "17a")
   (test-command "RENAME ConferencePolicy2 conf2" "false")
   
   (test-command "EXPLORE (conf1:Permit(sub, act, res) AND NOT conf2:Permit(sub, act, res)) OR 
            (conf2:Permit(sub, act, res) AND NOT conf1:Permit(sub, act, res)) OR 
            (conf1:Deny(sub, act, res) AND NOT conf2:Deny(sub, act, res)) OR
            (conf2:Deny(sub, act, res) AND NOT conf1:Deny(sub, act, res))"
                "Query created successfully."  "18")
  (test-command "COUNT 0" "2"  "19")
  (test-command "get one 0" "SOLUTION FOUND at size = 3"  "20")
  
  (test-command "EXPLORE readpaper(a) iff paper(r) and not conf1:permit(s, a, r)" 
                 "successfully"  "21")
  (test-command "EXPLORE readpaper(a) implies paper(r) and not conf1:permit(s, a, r)" 
                 "successfully" "22")
  
  ;Test explore modifiers
  (test-command "EXPLORE readpaper(a) and paper(r) and not conf1:permit(s, a, r) CEILING 10 publish s, a, r"
                "successfully" "22")
  
  ;Test TUPLING, since fw1 only has unary relations
  (test-command "load policy *margrave*/tests/fwex1.p" "" "22a")
  (test-command "EXPLORE fwex1:Accept(ipsrc, ipdest, portsrc, portdest, pro) TUPLING"
                "successfully" "23")
  
  ;test INCLUDE
  (test-command "EXPLORE conf1:permit(s, a, r) INCLUDE conf1:permit"
                "successfully" "24")
  
  
              
  (stop-margrave-engine)))

(define error-test
  (test-suite
   "Error tests"
   (start-margrave-engine #:margrave-params '("-log"))
   
   (test-command "load policy *margravE*/tests/conference1.p" "ConferencePolicy1" "load conference1.p")      
   (test-command-error "EXPLORE readpaper(a) and junk(b) and ConferencePolicy1:permit(s, a, r)"
                       "Unknown predicate junk of arity 1." "unknown edb: junk")
   ; ReadPaper is a sort. 
   (test-command-error "EXPLORE readpaper(a, b) and ConferencePolicy1:permit(s, a, r)"
                       "Unknown predicate readpaper of arity 2." "unknown edb: readpaper of arity 2 (even though readpaper is a sort name)")
         
   (stop-margrave-engine)))

(define engine-fail-test
  (test-suite
   "Java Engine Fail test"
   (start-margrave-engine)
   
   (mtext "load policy *margravE*/tests/conference1.p")       
   (mtext "RENAME ConferencePolicy1 conf1")
   (mtext "EXPLORE readpaper(a) and conf1:permit(s, a, r)")

   (stop-margrave-engine)
   (start-margrave-engine) ;; restart after closed by func?
   (test-command "info" "System Information:" "info test")
   (stop-margrave-engine)))
   
  
;*************************************************************************
; Constant XML for pretty-print tests

(define test-model (read-xml (open-input-string
                              "<MARGRAVE-RESPONSE type=\"model\">
<MODEL size=\"3\">
<UNIVERSE>
<ATOM>Atom0</ATOM>
<ATOM>Atom1</ATOM>
<ATOM>Atom2</ATOM>
</UNIVERSE>
<RELATION arity=\"1\" name=\"author\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"paper\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"subject\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"resource\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"submitreview\" />
<RELATION arity=\"1\" name=\"action\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"reviewer\" />
<RELATION arity=\"1\" name=\"readpaper\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"review\" />
<RELATION arity=\"1\" name=\"submitpaper\" />
<RELATION arity=\"2\" name=\"conflicted\">
<TUPLE>
<ATOM>Atom0</ATOM>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"2\" name=\"assigned\" />
<RELATION arity=\"1\" name=\"$r\">
<TUPLE>
<ATOM>Atom1</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"$a\">
<TUPLE>
<ATOM>Atom2</ATOM>
</TUPLE>
</RELATION>
<RELATION arity=\"1\" name=\"$s\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
<ANNOTATION>firewall1:accept is true for: [ipsrc, ipdest, portsrc, portdest, pro]</ANNOTATION>
</MODEL>
<STATISTICS computed-max-size=\"1\" max-size=\"1\" result-id=\"0\" user-max-size=\"6\"/>
</MARGRAVE-RESPONSE>")))

;Test that warnings appear if:
;the user ceiling is lower than the calculated ceiling
;or if the calculated ceiling is -1 (infinitary)
(define low-user-ceiling (read-xml (open-input-string
                                    "<MARGRAVE-RESPONSE type=\"model\">
<MODEL size=\"3\">
<UNIVERSE>
<ATOM>Atom0</ATOM>
<ATOM>Atom1</ATOM>
<ATOM>Atom2</ATOM>
</UNIVERSE>
<RELATION arity=\"1\" name=\"author\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
</MODEL>
<STATISTICS computed-max-size=\"3\" max-size=\"2\" result-id=\"0\" user-max-size=\"2\"/>
</MARGRAVE-RESPONSE>")))

(define negative-ceiling (read-xml (open-input-string
                                    "<MARGRAVE-RESPONSE type=\"model\">
<MODEL size=\"3\">
<UNIVERSE>
<ATOM>Atom0</ATOM>
<ATOM>Atom1</ATOM>
<ATOM>Atom2</ATOM>
</UNIVERSE>
<RELATION arity=\"1\" name=\"author\">
<TUPLE>
<ATOM>Atom0</ATOM>
</TUPLE>
</RELATION>
</MODEL>
<STATISTICS computed-max-size=\"-1\" max-size=\"1\" result-id=\"0\" user-max-size=\"0\"/>
</MARGRAVE-RESPONSE>")))

(define test-sys-info (read-xml (open-input-string
                                 "<MARGRAVE-RESPONSE type=\"sysinfo\">
<MANAGER atoms=\"9\" conjunctions=\"46\" decls=\"0\" disjunctions=\"11\" multiplicity=\"0\" negations=\"12\" num-variables=\"5\" q-exists=\"0\" q-forall=\"0\" relations=\"10\" total-formulas=\"78\" total-reclaimed=\"0\" variable-tuples=\"4\">
<HEAP-USAGE init=\"49729280\" max=\"708378624\" units=\"bytes\" used=\"9989928\"/>
<NON-HEAP-USAGE init=\"19136512\" max=\"117440512\" units=\"bytes\" used=\"4500392\"/>
</MANAGER>
<VOCABULARIES count=\"1\"/>
<COLLECTIONS count=\"1\"/>
<CACHED-RESULTS count=\"0\"/>
</MARGRAVE-RESPONSE>")))

(define test-coll-info (read-xml (open-input-string 
                                  "<MARGRAVE-RESPONSE type=\"collection-info\">
<POLICY-LEAF name=\"conf1\" rule-combine=\"fac\">
<IDBS>
<IDB base-name=\"paperconflict_applies\">conf1:paperconflict_applies</IDB>
<IDB base-name=\"paperconflict\">conf1:paperconflict</IDB>
<IDB base-name=\"deny\">conf1:deny</IDB>
<IDB base-name=\"permit\">conf1:permit</IDB>
<IDB base-name=\"papernoconflict_applies\">conf1:papernoconflict_applies</IDB>
<IDB base-name=\"paperassigned_applies\">conf1:paperassigned_applies</IDB>
<IDB base-name=\"papernoconflict\">conf1:papernoconflict</IDB>
<IDB base-name=\"paperassigned\">conf1:paperassigned</IDB>
</IDBS>
<FREE-VARIABLES>
<VARIABLE>s</VARIABLE>
<VARIABLE>a</VARIABLE>
<VARIABLE>r</VARIABLE>
</FREE-VARIABLES>
</POLICY-LEAF>
</MARGRAVE-RESPONSE>")))

(define test-vocab-info (read-xml (open-input-string
                                   "<MARGRAVE-RESPONSE type=\"vocabulary-info\">
<VOCABULARY name=\"examplefw1\">
<SORTS>
<SORT name=\"port80\"/>
<SORT name=\"port\">
<SORT name=\"port80\"/>
<SORT name=\"port21\"/>
</SORT>
<SORT name=\"port21\"/>
<SORT name=\"protocol\">
<SORT name=\"tcp\"/>
<SORT name=\"udp\"/>
</SORT>
<SORT name=\"ftpserver\"/>
<SORT name=\"udp\"/>
<SORT name=\"blacklistaddr\"/>
<SORT name=\"tcp\"/>
<SORT name=\"webserver\"/>
<SORT name=\"ipaddress\">
<SORT name=\"webserver\"/>
<SORT name=\"ftpserver\"/>
<SORT name=\"blacklistaddr\"/>
</SORT>
</SORTS>
<REQ-VECTOR>
<VARIABLE order=\"1\">ipsrc</VARIABLE>
<VARIABLE order=\"2\">ipdest</VARIABLE>
<VARIABLE order=\"3\">portsrc</VARIABLE>
<VARIABLE order=\"4\">portdest</VARIABLE>
<VARIABLE order=\"5\">pro</VARIABLE>
</REQ-VECTOR>
<AXIOMS>

<DISJOINT>

<SORT name=\"blacklistaddr\">

<SORT name=\"ftpserver\"/>

</SORT>

<SORT name=\"webserver\">

<SORT name=\"blacklistaddr\"/>

<SORT name=\"ftpserver\"/>

</SORT>

<SORT name=\"udp\">

<SORT name=\"tcp\"/>

</SORT>

<SORT name=\"port21\">

<SORT name=\"port80\"/>

</SORT>

</DISJOINT>

</AXIOMS>
</VOCABULARY>
</MARGRAVE-RESPONSE>")))

(define test-error-response
  (read-xml (open-input-string "<MARGRAVE-RESPONSE type=\"error\">

<ERROR subtype=\"a subtype\" type=\"a type\">This is an error</ERROR>

</MARGRAVE-RESPONSE>")))

(define test-exception
  (read-xml (open-input-string "<MARGRAVE-RESPONSE type=\"exception\">

<EXCEPTION class=\"edu.wpi.margrave.MSemanticException\" stack-trace=\"[edu.wpi.margrave.MCommunicator.validateDBIdentifier(MCommunicator.java:1161), edu.wpi.margrave.MCommunicator.exploreHelper(MCommunicator.java:971), edu.wpi.margrave.MCommunicator.xmlHelper(MCommunicator.java:144), edu.wpi.margrave.MCommunicator.handleXMLCommand(MCommunicator.java:100), edu.wpi.margrave.MCommunicator.executeCommand(MCommunicator.java:1051), edu.wpi.margrave.MCommunicator.readCommands(MCommunicator.java:1026), edu.wpi.margrave.MCommunicator.main(MCommunicator.java:74)]\">

<MESSAGE>Margrave could not understand...</MESSAGE>

<LOCATION problem=\"Unknown IDB Collection: firewall1\"/>

<COMMAND/>

</EXCEPTION>

</MARGRAVE-RESPONSE>")))


; ***********
(run-tests pretty-print-tests)
(run-tests conf1-test)
(run-tests error-test)
(run-tests engine-fail-test)

(run-tests vocab-errors)
(run-tests policy-errors)
(run-tests query-errors)
(run-tests query-results)