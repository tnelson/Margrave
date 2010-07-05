;    Copyright © 2009 Brown University and Worcester Polytechnic Institute.
;    
;    This file is part of Margrave.

;    Margrave is free software: you can redistribute it and/or modify
;    it under the terms of the GNU Lesser General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Margrave is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Lesser General Public License for more details.
;
;    You should have received a copy of the GNU Lesser General Public License
;    along with Margrave.  If not, see <http://www.gnu.org/licenses/>.

; tn

;; filenames must be URLS -- don't forget the file: prefix

(newline)
(display "-> Loading margrave.scm ... ... ...") (newline)
(load "margrave.scm")
(display "-> margrave.scm loaded. Starting to run test cases.") (newline)
(newline)

;; Define policies for use in testing
(newline)
(display "-> Parsing policies for use in tests ... ... ...") (newline)
(define pEmptyConference (load-policy (string-append my-directory "tests/emptyconference.p")))
(define pExtConference (load-policy (string-append my-directory "tests/extconference.p")))
(define pConference1 (load-policy (string-append my-directory "tests/conference1.p")))
(define pHospitalFAC (load-policy (string-append my-directory "tests/hospitaldenypayrollmedrecsfa.p")))
(define pHospitalDO (load-policy (string-append my-directory "tests/hospitaldenypayrollmedrecsdo.p")))
(define pAWFW (load-policy (string-append my-directory "tests/awfw.p")) )
(define pBigFW (load-policy (string-append my-directory "tests/bigfw.p"))) 

(define-policy "hospitalfac" pHospitalFAC)

(display "-> Test policy files loaded.") (newline)
(newline)

;; **********************************************************************************************
;; Test with GIANT policy first

;(print-policy-info pBigFW)

(define qTest (m "EXPLORE BigFW:Accept(ipsrc ipdest portsrc portdest)
          IS POSSIBLE?"))
; will trigger printing the formula, which is large. silly .toString() doesn't cache...
;(set-debug-level qTest 3)

(test "Giant model query 1" 
      qtest
      #t)

(newline) (newline)
;; **********************************************************************************************

; Invoke low-level KodKod tests from here.
(display "-> Running Java Tests ... ") (newline)
(define-java-class <MJavaTests> |edu.wpi.margrave.MJavaTests|)
((generic-java-method '|runTests|) (java-null <MJavaTests>))
(newline) (newline)

;; **********************************************************************************************

; Tests in ruleless policy
; Note this policy has no requirement that the subdomains of Subject are disjoint! 
(define qTest (m "EXPLORE EmptyConference:Permit(s a r)
                  IS POSSIBLE?"))

; Test settings API
(newline) (display "Testing settings api: Should be SAT4j, then MiniSAT, then back again to SAT4j.") (newline)
(print-query-settings qTest)
(set-query-satsolver qTest 'minisat)
(print-query-settings qTest)
(set-query-satsolver qTest 'sat4j)
(print-query-settings qTest)
(newline)

(test-model "Empty policy Permit is unsat" qTest -1 0 3)
(define qTest (m "EXPLORE Subject(s) AND Action(a) AND Resource(r) AND NOT EmptyConference:Permit(s a r)
                  IS POSSIBLE?"))
(test-model "Empty policy !Permit is everything at that size" qTest 3 36 3)
;(pretty-print-results qTest)

;; Be explicit per query
; This policy's vocab says s/a/r are abstract now.
(define qTest2 (m "EmptyConference:Permit(s a r)"))
(test-model "Above refined to single action of 3" qTest2 3 12 3)

(define qTest3 (m "EXPLORE Subject(s) AND Action(a) AND Resource(r) AND ReadPaper(a) AND Paper(r) AND NOT EmptyConference:Permit(s a r)"))
(test-model "Refine even more to single resource of 2 (more than half due to complexity of predicates on that resource)" qTest3 3 9 3)

(define qTest4 (m "EXPLORE Subject(s) AND Action(a) AND Resource(r) AND ReadPaper(a) AND Paper(r) AND NOT Conflicted(s r) AND NOT EmptyConference:Permit(s a r)"))
(test-model "Refine again to exclude (s, r) from predicate" qTest4 3 5 3)

(define qTest5 (m "EXPLORE Subject(s) AND Action(a) AND Resource(r) AND ReadPaper(a) AND Paper(r) AND NOT Conflicted(s r) AND NOT Assigned(s r) AND NOT EmptyConference:Permit(s a r)"))
(test-model "Refine again, exclude (s, r) from second pred" qTest5 3 3 3)

(define qTest6 (m "EXPLORE Subject(s) AND Action(a) AND Resource(r) AND ReadPaper(a) AND Paper(r) AND NOT Conflicted(s r) AND NOT Assigned(s r) AND Author(s) AND NOT EmptyConference:Permit(s a r)"))
(test-model "Refine to constrain Subject type" qTest6 3 2 3)

(define qTest7 (m "EXPLORE Subject(s) AND Action(a) AND Resource(r) AND ReadPaper(a) AND Paper(r) AND Reviewer(s) AND NOT Conflicted(s r) AND NOT Assigned(s r) AND Author(s) AND NOT EmptyConference:Permit(s a r)"))
(test-model "Refine to constrain Subject type (pt 2)" qTest7 3 1 3)

(newline)
;; **********************************************************************************************

; Tests for var disjoint constraint w.r.t query refinement
(define qTest (m "EXPLORE IPAddress(x) AND NOT IPAddress(x)"))
(test "IPSRC != IPDEST constraint doesn't cause error if both vars unused" (is-query-satisfiable? qTest) #f)
(define qTest (m "EXPLORE IPAddress(ipsrc) AND IPAddress(ipsrc)"))
(test "IPSRC != IPDEST constraint doesn't cause error if one var unused" (is-query-satisfiable? qTest) #t)

; (1) Refinement is incomplete
; (2) Disjoint-var constraint is incomplete
;(define qTest2 (refine-query qTest "(forsome ipdest IPAddress (IPAddress ipdest))"))
;(test "IPSRC != IPDEST constraint -- refinement to use both includes constraint (1)" (is-query-satisfiable? qTest2) #t)
;(define qTest2 (refine-query qTest "(forsome ipdest IPAddress (= ipsrc ipdest))"))
;(test "IPSRC != IPDEST constraint -- refinement to use both includes constraint (2)" (is-query-satisfiable? qTest2) #f)




; Firewall policy (from MWZ paper)
(define qTest (m "EXPLORE IPAddress(ipsrc) AND IPAddress(ipdest) AND Port(portsrc) AND Port(portdest) AND AWFW:Accept(ipsrc ipdest portsrc portdest)"))
(test "AWFW Basic Query 1" (is-query-satisfiable? qTest) #t)


; Test tupling in scm interface
(set-tupling qTest #t)
(set-debug-level qTest 2)
(test "AWFW Basic Query 1 (WITH TUPLING)" (is-query-satisfiable? qTest) #t)
(set-tupling qTest #f)

; Refine to no connections in state
;(define qTest (refine-query qTest "(forAll tempip1 IPAddress (forAll tempip2 IPAddress (forAll tempport1 Port (forAll tempport2 Port (not (Connection tempip1 tempip2 tempport1 tempport2))))))"))
;(test "AWFW Basic Query 2" (is-query-satisfiable? qTest) #t)

(define qTest (refine-query qTest "(EMPTY Connection)"))
(test "AWFW Basic Query 2" (is-query-satisfiable? qTest) #t)

;(define qTest2 (refine-query qTest "(and (Zoonet ipsrc) (traceroute portdest))"))
(define qTest2 (m "EXPLORE IPAddress(ipsrc) AND IPAddress(ipdest) AND Port(portsrc) AND Port(portdest) AND  AWFW:Accept(ipsrc ipdest portsrc portdest) AND Zoonet(ipsrc) AND nntp(portdest) AND EMPTY(Connection)"))
(test "AWFW Basic Query 3" (is-query-satisfiable? qTest2) #t)


; ^^^^^^^^^^^^^^^^^^^
; This could be fixed & extended to make a very good example.
; Do it (but not today.) - TN 12/13

;(define qTest3 (refine-query qTest2 "(not (nntp portsrc))"))
(define qTest3 (m "EXPLORE IPAddress(ipsrc) AND IPAddress(ipdest) AND Port(portsrc) AND Port(portdest) AND AWFW:Accept(ipsrc ipdest portsrc portdest) AND Zoonet(ipsrc) AND traceroute(portdest) AND NOT nntp(portsrc) AND EMPTY(Connection)"))
(test "AWFW Basic Query 4" (is-query-satisfiable? qTest3) #f)

(newline)
;; **********************************************************************************************

; Test of a very basic query WITHOUT a full request
(define qTest (m "EXPLORE Purpose(p) AND Scheduling(p) UNDER hospitalfac"))
(test-model "Basic Query Tests 1" qTest 1 1 1) ; one solution (when scheduling is the purpose!)

; Same, with negation
(define qTest (m "EXPLORE Purpose(p) AND NOT Scheduling(p) UNDER hospitalfac"))
(set-size-ceiling qTest 1)
(test-model "Basic Query Tests 1a" qTest 1 5 1) ; five solutions (the other possibilities; other subsorts plus Purpose)

; Test that a query CAN be made satisfiable by an empty sort
(define qTest (query-policy pHospitalFAC "(forall p Purpose (Subject p))"))
(test "Basic Query Tests 2" (is-query-satisfiable? qTest) #t)

; Test of an unsatisfiable query with leading universal (and EXPLICIT constraint, not part of the vocab!)
(define qTest (query-policy pHospitalFAC "(and (forall p Purpose (Subject p)) (forsome x Purpose (Purpose x)))"))
(test "Basic Query Tests 2a" (is-query-satisfiable? qTest) #f)

; Test of an unsatisfiable query with only an existential
(define qTest (m "EXPLORE Purpose(p) AND Subject(p) UNDER hospitalfac"))
(test "Basic Query Tests 2b" (is-query-satisfiable? qTest) #f)

; Test of an ALWAYS satisfiable query with only an existential
(define qTest (m "EXPLORE Purpose(p) UNDER hospitalfac"))
(set-size-ceiling qTest 1)
(test-model "Basic Query Tests 3" qTest 1 6 1) ; 5 purposes + none of the above.


; Test a normal request...
; Should be satisfiable (because we are using FA combinator and the first rule has potential applicability.)
(define qTest (m "EXPLORE HospitalDenyPayrollMedRecs:Permit(s a r p) UNDER hospitalfac"))
(set-size-ceiling qTest 4)
(test "Basic Request Query Tests 1" (is-query-satisfiable? qTest) #t)

;(pretty-print-results qTest)
;(set-debug-level qTest 3)

; H.U. here is 5, not 4 -- because there is a rule-scope existential in one of the rules invoked by Permit.

(test-model "Basic Request Query Tests 1a" qTest 4 84 5)


;(define qTest2 (refine-query qTest "(Scheduling p)"))

(define qTest2 (m "EXPLORE Scheduling(p) AND HospitalDenyPayrollMedRecs:Permit(s a r p) UNDER hospitalfac"))
(set-size-ceiling qTest2 4)

; How many permits have purpose = Scheduling?
(test-model "Basic Request Query Tests 1b" qTest2 4 8 5)

; Unsat (no permit rule has this target, since all Purposes are disjoint.) 
(define qTest3 (m "EXPLORE PaymentNotification(p) AND NOT PayrollDept(s) AND HospitalDenyPayrollMedRecs:Permit(s a r p) UNDER hospitalfac"))
(set-size-ceiling qTest3 4)
(test "Basic Request Query Tests 1c" (is-query-satisfiable? qTest3) #f)

; Sneaky sat (first permit rule has this target, since we don't require Subjects to be disjoint: This one is both a PayrollDept and a Doctor)
; Other rules apply too (only ruling out PayrollNotify...) 
(define qTest3 (m "EXPLORE HospitalDenyPayrollMedRecs:Permit(s a r p) AND NOT PaymentNotification(p) AND PayrollDept(s) UNDER hospitalfac"))
(set-size-ceiling qTest3 4)
(test "Basic Request Query Tests 1d" (is-query-satisfiable? qTest3) #t)
(test-model "Basic Request Query Tests 1e" qTest3 4 42 5)

; Rule out rules!
; Disallow SchedRevTelData
(define qTest3 (query-policy pHospitalFAC
                                "(forSome s Subject (forSome a Action (forSome r Resource (forSome p Purpose (and (HospitalDenyPayrollMedRecs:Permit s a r p) (not (PaymentNotification p)) (not (Scheduler s)) (PayrollDept s))))))"))
(set-size-ceiling qTest3 4)
(test-model "Basic Request Query Tests 1f" qTest3 4 19 5)

; Disallow PayrollReview and DocRevResults
; These 4 models are for the rule UpdateWithholding -- so subject is an Employee. But without a constraint on Subject, there are models with PayrollDept s as below!
; 4 Models: Doctor/not x Consent/not.
(define qTestOut (m "EXPLORE HospitalDenyPayrollMedRecs:Permit(s a r p) AND NOT PaymentNotification(p) AND NOT Review(a) AND NOT Scheduler(s) AND PayrollDept(s) UNDER hospitalfac"))
(set-size-ceiling qTestOut 4)
(test-model "Basic Request Query Tests 1g" qTestOut 4 4 5)

; Disallow final rule
(define qTest3 (m "EXPLORE HospitalDenyPayrollMedRecs:Permit(s a r p) AND NOT PaymentNotification(p) AND NOT Review(a) AND NOT Scheduler(s) AND PayrollDept(s) AND NOT Employee(s) UNDER hospitalfac"))
(set-size-ceiling qTest3 4)
(test "Basic Request Query Tests 1h" (is-query-satisfiable? qTest3) #f)


; Only rule 1 applies 
(define qTest3 (m "EXPLORE HospitalDenyPayrollMedRecs:Permit(s a r p) AND NOT Scheduling(p) AND Doctor(s) AND Review(a) AND MedTestResults(r) UNDER hospitalfac"))
(set-size-ceiling qTest3 4)
;(set-debug-level qTest 3)
;(pretty-print-results qTest3)
(test-model "Basic Request Query Tests 1i" qTest3 4 20 5)

(define qTest3 (refine-query qTest "(forSome s Subject (forSome a Action (forSome r Resource (forSome p Purpose (and (HospitalDenyPayrollMedRecs:Permit s a r p) (not (Scheduling p)) (not (PayrollDept s)) (Review a) (MedTestResults r) (not (Doctor s)))))))"))
(test "Basic Request Query Tests 1j" (is-query-satisfiable? qTest3) #f)

(newline)
(display "Test case suite finished.") (newline)






; ALL 128 solutions at size 8 have (Scheduling p) -- but that does not mean there are no 
; solutions with !(Scheduling p)
; query refinement is a tricky business... !






; ********************************
; Final memory check!
; (This is for testing purposes only -- don't use these in a real script.
(define pEmptyConference 0)
(define pExtConference 0)
(define pConference1 0)
(define pHospitalFAC 0)
(define pHospitalDO 0)
(define pAWFW 0)
(define pBigFW 0)

(define qTest 0)
(define qTest2 0)
(define qTest3 0)
(define qTest4 0)
(define qTest5 0)
(define qTest6 0)
(define qTest7 0)

(display "Running final gc.") (newline)
(run-gc)
(print-cache-info)

