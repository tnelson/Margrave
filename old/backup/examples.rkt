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

#lang racket

; updated to new EXPLORE query language 04/2010 tn

; TEMPORARY! Will be a nice module path soon.
(require (file "./margrave.rkt"))

; Welcome to Margrave! This file contains examples that are separated by (pause-for-user) so that you can
; execute it and read it in tandem. 

(define (run-examples)
  (start-margrave-engine)
  
  ;; Define the policies used in this script.
  ; Each policy references a vocabulary, which is loaded automatically. 
  ; The vocabulary defines (among other things) what shape a policy request takes.
  (load-policy (build-path (current-directory) "tests" "extconference.p"))
  (load-policy (build-path (current-directory) "tests" "conference1.p"))
  (load-policy (build-path (current-directory) "tests" "conference2.p"))
  (load-policy (build-path (current-directory) "tests" "phone1.p"))
  (load-policy (build-path (current-directory) "tests" "fwex1.p"))
  (load-policy (build-path (current-directory) "tests" "fwex1a.p"))
  (load-policy (build-path (current-directory) "tests" "fwex2.p"))
  (load-policy (build-path (current-directory) "tests" "fwex3.p"))
  (load-policy (build-path (current-directory) "tests" "happyrouterless.p"))
  (load-policy (build-path (current-directory) "tests" "happyroutermore.p"))
  (load-policy (build-path (current-directory) "tests" "iout.p"))
  
  (m "rename conferencepolicy1 conf1")
  (m "rename conferencepolicy2 conf2")
  (m "rename fwex1 firewall1")
  (m "rename fwex1a firewall1a")
  (m "rename fwex2 firewall2")
  (m "rename happyrouterless HRless")
  (m "rename happyroutermore HRmore")
  
  ;; ****
  ;; General examples
  ;; ****
  
  
  ; Basic predicate conference policy: When can someone read a paper?
  
  (mxout "EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r)")
  (mxout "SHOW ONE 0")
  (mxout "SHOW NEXT 0")
  (mxout "SHOW NEXT 0")
  (mxout "SHOW NEXT 0")
  (mxout "SHOW NEXT 0")
  (mxout "SHOW NEXT 0")
  (mxout "SHOW NEXT 0")
  (mxout "SHOW NEXT 0")
  (mxout "COUNT 0")
  (mxout "IS POSSIBLE? 0")
  
  
  
  
  ; 7 solutions
  ; * Both author and reviewer, and assigned to the paper
  ; * Both author and reviewer, both conflicted and assigned
  ; * Just a reviewer, both conflicted and assigned
  ; * Just a reviewer, neither conflicted nor assigned
  ; * Just a reviewer, only assigned
  ; * Just an author, neither conflicted nor assigned
  ; * Both author and reviewer, neither conflicted nor assigned
  (pause-for-user)
  
  
  
  
  ;; *******
  ;; Want to continue to refine? Restate the same query, using the PUBLISH clause.
  ;; The PUBLISH clause gives an ordering on the variables so you can use previous
  ;; queries in new ones.
  
  ;; The IS POSSIBLE? clause says to not bother printing the results -- we've
  ;; already seen them!
  
  (mxout "EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r) 
    PUBLISH s, a,r")
  (mxout "IS POSSIBLE? 0")
  
  ; The LAST identifier means the last query created via EXPLORE.
  (mxout "EXPLORE reviewer(s) and last(s,a,r)")
  (mxout "COUNT 0")
  
  ; Now there are 6 solutions. We ruled out this one:
  ; * Just an author, neither conflicted nor assigned
  (pause-for-user)
  
  
  
  
  
  
  
  
  ; Ok, so when is reading a paper NOT permitted?
  
  (mxout "EXPLORE readpaper(a) and paper(r) and not conf1:permit(s, a, r)")
  (mxout "SHOW CEILING 0")
  
  ; That query has a ceiling of 4 because of an assertion in the vocabulary that 
  ; there is some Subject. We didn't infer that subject(s). 
  (pause-for-user)
  
  
  ; Better:
  (mxout "EXPLORE readpaper(a) and paper(r) and subject(s) and not conf1:permit(s, a, r)")
  (mxout "SHOW CEILING 0")
  (pause-for-user)
  
  
  ;; !! check sort inference here
  
  
  ; Ok, so we can ask the most basic of questions: 
  ; "When does this policy render that decision?" How about something more complex:
  ; "When can a reviewer read ALL papers?"
  
  ; Like in Datalog, we get universals by defining subqueries and negating them.
  ; *** Query: There exists a reviewer who cannot read some paper.
  
  (mxout "EXPLORE reviewer(s) AND readpaper(a) AND paper(r) 
            AND NOT conf1:Permit(s, a, r)
    PUBLISH s, a")
  (mxout "IS POSSIBLE? 0")
  
  ; The PUBLISH clause lets us dictate which variables are available for later use.
  ; Un-PUBLISHED variables are bound within the query and can be negated as seen below.
  
  ; You can always see what parameters a saved query takes, and in what order:
  (mxout "INFO last")
  
  ; And rename "last" to something more meaningful
  (mxout "RENAME LAST notallowed")
  
  ; *** Query: There is a reviewer to whom the last query does not apply."
  (mxout "EXPLORE reviewer(s) AND readpaper(a) AND NOT notallowed(s, a)")
  (mxout "COUNT 0")
  
  ; That's odd: there are two solutions where there seem to be no papers at all! 
  ; We said there had to be some *Resource*, but we never said there had to be a *Paper*,
  ; and any situation with no papers satisfies this query vacuously. 
  (pause-for-user)
  
  ; Let's remove those confusing solutions:
  (mxout "EXPLORE reviewer(s) AND readpaper(a) AND NOT notallowed(s, a) AND paper(p)")
  (mxout "COUNT 0")
  ; This query forces some paper to exist in the scenario given, which weeds out
  ; the two vacuous solutions from before.
  
  (pause-for-user)
  
  
  
  
  ; A major aspect of Margrave is change-impact analysis: Discovering all
  ; situations in which 2 policies will render different decisions. 
  ; Suppose the two policies are Conference1 and Conference2: Conference2
  ; has a rule that Conference1 lacks.
  
  ; You can create your own change-impact query like so:
  
  (mxout "EXPLORE (conf1:Permit(sub, act, res) AND NOT conf2:Permit(sub, act, res)) OR 
            (conf2:Permit(sub, act, res) AND NOT conf1:Permit(sub, act, res)) OR 
            (conf1:Deny(sub, act, res) AND NOT conf2:Deny(sub, act, res)) OR
            (conf2:Deny(sub, act, res) AND NOT conf1:Deny(sub, act, res))")                
  (mxout "COUNT 0")
  (mxout "get one 0")
  
  (newline) (display "That was the manual change impact.") (newline)
  (pause-for-user)
  
  ; Margrave can also make a change-impact query for you via the COMPARE command:
  
  (mxout "COMPARE conf1 conf2")
  (mxout "COUNT 0")
  (mxout "get one 0")
  (newline) (display "That was the change impact via COMPARE.") (newline)
  (pause-for-user)
  
  ; the ordering used by LAST is now the standard ordering given by the policys' vocabulary.
  ; To confirm:
  (mxout "INFO last")
  
  ; And of course, change-impact queries can always be re-used via the LAST keyword.
  (mxout "EXPLORE author(s) AND last(s, a, r)")
  (mxout "COUNT 0")
  (mxout "get one 0")
  ; A word of caution: If you use your own queries instead of the CHANGE command,
  ; be sure to provide a PUBLISH keyword, or the ordering may not be standard!
  
  (pause-for-user)
  
  
  
  
  
  
  ; ***********************************************************************************************
  
  ; You aren't restricted to simple "Permit" and "Deny" either. This
  ; example phone company policy has 3 different decisions: TollFree, 
  ; Toll, and Refuse.
  
  ; When can someone make a toll-free call?
  (mxout "EXPLORE Phone1:TollFree(ncaller, nreceive)")
  (mxout "COUNT 0")
  (mxout "get one 0")
  (pause-for-user)
  
  
  
  
  
  
  ;; **********************************************************************************************
  ; Example 1: Simple firewall policy, no state
  ;; **********************************************************************************************
  
  
  ; What sorts of incoming packets do we allow through?
  (mxout "EXPLORE Firewall1:Accept(ipsrc, ipdest, portsrc, portdest, pro)")
  (mxout "COUNT 0")
  (mxout "get one 0")
  (pause-for-user)
  
  
  
  
  ; That is a lot of packet types, and so not very useful. Let's narrow the scope a bit.
  ; What packets are allowed through using TCP to port 21?
  (mxout "EXPLORE Firewall1:Accept(ipsrc, ipdest, portsrc, portdest, pro) 
            AND TCP(pro) AND port21(portdest)")
  (mxout "COUNT 0")
  (mxout "get one 0")
  (pause-for-user)
  
  
  
  
  
  ; Now suppose we changed this policy a bit. What has changed?
  ; (This doesn't break out by what *kind* of change. Can run separate queries if needed.)
  (mxout "COMPARE Firewall1 Firewall1a")
  (mxout "COUNT 0")
  (mxout "get one 0")
  (pause-for-user)
  
  
  
  
  ;; *********************************************************************************************
  ; Example 2: Simple stateless firewall policy, with ip ranges
  ;; **********************************************************************************************
  
  ; What packets are allowed through using TCP to port 21?
  (mxout "EXPLORE Firewall2:Accept(ipsrc, ipdest, portsrc, portdest, pro) and tcp(pro) and port21(portdest)")
  (mxout "COUNT 0")
  (mxout "get one 0")
  
  ; Nothing can connect to port 21 because of a misordering of rules...
  (pause-for-user)
  
  
  
  
  
  
  
  ; *********************************************************************************************
  ; Example 3: Sample ACL config from HappyRouter.com's Cisco ACL tutorial
  ; http://happyrouter.com/free-video-harden-your-cisco-router-with-ios-aclS
  
  ; When can a host on the less secure network have a 2-way TCP conversation with port 80 on the PC?
  (mxout "EXPLORE HRLess:Accept(ipoutside, ipinside, portoutside, portinside, pro) 
    and HRMore:Accept(ipinside, ipoutside, portinside, portoutside, pro) 
    and tcp(pro) and http(portinside) and 10network(ipoutside) and pc(ipinside)")
  (mxout "COUNT 0")
  (mxout "get one 0")
  (pause-for-user)
  
  
  
  ; For extremely large firewalls, it may help to turn on the TUPLING optimization:
  (mxout "EXPLORE HRLess:Accept(ipoutside, ipinside, portoutside, portinside, pro)
    and HRMore:Accept(ipinside, ipoutside, portinside, portoutside, pro)
    and tcp(pro) and http(portinside) and 10network(ipoutside) and pc(ipinside)
    TUPLING")
  (mxout "COUNT 0")
  (mxout "get one 0")
  
  ; Tupling often results in fewer solutions, but those solutions still
  ; characterize all possible satisfying scenarios for the query.
  (pause-for-user)
  
  
  (stop-margrave-engine))


; To EXIT the Margrave environment, use the (exit) command.

; 7 6 34 2 8 6 2 2 1 12 18 9 9 0 15 10

; !!!
; Missing: convenient keywords for ONE (e.g. ONE connection), disjointness, etc.

