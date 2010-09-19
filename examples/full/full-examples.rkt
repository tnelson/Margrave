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

; tn

#lang racket
(require "../../margrave.rkt")

; Welcome to Margrave! This file contains examples that are separated by (pause-for-user) so that you can
; execute it and read it in tandem.


; First, start up the Margrave engine. 
(start-margrave-engine)


;; Define the policies used in this script.
; Each policy references a vocabulary, which is loaded automatically.
; The vocabulary defines (among other things) what shape a policy request takes.
(load-policy (build-path margrave-home-path "tests" "conference1.p"))
(load-policy (build-path margrave-home-path "tests" "conference2.p"))
(load-policy (build-path margrave-home-path "tests" "phone1.p"))
(load-policy (build-path margrave-home-path "tests" "fwex1.p"))
(load-policy (build-path margrave-home-path "tests" "fwex1a.p"))
(load-policy (build-path margrave-home-path "tests" "fwex2.p"))
(load-policy (build-path margrave-home-path "tests" "happyrouterless.p"))
(load-policy (build-path margrave-home-path "tests" "happyroutermore.p"))

; Policies are loaded under the name given in their policy file;
; we'd like to change that and use some better names in these examples.
(display-response (mtext "rename conferencepolicy1 conf1"))
(display-response (mtext "rename conferencepolicy2 conf2"))
(display-response (mtext "rename fwex1 firewall1"))
(display-response (mtext "rename fwex1a firewall1a"))
(display-response (mtext "rename fwex2 firewall2"))
(display-response (mtext "rename happyrouterless HRless"))
(display-response (mtext "rename happyroutermore HRmore"))


; The mtext function executes a Margrave command, as seen in the
; RENAME statements above.

;; ********************************************
;; General examples
;; ********************************************

; In the basic conference policy: When can someone read a paper?  
(display-response (mtext "EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r)"))
; The EXPLORE statement defines a query. Now we need to get its results.

; First, are there ANY scenarios that satisfy the query?
(display-response (mtext "IS POSSIBLE?"))

; Yes there are. But how many?
(display-response (mtext "COUNT"))


; 7 solutions. What are they?
(printf "~a~n" (mtext "SHOW ALL"))

; (Margrave's result order isn't fixed, so these may be out of order.)
; * Both author and reviewer, and assigned to the paper
; * Both author and reviewer, both conflicted and assigned
; * Just a reviewer, both conflicted and assigned
; * Just a reviewer, neither conflicted nor assigned
; * Just a reviewer, only assigned
; * Just an author, neither conflicted nor assigned
; * Both author and reviewer, neither conflicted nor assigned

(pause-for-user)
;; ********************************************


;; Want to continue to refine? Restate the same query, using the PUBLISH clause.
;; The PUBLISH clause gives an ordering on the variables so you can use previous
;; queries in new ones.

(display-response (mtext "EXPLORE readpaper(a) and paper(r) and conf1:permit(s,a,r) PUBLISH s,a,r"))

; Now we can refine that query.
; The LAST identifier means the last query created via EXPLORE.
(display-response (mtext "EXPLORE reviewer(s) and last(s,a,r)"))

; How many of the solutions involve a reviewer?
(display-response (mtext "COUNT"))

; Now there are 6 solutions. We ruled out this one:
; * Just an author, neither conflicted nor assigned
(pause-for-user)
;; ********************************************



; Ok, so when is reading a paper NOT permitted?
(display-response (mtext "EXPLORE readpaper(a) and paper(r) and subject(s) and not conf1:permit(s, a, r)"))

; Let's just ask for the first solution:
(printf "~a~n" (mtext "SHOW ONE"))

; And the second:
(printf "~a~n" (mtext "SHOW NEXT"))

; When no solutions remain, SHOW NEXT will give a 
; "no more solutions" message.

(pause-for-user)
;; ********************************************




; Ok, so we can ask the most basic of questions:
; "When does this policy render that decision?"

; How about something more complex:
; "When can a reviewer read ALL papers?" 

; Like in Datalog, we get universals by defining subqueries and negating them.

; First query: There exists a reviewer who cannot read some paper.
(display-response (mtext "EXPLORE reviewer(s) AND readpaper(a) AND paper(r)
            AND NOT conf1:Permit(s, a, r)
    PUBLISH s, a"))

; The PUBLISH clause lets us dictate which variables are available for
; later use, and what order to use them in. Un-PUBLISHED variables are
; bound within the query and can be negated as seen below.

; You can always see what parameters a saved query takes, and in what order:
(display-response (mtext "INFO last"))

; And rename "last" to something more meaningful
(display-response (mtext "RENAME LAST notallowed"))

; Query: There is a reviewer to whom the last query does not apply."
(display-response (mtext "EXPLORE reviewer(s) AND readpaper(a) AND NOT notallowed(s, a)"))

; Uncomment this line to see all the solutions
;(printf "~a~n" (mtext "SHOW ALL"))
(display-response (mtext "COUNT"))

; That's odd: there are two solutions where there seem to be no papers
; at all! The policy's vocabulary says that all scenarios involve some 
; *Resource*, but we never said there had to be a *Paper*,
; and any scenario with no papers satisfies this query vacuously.
(pause-for-user)

; Let's remove those confusing solutions:
(display-response (mtext "EXPLORE reviewer(s) AND readpaper(a) AND NOT notallowed(s, a) AND paper(p)"))
(display-response (mtext "COUNT"))

; That query forces some paper to exist, which
; weeds out the two vacuous solutions from before.

(pause-for-user)
;; ********************************************



; A major feature of Margrave is change-impact analysis: Discovering all
; situations in which 2 policies will render different decisions.
; Suppose the two policies are Conference1 and Conference2: Conference2
; has a rule that Conference1 lacks.

; You can create your own change-impact query like so:

(display-response (mtext "EXPLORE (conf1:Permit(sub, act, res) AND NOT conf2:Permit(sub, act, res)) OR
            (conf2:Permit(sub, act, res) AND NOT conf1:Permit(sub, act, res)) OR
            (conf1:Deny(sub, act, res) AND NOT conf2:Deny(sub, act, res)) OR
            (conf2:Deny(sub, act, res) AND NOT conf1:Deny(sub, act, res))
            PUBLISH sub, act, res"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ALL"))

(printf "That was the manual change impact.~n")
(pause-for-user)

; Margrave can also make a change-impact query for you via the COMPARE command:
; Not yet in for first version; coming soon. (See Issues on github.) -- TN
;(mtext "COMPARE conf1 conf2")
;(mtext "COUNT")
;(mtext "get one")
;(newline) (display "That was the change impact via COMPARE.") (newline)
;(pause-for-user)

; the ordering used by LAST is now the standard ordering given by the policys' vocabulary.
; To confirm:
(display-response (mtext "INFO last"))

; And of course, change-impact queries can always be re-used via the LAST keyword.
(display-response (mtext "EXPLORE author(sub) AND last(sub, act, res)"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ALL"))

; A word of caution: If you use your own queries instead of the CHANGE command,
; be sure to provide a PUBLISH keyword, or the ordering may not be standard!

(pause-for-user)






; ***********************************************************************************************

; You aren't restricted to simple "Permit" and "Deny" either. This
; example phone company policy has 3 different decisions: TollFree,
; Toll, and Refuse.

; When can someone make a toll-free call?
(display-response (mtext "EXPLORE Phone1:TollFree(ncaller, nreceive)"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ONE"))
(pause-for-user)






;; **********************************************************************************************
; Example 1: Simple firewall policy, no state
;; **********************************************************************************************


; What sorts of incoming packets do we allow through?
(display-response (mtext "EXPLORE Firewall1:Accept(ipsrc, ipdest, portsrc, portdest, pro)"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ONE"))
(pause-for-user)




; That is a lot of packet types (18), and so not very useful. Let's narrow the scope a bit.
; What packets are allowed through using TCP to port 21? (Should be 9.)
(display-response (mtext "EXPLORE Firewall1:Accept(ipsrc, ipdest, portsrc, portdest, pro)
            AND TCP(pro) AND port21(portdest)"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ONE"))
(pause-for-user)





; Now suppose we changed this policy a bit. What has changed?
; (This doesn't break out by what *kind* of change. Can run separate queries if needed.)
;(display-response (mtext "COMPARE Firewall1 Firewall1a"))
;(display-response (mtext "COUNT"))
;(printf "~a~n" (mtext "SHOW ONE"))
;(pause-for-user)

; Above is not yet in; could do a manual change-impact here.
; TODO: update when compare keyword is added. -TN



;; *********************************************************************************************
; Example 2: Simple stateless firewall policy, with ip ranges
;; **********************************************************************************************

; What packets are allowed through using TCP to port 21?
(display-response (mtext "EXPLORE Firewall2:Accept(ipsrc, ipdest, portsrc, portdest, pro) and tcp(pro) and port21(portdest)"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ONE"))

; Zero solutions.
; Nothing can connect to port 21 because of a misordering of rules.
(pause-for-user)







; *********************************************************************************************
; Example 3: Sample ACL config from HappyRouter.com's Cisco ACL tutorial
; http://happyrouter.com/free-video-harden-your-cisco-router-with-ios-aclS

; When can a host on the less secure network have a 2-way TCP conversation with port 80 on the PC?
(display-response (mtext "EXPLORE HRLess:Accept(ipoutside, ipinside, portoutside, portinside, pro)
    and HRMore:Accept(ipinside, ipoutside, portinside, portoutside, pro)
    and tcp(pro) and http(portinside) and 10network(ipoutside) and pc(ipinside)"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ONE"))
(pause-for-user)



; For large firewalls, it may help to turn on the TUPLING optimization:
(display-response (mtext "EXPLORE HRLess:Accept(ipoutside, ipinside, portoutside, portinside, pro)
    and HRMore:Accept(ipinside, ipoutside, portinside, portoutside, pro)
    and tcp(pro) and http(portinside) and 10network(ipoutside) and pc(ipinside)
    TUPLING"))
(display-response (mtext "COUNT"))
(printf "~a~n" (mtext "SHOW ONE"))

; Tupling often results in fewer solutions, but those solutions still
; characterize all possible satisfying scenarios for the query.

;(pause-for-user)

; Close down the Margrave engine.
;(stop-margrave-engine)
; Unless you want to keep running queries!