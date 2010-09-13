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

; TEMPORARY! Will be a nice module path soon.
(require ;(file "./read.rkt")
         "margrave.rkt"
         "margrave-xml.rkt"
         "margrave-policy-vocab.rkt"
         xml)


; Welcome to Margrave! This file contains examples that are separated by (pause-for-user) so that you can
; execute it and read it in tandem. 


(start-margrave-engine (current-directory) '() '("-log"))

;(load-policy (build-path (current-directory) "tests" "extconference.p"))
(load-policy (build-path (current-directory) "tests" "conference1.p"))
;(load-policy (build-path (current-directory) "tests" "conference2.p"))
;(load-policy (build-path (current-directory) "tests" "phone1.p"))
(load-policy (build-path (current-directory) "tests" "fwex1.p"))
;(load-policy (build-path (current-directory) "tests" "fwex1a.p"))
;(load-policy (build-path (current-directory) "tests" "fwex2.p"))
;(load-policy (build-path (current-directory) "tests" "fwex3.p"))
;(load-policy (build-path (current-directory) "tests" "happyrouterless.p"))
;(load-policy (build-path (current-directory) "tests" "happyroutermore.p"))
;(load-policy (build-path (current-directory) "tests" "iout.p"))
(load-policy (build-path (current-directory) "tests" "subset.p"))

;(mtext "info")
;(mtext "info fwex1")
;(mtext "info examplefw1")
;(m (xml-make-rename-command "conferencepolicy1" "conf1"))
;(m (xml-make-rename-command "conferencepolicy2" "conf2"))
;(m (xml-make-rename-command "fwex1" "firewall1"))
;(m (xml-make-rename-command "fwex1a" "firewall1a"))
;(m (xml-make-rename-command "fwex2" "firewall2"))
;(m (xml-make-rename-command "happyrouterless" "HRless"))
;(m (xml-make-rename-command "happyroutermore" "HRmore"))

(display-response (mtext "rename conferencepolicy1 conf1"))
;(mtext "rename conferencepolicy2 conf2")
;(mtext "rename fwex1 firewall1")
;(mtext "rename fwex1a firewall1a")
;(mtext "rename fwex2 firewall2")
;(mtext "rename happyrouterless HRless")
;(mtext "rename happyroutermore HRmore")

;(mtext "info")
;(mtext "info conf1")

; Mypol doesn't exist: expect an error on UNDER clause.
;(define test-string "EXPLORE xsort(x) AND xsort(y) UNDER mypol INCLUDE mypol:rule1(x, y), mypol:rule2(x, y), mypol:rule1_applies(x, y), mypol:rule2_applies(x, y) TUPLING")
;(define test-stream (open-input-string test-string))
;(define test-xml (evalxml test-string))
;(m test-xml)

(define theid (mtext "EXPLORE conf1:permit(s, a, r) INCLUDE conf1:permit"))
;(printf "Id: ~a~n" (xml-explore-result->id theid))
(display-response (mtext "GET ONE"))


(display-response (mtext "EXPLORE conf1:permit(s, a, r) INCLUDE conf1:permit(s, a, r), conf1:deny(s, a, r) DEBUG 3 TUPLING"))

; You don't need to pass an EXPLORE id if you're referencing the last explore:
(display-response (mtext "GET ONE"))
(display-response (mtext "SHOW REALIZED conf1:permit(s, a, r), conf1:deny(s, a, r), assigned(s, r)"))
(display-response (mtext "SHOW UNREALIZED conf1:permit(s, a, r), conf1:deny(s, a, r), assigned(s, r)"))
(display-response (mtext "SHOW REALIZED conf1:permit(s, a, r), conf1:deny(s, a, r), assigned(s, r) FOR CASES assigned(s, r), conf1:deny(s, a, r)"))


;(mtext "EXPLORE firewall1:accept(ipsrc, ipdest, portsrc, portdest, pro) INCLUDE firewall1:accept(ipsrc, ipdest, portsrc, portdest, pro) TUPLING")
;(mtext "GET ONE 0")

;(mtext "EXPLORE firewall1:accept(ipsrc, ipdest, portsrc, portdest, pro) INCLUDE firewall1:accept")
;(mtext "GET ONE 0")

;(mtext "create vocabulary myvoc")
;(mtext "add to myvoc sort xsort")
;(mtext "add to myvoc subsort xsort s1")
;(mtext "add to myvoc subsort xsort s2")
;(mtext "add to myvoc decision permit")
;(mtext "add to myvoc decision deny")
;(mtext "add to myvoc requestvar x xsort")
;(mtext "add to myvoc requestvar y xsort")

;(mtext "create policy leaf mypol myvoc")
;(mtext "add rule to mypol rule1 permit (s1 x) (s2 y)")
;(mtext "add rule to mypol rule2 deny (s2 x) (s1 y)")
;(mtext "prepare mypol")

;(mtext "explore xsort(x) and xsort(y) UNDER mypol INCLUDE mypol:rule1(x, y), mypol:rule2(x, y), mypol:rule1_applies(x, y), mypol:rule2_applies(x, y) tupling")
;(mtext "show REALIZED 0 mypol:rule1(x, y), mypol:rule2(x, y) for cases mypol:rule1_applies(x, y), mypol:rule2_applies(x, y)")

;(mtext "info myvoc")

(display-response (mtext "GET QUALIFIED RULES IN conf1"))


(display-response (mtext "EXPLORE subject(x) and subject(y) and subject(z) and x=y and y=z UNDER conf1 TUPLING"))
;(display-response (mtext "EXPLORE subject(x) and notasort(y) and subject(z) and x=y and y=z UNDER conf1 TUPLING"))
;(display-response (mtext "EXPLORE subject(x) and notasort(y) and subject(z) and x=y and y=z TUPLING"))
(display-response (mtext "GET ONE"))
(printf "~a~n" (mtext "SHOW ONE"))
(printf "~a~n" (mtext "SHOW ONE 0"))
(printf "~a~n" (mtext "GET ALL"))
(printf "~n~n~a~n" (mtext "SHOW ALL"))


(display-response (mtext "EXPLORE NOT subject (x) AND x=y and NOT y=z UNDER conf1"))
; Should display x, y, and z anyway.
(printf "~a~n" (mtext "SHOW ONE"))

; No sort inference can be done here. Only know 2 variables x and y.
; Ceiling should be 2, both assertions should be UNIV.
(display-response (mtext "EXPLORE x = y UNDER subspolicy DEBUG 3 TUPLING"))
;(display-response (mtext "EXPLORE x = y AND NOT potato(x) AND NOT tool(x) UNDER subspolicy"))
(printf "~a~n" (mtext "SHOW ALL"))

;(stop-margrave-engine)


(display-response (mtext "EXPLORE conf1:Deny(s, a, r) AND 
        reviewer(s) AND paper(r) AND readpaper(a)"))
(printf "~a~n" (mtext "SHOW ALL"))


