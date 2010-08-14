#lang racket

; TEMPORARY! Will be a nice module path soon.
(require ;(file "./read.rkt")
         "margrave.rkt"
         "margrave-xml.rkt"
         "margrave-policy-vocab.rkt"
         xml)


; Welcome to Margrave! This file contains examples that are separated by (pause-for-user) so that you can
; execute it and read it in tandem. 


(start-margrave-engine)

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

(mtext "rename conferencepolicy1 conf1")
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
(mtext "GET ONE 0")


(mtext "EXPLORE conf1:permit(s, a, r) INCLUDE conf1:permit(s, a, r), conf1:deny(s, a, r) TUPLING")

; You don't need to pass an EXPLORE id if you're referencing the last explore:
(mtext "GET ONE")
(mtext "SHOW POPULATED conf1:permit(s, a, r), conf1:deny(s, a, r), assigned(s, r)")
(mtext "SHOW UNPOPULATED conf1:permit(s, a, r), conf1:deny(s, a, r), assigned(s, r)")
(mtext "SHOW POPULATED conf1:permit(s, a, r), conf1:deny(s, a, r), assigned(s, r) FOR CASES assigned(s, r), conf1:deny(s, a, r)")


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
;(mtext "show populated 0 mypol:rule1(x, y), mypol:rule2(x, y) for cases mypol:rule1_applies(x, y), mypol:rule2_applies(x, y)")

;(mtext "info myvoc")

(mtext "GET QUALIFIED RULES IN conf1")
;(stop-margrave-engine)



