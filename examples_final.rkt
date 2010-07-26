#lang racket

; updated to new EXPLORE query language 04/2010 tn

; TEMPORARY! Will be a nice module path soon.
(require (file "./read.rkt")
         (file "./margrave.rkt")
         xml
         parser-tools/lex)

; Welcome to Margrave! This file contains examples that are separated by (pause-for-user) so that you can
; execute it and read it in tandem. 


(start-margrave-engine)

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

;(m (xml-make-rename-command "conferencepolicy1" "conf1"))
;(m (xml-make-rename-command "conferencepolicy2" "conf2"))
;(m (xml-make-rename-command "fwex1" "firewall1"))
;(m (xml-make-rename-command "fwex1a" "firewall1a"))
;(m (xml-make-rename-command "fwex2" "firewall2"))
;(m (xml-make-rename-command "happyrouterless" "HRless"))
;(m (xml-make-rename-command "happyroutermore" "HRmore"))

(mtext "rename conferencepolicy1 conf1")
(mtext "rename conferencepolicy2 conf2")
(mtext "rename fwex1 firewall1")
(mtext "rename fwex1a firewall1a")
(mtext "rename fwex2 firewall2")
(mtext "rename happyrouterless HRless")
(mtext "rename happyroutermore HRmore")

(mtext "info")
(mtext "info conf1")

; Mypol doesn't exist: expect an error on UNDER clause.
;(define test-string "EXPLORE xsort(x) AND xsort(y) UNDER mypol IDBOUTPUT mypol:rule1(x, y), mypol:rule2(x, y), mypol:rule1_applies(x, y), mypol:rule2_applies(x, y) TUPLING")
;(define test-stream (open-input-string test-string))
;(define test-xml (evalxml test-string))
;(m test-xml)

; Parser problem: expects list of atomic formulas, and atomic formulas have to have non-empty var vectors
;(define test-string "EXPLORE conf1:permit(s, a, r) IDBOUTPUT conf1:permit")
;(mtext test-string)

;

; Won't tuple since conf1 has >1-ary predicates. (Will be enhancing to allow any arity soon.)
;(define test-string "EXPLORE conf1:permit(s, a, r) IDBOUTPUT conf1:permit(s, a, r) TUPLING")
;(mtext test-string)
;(mtext "GET ONE 0")



(define test-string2 "EXPLORE subject(s) UNDER conf1")
(mtext test-string2)





(stop-margrave-engine)



