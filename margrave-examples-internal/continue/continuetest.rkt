#lang racket

(require "../../margrave/margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../../margrave")

(m-load-policy "continue" "continuep.p")

(m-let "Q1" '([s User]
              [a Action]
              [r Object])
       '([continue permit] s a r)
       #:ceiling '([univ 9]
                   [User 1]
                   [Action 1]
                   [Paper 1]
                   [Review 1]
                   [Decision 1]
                   [Conference 1]
                   [ConferencePhase 1]
                   [PaperPhase 1]
                   [ConferenceInfo 1]))
(check-true (m-scenario? (m-get "Q1")))
(m-get "Q1")

;(m-let "QTest" '()
;       'true
;       #:under '("continue"))
;(m-get "QTest")


;(m-let "Q2" '([s User]
;              [a Action]
;              [r Object])
;       '(and ([continue permit] s a r)
;             (not ([continue permit] s a r))))
;(check-true (m-unsat? (m-get "Q2")))

;(m-let "Q2" '([s User]
;              [a Action]
;              [r Object])
;       'true
;       #:under '( "continue")
;       #:debug 3)
;(m-get "Q2")

;#lang margrave
;
;LOAD POLICY continuep.p;
;EXPLORE continuep:permit(s,a,r);
;SHOW ONE;
;
;
;// For type inference testing (doesn't work yet)
;EXPLORE authorOf(x, y) under continuep;
;is possible?;  
;// should be true, is false
;EXPLORE authorOf(x, y) under continuep ceiling 4;
;is possible?;




