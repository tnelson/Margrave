#lang racket

(require "../../margrave/margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../../margrave")

(m-load-policy "continue" "continuep.p")

(m-let "Q1" '([s User]
              [a Action]
              [r Object])
       '(and ([continue permit] s a r)
             (= 'advancePhase a)             
             (isa r Conference (= (conferencePhase r) 'cDiscussion))
             (exists p Paper true)
             (forall u User (forall p Paper (not (conflicted u p))))
             )
       #:ceiling '([univ 9]
                   [User 2] ; need >1 for admin plus an author. term counting normally takes care of this...
                   [Action 16]
                   [Paper 1]
                   [Review 1]
                   [Decision 1]
                   [Conference 1]
                   [ConferencePhase 9]
                   [PaperPhase 6]
                   [ConferenceInfo 1]))
(check-true (m-scenario? (m-get "Q1")))
;(m-get "Q1")

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




