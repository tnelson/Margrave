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
             ; In discussion phase
             (isa r Conference (= (conferencePhase r) 'cDiscussion))
             ; Force state to comply in a stateless query:
             (forall p Paper (= (paperPhase p) 'pDiscussion))
             (exists p Paper true)
             (forall u User (forall p Paper (not (conflicted u p))))
             )
       #:ceiling '(;[univ 9]
                   [User 2] ; need >1 for admin plus an author. term counting normally takes care of this...
                   [Action 16]
                   [Paper 1]
                   [Review 0]
                   [Decision 1]
                   [Conference 1]
                   [ConferencePhase 9]
                   [PaperPhase 6]
                   [ConferenceInfo 0]))
(check-true (m-scenario? (m-get "Q1")))
;(m-get "Q1")
(display (m-scenario->string 
          (m-get "Q1" #:include '(([continue permit] s a r) 
                                  ([continue rule755_applies] s a r)                                    
                                  ([continue ruleFinal_applies] s a r)
                                  ([continue ruleFinal_matches] s a r)
                                  
                                  ([continue rule709_applies] s a r)
                                  ([continue rule713_applies] s a r)
                                  ([continue rule717_applies] s a r)
                                  ([continue rule707otherwise_applies] s a r)
                                  ([continue rule725_applies] s a r)
                                  ([continue rule729_applies] s a r)
                                  ([continue rule740_applies] s a r)
                                  ([continue rule747_applies] s a r)
                                  ([continue rule755_applies] s a r)
                                  ([continue rule725otherwise_applies] s a r)
                                  ))))


