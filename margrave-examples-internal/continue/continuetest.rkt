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
             ; Advancing phase
             (= 'advancePhase a)             
             ; of a Conference             
             ; in discussion phase
             (isa r Conference (= (conferencePhase r) 'cDiscussion))
             
                          
             ; Force state to comply in a stateless query:
             
             ; All papers are past the Reviewing phase, into Notification
             (forall p Paper (= (paperPhase p) 'pNotification))
             ; There is a paper that's been bid on.
             (exists p Paper (exists u User (and (bid u p) (assignedTo u p))))
             
             (forall u User (forall p Paper (not (conflicted u p))))
             )
      ; #:under '( "continue")
       #:ceiling '([univ 36]                   
                   [Object 5] ;<-- don't go above 5 unless "needed" by a subsort.
                   [User 2] ; need >1 for admin plus an author. term counting normally takes care of this...
                   [Action 16]
                   [Paper 1]
                   [Review 0]
                   [Decision 1]
                   [Conference 1]
                   [ConferencePhase 9]
                   [PaperPhase 6]
                   [ConferenceInfo 0]))
;(check-true (m-scenario? (m-get "Q1")))
;(m-get "Q1")
                     ; #:include '(([continue permit] s a r) 
                     ;             ([continue rule755_applies] s a r)                                    
                     ;             ([continue ruleFinal_applies] s a r)
                     ;             ([continue ruleFinal_matches] s a r)
                     ;             
                     ;             ([continue rule709_applies] s a r)
                     ;             ([continue rule713_applies] s a r)
                      ;            ([continue rule717_applies] s a r)
                     ;             ([continue rule707otherwise_applies] s a r)
                     ;             ([continue rule725_applies] s a r)
                     ;             ([continue rule729_applies] s a r)
                     ;             ([continue rule740_applies] s a r)
                     ;             ([continue rule747_applies] s a r)
                     ;             ([continue rule755_applies] s a r)
                     ;             ([continue rule725otherwise_applies] s a r)
                     ;             )))
(display (m-scenario->string 
          (m-get "Q1" )))
(display (m-count-scenarios "Q1"))


