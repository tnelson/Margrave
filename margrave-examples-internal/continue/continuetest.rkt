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
             
             ; There is a SINGLE paper that's been bid on AND assigned, and has been reviewed.
             ; Also the decision on that paper is not undecided.
             ; And its authors aren't reviewers...
             (exists p Paper (and (forall auth User (implies (authorOf p auth)
                                                             (not (reviewer auth))))
                                  (exists u User (exists rev Review (and (forall p2 Paper (= p p2))
                                                                         (bid u p)         
                                                                         (reviewOn u p rev)                                                                    
                                                                         (not (= (decisionIs p) 'undecided))
                                                                         )))))
                          
             
             )
      ; #:under '( "continue")
       #:ceiling '([univ 38]                   
                   [Object 6] ;<-- don't go above 5 unless "needed" by a subsort.
                   [User 2] ; need >1 for admin plus an author. term counting normally takes care of this...
                   [Action 16]
                   [Paper 1]
                   [Review 1]
                   [Decision 2] ; <-- need to allow for a non-undecided decision
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

