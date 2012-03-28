#lang racket

(require "../../racket/margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../../racket")

(m-load-policy "continue" "continuep.p")

; Taken from the policy:
(m-let "Framing" '()
       ; Whatever phase the conference is in, its papers must reflect that.
       '(and          
         (forall p Paper 
                 (and
                  ; to move into reviewing phase, every paper has to have a user assigned to it.
                  (implies (or (= (paperPhase p) 'pReviewing)
                               (= (paperPhase p) 'pDiscussion)
                               (= (paperPhase p) 'pNotification))
                           (exists u User (assignedTo u p)))
                  ; to move into the discussion phase, every paper needs to have at least one review
                  (implies (or (= (paperPhase p) 'pDiscussion)
                               (= (paperPhase p) 'pNotification))
                           (exists u User (exists rev Review (reviewOn u p rev))))
                  ; to move into the notification phase, a paper needs a decision.
                  (implies (= (paperPhase p) 'pNotification)
                           (exists d Decision (and (not (= 'undecided d)) 
                                                   (= d (decisionIs p)))))))      
         
         ; (decisionIs p d)
         ; ^^^ !!! TODO
         ; This should give an error in the *POLICY*. Why didn't it?
         
         (forall conf Conference 
                 (and  
                  ; No papers if the conference is in pre-submission or initialization
                  (implies (or (= (conferencePhase conf) 'cPreSubmission)
                               (= (conferencePhase conf) 'cInitialization))
                           (forall p Paper (not (= p p))))
                  
                  (implies (= (conferencePhase conf) 'cPublishing)
                           (and
                            ; condition for being past the bidding phase
                            (forall u User (implies (reviewer u)
                                                    (exists p Paper (bid u p))))
                            
                            ; papers meet the phase requirements: none of them lag behind
                            (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                                 (not (= (paperPhase p) 'pBidding))
                                                 (not (= (paperPhase p) 'pAssignment))
                                                 (not (= (paperPhase p) 'pReviewing))
                                                 (not (= (paperPhase p) 'pDiscussion))))))
                  
                  (implies (= (conferencePhase conf) 'cDiscussion)
                           (and
                            ; condition for being past the bidding phase
                            (forall u User (implies (reviewer u)
                                                    (exists p Paper (bid u p))))
                            
                            ; papers meet the phase requirements: none of them lag behind
                            (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                                 (not (= (paperPhase p) 'pBidding))
                                                 (not (= (paperPhase p) 'pAssignment))
                                                 (not (= (paperPhase p) 'pReviewing))))))
                  
                  (implies (= (conferencePhase conf) 'cReviewing)
                           (and 
                            ; condition for being past the bidding phase
                            (forall u User (implies (reviewer u)
                                                    (exists p Paper (bid u p))))
                            ; papers meet the phase requirements: none of them lag behind
                            (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                                 (not (= (paperPhase p) 'pBidding))
                                                 (not (= (paperPhase p) 'pAssignment))))))
                  
                  (implies (= (conferencePhase conf) 'cAssignment)
                           (and 
                            ; condition for being past the bidding phase
                            (forall u User (implies (reviewer u)
                                                    (exists p Paper (bid u p))))
                            ; papers meet the phase requirements: none of them lag behind
                            (forall p Paper (and (not (= (paperPhase p) 'pSubmission))
                                                 (not (= (paperPhase p) 'pBidding))))))
                  
                  (implies (= (conferencePhase conf) 'cBidding)
                           (forall p Paper (not (= (paperPhase p) 'pSubmission)))))))
       #:under '("continue") )

(m-let "Q1" '([s User]
              [a Action]
              [r Object])
       '(and 
         
         ; The request is permitted         
         ([continue permit] s a r)
         
         ; paperBid has a negative condition. (Can't bid if conflicted.)
         ; So it's a good start.
         ;; 12 solns: author who is also a reviewer trying to bid on something
         ;; some of these have the author of p trying to bid on his/her own work!
         (author s)
                                    
         (= a 'paperBid)
         
        ; ([Framing])
         
         ;;; axioms not in framing, tailored for THIS query to save time
         ;;; also some restrictions on the query space to reduce # solns
         ; no reviews yet, since we're still bidding
         (forall p Paper (forall u User (forall rev Review (not (reviewOn u p rev)))))             
         ; paper decisions still unfixed
         (forall p Paper (= (decisionIs p) 'undecided))     
         ; Nobody has been assigned yet (still bidding)
         (forall p Paper (forall u User (not (assignedTo u p))))
         
         ; some paper has been submitted
         (exists p Paper true )   
         
         ; the conference is in the bidding phase
         (exists conf Conference (= 'cBidding (conferencePhase conf)))
         
         ; the admin is not conflicted on anything
         ; the admin is not bidding on this paper
         ; the admin is not a reviewer
         (forall u User (forall p Paper (implies (admin u) 
                                                 (and 
                                                  (not (reviewer u))
                                                  (not (bid u p))
                                                  (not (conflicted u p))))))                                    
         
         ; This user doesn't already have a bid in on this paper
         (isa r Paper (not (bid s r)))                  
         )
       
       ; !!! TODO. Observation: managing these is a PAIN.
       #:ceiling '([univ 37]                   
                   [Object 6] ;<-- don't increase unless increasing subsort
                   [User 3] ; an admin, an author, a reviewer (who isn't conflicted --> can't be the author)
                   [Action 16]
                   [Paper 1]
                   [Review 1]
                   [Decision 2] ; <-- need to allow for a non-undecided decision
                   [Conference 1]
                   [ConferencePhase 9]
                   [PaperPhase 6]
                   [ConferenceInfo 0]))
;(check-true (m-scenario? (m-get "Q1")))
(define result (m-get "Q1"
                      #:include '(([continue rule810_matches] s a r)
                                  ([continue rule810_applies] s a r))))

;(display (m-scenario->string 
;          result))
;(display (m-count-scenarios "Q1"))
;(save-all-scenarios "Q1" #:brief #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The "Simple sanity checks" from the Alloy spec are all stateful...

; Confirm: A paper's author may not see the identity of 

;(m-let "Q2" '([s User]              
;              [r User])
;       '(and                   
;         (admin s) 
;         ;(not ([continue permit] s 'modifyUserPassword r)))
;         ([continue permit] s 'modifyUserPassword r))
;       #:debug 3)
;(check-true (m-scenario? (m-get "Q1")))
;(time (display (m-show "Q2")))


