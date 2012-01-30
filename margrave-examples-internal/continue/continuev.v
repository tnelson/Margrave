(Theory continuev
        (Vocab continuev
               
               (Constants
                ('readReview Action)
                ('writeReview Action)
                ('becomeAuthor Action)
                ('makeSubmission Action)
                ('modifySubmission Action)
                ('addReviewer Action)
                ('removeUser Action)
                ('modifyUserJob Action)
                ('modifyUserInfo Action)
                ('modifyUserPassword Action)
                ('paperBid Action)
                ('paperConflict Action)
                ('paperAssign Action)
                ('paperDecide Action)
                ('advancePhase Action)
                ('editConferenceInfo Action)                                
                
                ; Each paper is in exactly one phase       
                ('pSubmission PaperPhase)   
                ('pBidding PaperPhase)                             
                ('pAssignment PaperPhase)                                   
                ('pReviewing PaperPhase)                               
                ('pDiscussion PaperPhase)                                 
                ('pNotification PaperPhase)
                
                ('undecided Decision)
                
                ; The conference is in exactly one phase                                                                  
                ('cInitialization ConferencePhase)                                            
                ('cPreSubmission ConferencePhase)
                ('cSubmission ConferencePhase)                                            
                ('cBidding ConferencePhase)                                             
                ('cAssignment ConferencePhase)                                             
                ('cReviewing ConferencePhase)                                             
                ('cDiscussion ConferencePhase)                                             
                ('cNotification ConferencePhase)                                             
                ('cPublishing ConferencePhase)
                )
               
               (Functions
                (paperPhase Paper PaperPhase)
                (conferencePhase Conference ConferencePhase)
                (decisionIs Paper Decision))
               
               (Types                
                Action                 
                (Object > User Resource)
                (Resource > Paper ConferenceInfo Review Conference)  
                ConferencePhase
                PaperPhase
                Decision)
                                                             
               (Predicates
                
                ; These can overlap, so not types.
                (admin User)
                (reviewer User)
                (author User)                               
                
                ; validConference assertion restricts these sensibly
                ; (e.g. Reviewer instead of User)
                
                (authorOf Paper User) ; really paper->author
                (bid User Paper) ; really reviewer->paper
                (conflicted User Paper) ; really reviewer->paper
                (assignedTo User Paper) ; really reviewer->paper
                (reviewOn User Paper Review)) ; really reviewer->paper->review                        
               )
        
        (Axioms
         ; authorOf is really Paper x Author, etc.
         ; TODO: Example of how messy the type system is forcing axioms to be...
         (formula (forall p Paper (forall u User (implies (authorOf p u)
                                                          (author u)))))         
         (formula (forall u User (forall p Paper (implies (bid u p)
                                                          (reviewer u)))))
         (formula (forall u User (forall p Paper (implies (conflicted u p)
                                                          (reviewer u)))))
         (formula (forall u User (forall p Paper (implies (assignedTo u p)
                                                          (reviewer u)))))
         (formula (forall u User (forall p Paper (forall r Review (implies (reviewOn u p r)
                                                                           (reviewer u))))))
         
         ; Never more than one conference at a time
         ; (To allow >1, would need to add to the arities of the preds above)
         (atmostone Conference)
         
         ; Object is either a User or a Resource
         (abstract Object)
         
         ; There is some administrator (constraint on _predicate_)
         (nonempty admin)
                  
         ; All constants different! 
         (constants-neq-all PaperPhase)
         (constants-neq-all ConferencePhase)
         (constants-neq-all Action)
         
         ; admins cannot be authors                       
         (disjoint admin author)
         
         (abstract Resource)                           
         (abstract PaperPhase)                           
         (abstract ConferencePhase)                  
         (abstract Action)         
         
         ; Every paper has an author
         ; But papers can have more than one author (so not functional)         
         (total-relation authorOf)       
         
         ; Not everyone submits a review, and may be an early phase
         ; but at most one review
         (partial-function reviewOn)
         
               
         ; ASSUMPTIONS                   
         ; (1) AssignedTo is disjoint from Conflict
         ;    "No reviewer can be assigned to papers they are conflicted on."
         (disjoint assignedTo conflicted)
               
         ; (2) ReviewOn implies AssignedTo
         ;    "Nobody can submit a review on something they are not assigned to."                
         (formula (forall ux User
                          (forall px Paper
                                  (forall rx Review 
                                          (implies (reviewOn ux px rx)
                                                   (assignedTo ux px))))))
         
         ; (3) Added by Tim:
         ; Authors who are reviewers are automatically conflicted
         (formula (forall ux User 
                          (forall px Paper (implies (and (authorOf px ux) (reviewer ux))
                                                    (conflicted ux px)))))
         
         ; (Together (1) (2) and (3) prevent authors from reviewing their own paper.
         
         ))   