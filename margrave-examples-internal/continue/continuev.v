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
                
                ; These phases could be unary predicates + custom axioms, right? would reduce # of atoms
                
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
         ; Never more than one conference at a time
         ; (To allow >1, would need to add to the arities of the preds above)
         (atmostone Conference)
         
         ; Object is either a User or a Resource
         (abstract Object)
         
         ; There is some administrator (constraint on _predicate_)
         (nonempty admin)
                  
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
         ; !!! exception
         (partial-function reviewOn)
         
               
               ; ASSUMPTIONS 
               ; Use in eval: assumption, not axiom. Relates to system guarantees
               
               ; (1) AssignedTo is disjoint from Conflict
               ;    "No reviewer can be assigned to papers they are conflicted on."
               ; (disjoint assignedTo conflicted)
               ; (2) ReviewOn implies AssignedTo
               ;    "Nobody can submit a review on something they are not assigned to." 
               ; (subset reviewOn assignedTo)  ; check order!
         ))   