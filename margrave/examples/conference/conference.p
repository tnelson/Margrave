(Policy uses Conference
        (Variables 
         (Variable s Subject)
         (Variable a Action)
         (Variable r Resource)
         (Variable p Paper))
        
        (Rules 
         ; "During the review phase, reviewer r may submit a review for paper p if r is assigned to p."
         (REVSubmitAssigned = (permit s a r) :- (assigned s r) (SubmitReview a) (Paper r))
         ; "During the review phase, a reviewer r can read scores for paper p if r has submitted a revew for p."
         (REVReadScoresSubmitted = (permit s a r) :- (exists p Paper 
                                                             (and (submittedReview s p)
                                                                  (ReadScore a) 
                                                                  (scoreFor p r))))         
         )
        
        (RComb (fa permit deny)))

