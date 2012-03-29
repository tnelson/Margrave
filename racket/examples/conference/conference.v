(Theory Conference
        (Vocab Conference
               (Types
                (Subject > Author Reviewer)                
                (Action > SubmitPaper ReadPaper SubmitReview ReadScore)
                (Resource > Paper Score))
               
               (Predicates
                (conflicted Reviewer Paper)
                (assigned Reviewer Paper)
                (submittedReview Reviewer Paper)
                (scoreFor Paper Score)
                ))

        (Axioms
         ; Weed out vacuous solutions: Require there to be some Subject and some Resource.
         (nonempty Subject)
         (nonempty Resource)
         
         ; Specify that there is no S, A, or R outside our declared subtypes;
         (abstract Subject)
         (abstract Action)
         (abstract Resource)))
