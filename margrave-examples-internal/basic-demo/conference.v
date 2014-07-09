(Theory conference
        (Vocab conference
               (Types
                Subject
                (Action > SubmitPaper ReadPaper SubmitReview)
                (Resource > Paper Review TechReport))
               (Predicates

                (conflicted Subject Paper)
                (assigned Subject Paper)

                ; Author and reviewer can overlap when declared as predicates
                (author Subject)
                (reviewer Subject)))
        (Axioms
         ; Every Resource is a Paper, a Review, or a TechReport
         (abstract Resource)
         ; Similarly for Action
         (abstract Action)))
