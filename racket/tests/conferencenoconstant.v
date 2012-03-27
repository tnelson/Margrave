(Theory conferencenoconstant
        (Vocab conferencenoconstant
               (Types
                Subject                            
                (Action > SubmitPaper ReadPaper SubmitReview)
                (Resource > Paper Review TechReport))
               (Predicates
                
                (conflicted Subject Paper)
                (assigned Subject Paper)
                
                ; Author and reviewer can overlap when declared as predicates
                (author Subject)
                (reviewer Subject))
               
               ; NO CONSTANT in this vocabulary. Used to test vacuity.
               ;(Constants ('margravepaper Paper))
               (Functions (techreportfor Paper TechReport)))
        (Axioms 
         ;(abstract Subject)
         (abstract Action)
         (abstract Resource)))        	    	     
