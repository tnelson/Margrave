(Theory partialfunction
        (Vocab partialfunction
               (Types
                Subject                            
                (Action > SubmitPaper ReadPaper SubmitReview)
                (Resource > Paper Review TechReport))
               (Predicates
                
                (conflicted Subject Paper)
                (assigned Subject Paper)
                
                ; Author and reviewer can overlap when declared as predicates
                (author Subject)
                (reviewer Subject)
                (techreportfor Paper TechReport)
                ))
        (Axioms 
         (partial-function techreportfor)
         
         ;(abstract Subject)
         (abstract Action)
         (abstract Resource)))        	    	     
