(Theory small-conference
        (Vocab small-conference
               (Types
                Subject                            
                (Action > ReadPaper )
                (Resource > Paper Review TechReport))
               (Predicates
                
                (conflicted Subject Paper)
                (assigned Subject Paper)
                
                ; Author and reviewer can overlap when declared as predicates
                (author Subject)
                (reviewer Subject))
                              
               (Functions (techreportfor Paper TechReport)))
        (Axioms 
         ;(abstract Subject)
         (abstract Action)
         (abstract Resource)))        	    	     
