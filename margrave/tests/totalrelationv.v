(PolicyVocab totalrelationv
             (Types
              (Subject : Author Reviewer)
              (Action : SubmitPaper ReadPaper SubmitReview)
              (Resource : Paper Review))
             (Decisions 
              Permit
              Deny)
             (Predicates
              (Conflicted : Reviewer Paper)
              (Assigned : Reviewer Paper))

	     (ReqVariables (s : Subject)
                           (a : Action)
                           (r : Resource))
             (OthVariables )
             (Constraints
              ; test total-relation
              ; every reviewer is assigned to at least one paper
              (total-relation Assigned)              
              
	      ; A resource is never BOTH a paper and a review. An action is never another action type.
              (disjoint-all Resource)
              (disjoint-all Action)
	      
              ; Efficiency constraint: Never assign more than one atom to any action type. (They are "atomic.")
              (atmostone-all Action)

	      ; Specify that there is no S, A, or R outside our declared subtypes
	      (abstract Subject)
	      (abstract Action)
              (abstract Resource)

	      ; Weed out vacuous solutions: Require there to be some Subject and some Resource.
              (nonempty Subject)
              (nonempty Resource)))
