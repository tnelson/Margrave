(Vocab Conference
             (Types
              (Type Subject > Author Reviewer)
              (Type Author > LBAuthorReviewer)
              
              ;; Allow reviewers to be authors
              (Type Reviewer > LBAuthorReviewer)
              
              (Type Action > SubmitPaper ReadPaper SubmitReview)
              (Type Resource > Paper Review))
             (Predicates
              (Predicate conflicted Reviewer Paper)
              (Predicate assigned Reviewer Paper))
             (Constants (Constant c Resource))
             (Functions (Function f Resource Resource)))

	      


	      ; Weed out vacuous solutions: Require there to be some Subject and some Resource.
;              (nonempty Subject)
;              (nonempty Resource)))

	      ; Specify that there is no S, A, or R outside our declared subtypes;
	      ;(abstract Subject)
	      ;(abstract Action)
              ;(abstract Resource)
