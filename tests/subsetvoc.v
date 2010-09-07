; Margrave is always a model-finder (enforced via constraint rather than sort hierarchy)
; Potatoes and Tools are always disjoint (top-level types always disjoint)

(PolicyVocab SubsetVoc
             (Types (Tool : ModelFinder Margrave )
                    (Potato : )
              ) 

             (Decisions 
               Permit
               Deny) 

             (Predicates )

	     (ReqVariables (t : Tool))
             (OthVariables )
             (Constraints
              (subset Margrave ModelFinder) ; no support for constraining predicates yet. 
              ))
