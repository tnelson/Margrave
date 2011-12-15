(Theory rbac 
        (Vocab rbac
               (Types
                (Type User)
                (Type Role)
                (Type Permission))
               
               (Predicates
                (Predicate hadRole User Role)
                (Predicate hasRole User Role)
                (Predicate hadPermission Role Permission)
                (Predicate hasPermission Role Permission))
               
               (Constants 
                (Constant 'tim User)
                (Constant 'kathi User)
                (Constant 'salman User)
                (Constant 'submitPaper Permission)
                (Constant 'teach Permission)
                (Constant 'enterLab Permission)
                (Constant 'grade Permission)
                (Constant 'takeExam Permission)
                (Constant 'professor Role)
                (Constant 'student Role)
                (Constant 'ta Role)
                (Constant 'ra Role)))
        
        (Axioms 
               ; TODO: java support for custom axioms, e.g.
         ;          (forall arole Role (or (= arole 'professor) (= arole 'student) (= arole 'ta) (= arole 'ra)))
         ;(singleton-all User)
         ;(atmostone-all Role)
         ;(atmostone-all Permission)
         ;(abstract User)
         ;(abstract Role)
         ;(abstract Permission)
         ))
        