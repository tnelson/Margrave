(Theory rbac 
        (Vocab rbac
               (Types
                User
                Role
                Permission
                ; Test for subtype declarations
                (Refreshment > Food Beverage))
               
               (Predicates
                (hadRole User Role)
                (hasRole User Role)
                (hadPermission Role Permission)
                (hasPermission Role Permission))
               
               (Constants 
                ('tim User)
                ('kathi User)
                ('salman User)
                ('submitPaper Permission)
                ('teach Permission)
                ('enterLab Permission)
                ('grade Permission)
                ('takeExam Permission)
                ('professor Role)
                ('student Role)
                ('ta Role)
                ('ra Role)))
        
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
        