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
         (abstract User)
         (abstract Role)
         (abstract Permission)
         (constants-neq-all User)
         (constants-neq-all Role)
         (constants-neq-all Permission)
         
         ;(singleton-all User)
         ;(atmostone-all Role)
         ;(atmostone-all Permission)
         ;(abstract User)
         ;(abstract Role)
         ;(abstract Permission)
         ))
        