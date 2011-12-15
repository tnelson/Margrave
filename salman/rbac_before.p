(Policy uses rbac        
        (Variables
         (Variable u User)
         (Variable p Permission))
        (Rules 
	  (BeforePermission = (permit u p) :- (exists r Role 
                                                      (and (hadRole u r) (hadPermission r p))))
          ;(TestConstantsInPolicy = (deny u p) :- (hadRole 'kathi 'student)))
          )
        (RComb (over permit deny)))