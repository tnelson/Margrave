(Policy uses rbac        
        (Variables
         (u User)
         (p Permission))
        (Rules 
	  (BeforePermission = (permit u p) :- (exists oldrole Role 
                                                      (and (hadRole u oldrole) (hadPermission oldrole p))))
          ;(TestConstantsInPolicy = (deny u p) :- (hadRole 'kathi 'student)))
          )
        (RComb (over permit deny)))