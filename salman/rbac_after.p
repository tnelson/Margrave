(Policy uses rbac        
        (Variables
         (Variable u User)         
         (Variable p Permission))
        (Rules 
         
         (AfterPermission = (permit u p) :- (exists newrole Role 
                                                    (and (hasRole u newrole) (hasPermission newrole p)))))
        (RComb (over permit deny)))