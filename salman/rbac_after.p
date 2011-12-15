(Policy uses rbac        
        (Variables
         (Variable u User)         
         (Variable p Permission))
        (Rules 
         
         (AfterPermission = (permit u p) :- (exists r Role 
                                                    (and (hasRole u r) (hasPermission r p)))))
        (RComb (over permit deny)))