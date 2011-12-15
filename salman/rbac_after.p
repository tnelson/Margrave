(Policy uses rbac        
        (Variables
         (Variable u User)         
         (Variable p Permission))
        (Rules 
         
         (AfterPermission = (permit u p) :- (exists r Role 
                                                    (and (hadRole u r) (hadPermission r p)))))
        (RComb (over permit deny)))