; Semi-stateful example due to Paul Anderson
; TN

(Policy uses pa-state
        (Variables
         (t ReqTime))
        (Rules
         ;(*) Only Mr Security is allowed to specify the port number on the external web server
         (rule1 = (permit t) :- 
                (RSetPort (requestAtTime t))
                (= (requestUser (requestAtTime t)) 'mrSecurity) 
                (= (requestServer (requestAtTime t))
                   (externalAtTime t)))        
         
         ;(*) Mr Admin can set the port number on any other web server
         (rule2 = (permit t) :- 
                (RSetPort (requestAtTime t))
                (= (requestUser (requestAtTime t)) 'mrAdmin)
                (not (= (requestServer (requestAtTime t)) 
                        (externalAtTime t))))
         
         ;(*) Only Mr Manager can nominate the machine to serve as the external web server
         (rule3 = (permit t) :-
                (RSetExternal (requestAtTime t))
                (= (requestUser (requestAtTime t)) 'mrManager))
         
          ;TN: Of course, all of these actions have side effects in the future,
          ;which are given in the vocabulary.
         
         (ruleOtherwise = (deny t) :- true))        
        (RComb (fa permit deny)))