; Semi-stateful example due to Paul Anderson
; TN

(Policy uses pa-state
        (Variables
         (req Request))
        (Rules
         ;(*) Only Mr Security is allowed to specify the port number on the external web server
         (rule1 = (permit req) :- 
                (RSetPort req)
                (= (requestUser req) 'mrSecurity) 
                (= (requestServer req)
                   (externalAtTime (requestAtTime req))))
         
         ;(*) Mr Admin can set the port number on any other web server
         (rule2 = (permit req) :- 
                (RSetPort req)
                (= (requestUser req) 'mrAdmin)
                (not (= (requestServer req) 
                        (externalAtTime (requestAtTime req)))))
         
         ;(*) Only Mr Manager can nominate the machine to serve as the external web server
         (rule3 = (permit req) :-
                (RSetExternal req)
                (= (requestUser req) 'mrManager))
         
          ;TN: Of course, all of these actions have side effects in the future,
          ;which are given in the vocabulary.
         
         (ruleOtherwise = (deny req) :- true))        
        (RComb (fa permit deny)))