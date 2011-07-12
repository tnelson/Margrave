
(Policy uses TestRoleOverlap
        (Variables 
         (Variable s Subject)
         (Variable a Action)
         (Variable r Resource))
        (Rules 
  	  (ProfessorsCanWrite = (permit s a r) :- (isFaculty s) (Write a) (Grade r))
          (StudentsCanReadOwn = (permit s a r) :- (isStudent s) (Read a) (Grade r) (gradeBelongsTo r s))
          (DenyOtherwise = (deny s a r) :- true)
	  )
        (RComb (fa permit deny)))


;(Policy uses TestRoleOverlap
;        (Variables 
;         (Variable s Subject)
;         (Variable a Action)
;         (Variable r Resource))
;        (Rules 
;  	  (ProfessorsCanWrite = (permit s a r) :- (Faculty (rolesOf s)) (Write a) (Grade r))
;          (StudentsCanReadOwn = (permit s a r) :- (Student (rolesOf s)) (Read a) (Grade r) (= s (gradeBelongsTo r)))
;          (DenyOtherwise = (deny s a r) :- true)
;	  )
;        (RComb (fa permit deny)))

;(Policy uses TestRoleOverlap
;        (Variables 
;         (Variable s Subject)
;         (Variable a Action)
;         (Variable r Resource))
;        (Rules 
;  	  (ProfessorsCanWrite = (permit s a r) :- (Faculty s) (Write a) (Grade r))
;          (StudentsCanReadOwn = (permit s a r) :- (Student s) (Read a) (Grade r) (gradeBelongsTo r s))
;          (DenyOtherwise = (deny s a r) :- true)
;	  )
;        (RComb (fa permit deny)))
