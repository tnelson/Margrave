

; Type vs. predicate difficulty here.
; (1) want to restrict function co-domain for gradeBelongsTo
; (2) but want roles to be predicates in order to easily allow overlapping
(Vocab TestRoleOverlap              
             (Types
              (Type Subject)                            
              (Type Action > Read Write)                            
              (Type Resource > Grade))
             (Predicates 
              (Predicate isStudent Subject)
              (Predicate isFaculty Subject)
              (Predicate isAdministrator Subject)
              (Predicate gradeBelongsTo Grade Subject)))


;(Vocab TestRoleOverlap              
;             (Types
;              (Type Subject > Student Faculty Administrator)
;              
;              ; This is bloody annoying.
;              ; Instead, make them predicates (hasRole!)
;              (Type Student > AllSFA)
;              (Type Faculty > AllSFA)
;              (Type Administrator > AllSFA)
;              
;              (Type Action > Read Write)                            
;              (Type Resource > Grade))
;             (Predicates (Predicate gradeBelongsTo Grade Student)))
             
              
;(Vocab TestRoleOverlap              
;             (Types
;              (Type Subject)
;              (Type Action > Read Write)                            
;              (Type Resource > Grade)
;              
;              ; Expressed this way to experiment with use of functions
;              (Type Roles > Student Faculty Administrator))
;                          
;             (Functions (Function rolesOf Subject Roles)
;                        (Function gradeBelongsTo Grade Subject)))

	      
