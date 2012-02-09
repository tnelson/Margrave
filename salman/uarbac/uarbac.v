; Sorts are Capitalized
; predicates (both EDBs and IDBs like decisions) are lowercase
; constants are lowercase, preceded with single-quote

(Theory uarbac
        (Vocab uarbac
               (Types
                Class
                (Object > User Role DomainObject)
                Permission)
                        
               (Constants
	        ('sso User)
                ('craig User)
                ('dan User)
                ('kathi User) 
                ('tim User) 
                ('theo User) 
                ('salman User)
		('administrator Role)
                ('head-cs Role)
                ('professor-cs2102 Role) 
		('professor-cs521 Role)
                ('student-csgrad Role)
		('manager-alas Role)
		('manager-hci Role)
		('faculty-cs Role)
                ('ta-cs2102 Role) 
                ('ra-alas Role)
		('user Class)
		('role Class)
                ('course Class)
                ('lab Class) 
                ('department Class)                
                ('cs2102 DomainObject)
                ('cs521 DomainObject)
                ('alas DomainObject)
                ('hci DomainObject)
                ('cs DomainObject)
                ('register Permission)
                ('drop Permission) 
                ('lecture Permission)
                ('grade Permission)
                ('research Permission)
                ('recruit Permission)
		('create Permission)
                ('admin Permission)
                ('empower Permission)
                ('grant Permission))
               
               (Predicates
                (rh Role Role)
                (ua User Role)
                (cpa Role Permission Class)
		(opa Role Permission Object))

               (Functions
                (ob Object Class)))
        
        (Axioms
         (abstract User)
         (abstract Role)
	 (abstract DomainObject)
         (abstract Permission)
         (abstract Class)
         (abstract Object)
         (constants-neq-all User)
         (constants-neq-all Role)
         (constants-neq-all Permission)
         (constants-neq-all Class)
	 (constants-neq-all DomainObject)
         (constants-neq-all Object)))