; Sorts are Capitalized
; predicates (both EDBs and IDBs like decisions) are lowercase
; constants are lowercase, preceded with single-quote

(Theory uarbac
        (Vocab uarbac
               (Types
                User 
                Role 
                Class
                Object 
                Permission 
                PseudoAction)
                        
               (Constants
                ('craig User)
                ('joshua User)
                ('kathi User) 
                ('tim User) 
                ('theo User) 
                ('salman User)
                ('head Role)
                ('professor Role) 
                ('student Role)
                ('ta Role) 
                ('ra Role)
                ('course Class)
                ('lab Class) 
                ('department Class)                
                ('cs2102 Object)
                ('cs521 Object)
                ('alas Object)
                ('hci Object)
                ('cs Object)                
                ('register Permission)
                ('drop Permission) 
                ('lecture Permission)
                ('grade Permission)
                ('research Permission)
                ('recruit Permission)                
                ('admin PseudoAction)
                ('empower PseudoAction)
                ('grant PseudoAction))
               
               (Predicates
                (ob Class Object)
                (rh Role Role)
                (ua User Role)
                (pa Role Permission Class)
                (paa Role PseudoAction Class)
                (rpaa Role PseudoAction Role)))
        
        
        (Axioms
         ;(singleton-all User)
         ;(atmostone-all Role)
         ;(atmostone-all Permission)
         ;(atmostone-all Class)
         ;(atmostone-all Object)
         ;(disjoint-all User)
         ;(disjoint-all Role)
         ;(disjoint-all Permission)
         ;(disjoint-all Class)
         ;(disjoint-all Object)
         (abstract User)
         (abstract Role)
         (abstract Permission)
         (abstract Class)
         (abstract Object)))