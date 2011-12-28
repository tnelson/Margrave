; Sorts are Capitalized
; predicates (both EDBs and IDBs like decisions) are lowercase
; constants are lowercase, preceded with single-quote

(Policy uses uarbac
        (Variables 
         (u User)
         (p Permission)
         (c Class))
        (Rules 
	  (Perm = (permit u p c) :- (exists r Role (and (ua u r) (pa r p c))))
          (HierarchyPerm = (permit u p c) :- (exists subr Role (exists supr Role (and (ua u supr) (rh supr subr) (pa subr p c))))))
        (RComb (over permit deny)))