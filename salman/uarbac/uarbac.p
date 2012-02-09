; Sorts are Capitalized
; predicates (both EDBs and IDBs like decisions) are lowercase
; constants are lowercase, preceded with single-quote

(Policy uses uarbac
        (Variables 
         (u User)
         (p Permission)
         (c Class)
	 (o Object)
	 (pr Role)
	 (spr Role)
	 (pu User))
        (Rules 
	  (Perm = (permit u p o) :- (exists r Role (and (ua u r) (opa r p o))))
	  (PermViaCls = (permit u p o) :- (exists r Role (exists c Class (and (ua u r) (cpa r p c) (= (ob o) c)))))
          (HierPerm = (permit u p o) :- (exists subr Role (exists supr Role (and (ua u supr) (rh supr subr) (opa subr p o)))))
          (HierPermViaCls = (permit u p o) :- (exists subr Role (exists supr Role (exists c Class (and (ua u supr) (rh supr subr) (cpa subr p c) (= (ob o) c))))))

	  (CrtObj = (createObject u c pr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'create c) (opa r2 'empower pr)))))

	  (DelObj = (deleteObject u o) :- (exists r Role (and (ua u r) (opa r 'admin o))))
	  (DelObjViaCls = (deleteObject u o) :- (exists r Role (exists c Class (and (ua u r) (cpa r 'admin c) (= (ob o) c)))))

	  (GrntRoleToUsr = (grantRoleToUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'grant pr) (opa r2 'empower pu)))))
	  (GrntRoleToUsrViaRoleCls = (grantRoleToUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'grant 'role) (opa r2 'empower pu)))))
	  (GrntRoleToUsrViaUsrCls = (grantRoleToUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'grant pr) (cpa r2 'empower 'user)))))
	  (GrntRoleToUsrViaRoleAndUsrCls = (grantRoleToUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'grant 'role) (cpa r2 'empower 'user)))))

	  (RvkRoleFromUsr = (revokeRoleFromUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (or (opa r1 'admin pr) (opa r1 'admin pu) (and (opa r1 'grant pr) (opa r2 'empower pu)))))))
	  (RvkRoleFromUsrViaRoleClsAdm = (revokeRoleFromUser u pr pu) :- (exists r Role (and (ua u r) (cpa r 'admin 'role))))
	  (RvkRoleFromUserViaUserClsAdm = (revokeRoleFromUser u pr pu) :- (exists r Role (and (ua u r) (cpa r 'admin 'user))))
	  (RvkRoleFromUserViaRoleClassGrnt = (revokeRoleFromUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'grant 'role) (opa r2 'empower pu)))))
	  (RvkRoleFromUsrViaUsrClsEmp = (revokeRoleFromUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'grant pr) (cpa r2 'empower 'user)))))
	  (RvkRoleFromUsrViaRoleClsGrntAndUsrClsEmp = (revokeRoleFromUser u pr pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'grant 'role) (cpa r1 'empower 'user)))))

	  (GrntRoleToRole = (grantRoleToRole u pr spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'empower pr) (opa r2 'grant spr)))))
	  (GrntRoleToRoleViaRoleClsEmp = (grantRoleToRole u pr spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'empower'role) (opa r2 'grant spr)))))
	  (GrntRoleToRoleViaRoleClsGrnt = (grantRoleToRole u pr spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'empower pr) (cpa r2 'grant 'role)))))
	  (GrntRoleToRoleViaRoleClsGrntAndEmp = (grantRoleToRole u pr spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'empower 'role) (cpa r2 'grant 'role)))))

	  (RvkRoleFromRole = (revokeRoleFromRole u pr spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (or (opa r1 'admin pr) (opa r1 'admin spr) (and (opa r1 'empower pr) (opa r2 'grant spr)))))))
	  (RvkRoleFromRoleViaRoleClsAdm = (revokeRoleFromRole u pr spr) :- (exists r Role (and (ua u r) (cpa r 'admin 'role))))
	  (RvkRoleFromRoleViaRoelClsEmp = (revokeRoleFromRole u pr spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'empower 'role) (opa r2 'grant spr)))))
	  (RvkRoleFromRoleViaRoleClsGrnt = (revokeRoleFromRole u pr spr) :- (exists r Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'empower pr) (cpa r2 'grant 'role)))))
	  (RvkRoleFromRoleViaRoleClsEmpAndGrnt = (revokeRoleFromRole u pr spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'empower 'role) (cpa r2 'grant 'role)))))

	  (GrntObjPermToRole = (grantObjectPermToRole u pr p o) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'admin o) (opa r2 'admin pr)))))
	  (GrntObjPermToRoleViaClass = (grantObjectPermToRole u pr p o) :- (exists r1 Role (exists r2 Role (exists c Class (and (ua u r1) (ua u r2) (= (ob o) c) (cpa r1 'admin c) (opa r2 'admin pr))))))
	  (GrntObjPermToRoleViaRoleClass = (grantObjectPermToRole u pr p o) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'admin o) (cpa r2 'admin 'role)))))
	  (GrntObjPermToRoleViaClsAndRoleCls = (grantObjectPermToRole u pr p o) :- (exists r1 Role (exists r2 Role (exists c Class (and (ua u r1) (ua u r2) (= (ob o) c) (opa r1 'admin c) (cpa r2 'admin 'role))))))

	  (RvkObjPermFromRole = (revokeObjectPermFromRole u pr p o) :- (exists r Role (and (ua u r) (or (opa r 'admin o) (opa r 'admin pr)))))
	  (RvkObjPermFromRoleViaCls = (revokeObjectPermFromRole u pr p o) :- (exists r Role (exists c Class (and (ua u r) (= (ob o) c) (cpa r 'admin c)))))
	  (RvkObjPermFromRoleViaRoleCls = (revokeObjectPermFromRole u pr p o) :- (exists r Role (and (ua u r) (cpa r 'admin 'role))))

	  (GrntUsrPermToRole = (grantUserPermToRole u pr p pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'admin pu) (opa r2 'admin pr)))))
	  (GrntUsrPermToRoleViaUsrCls = (grantUserPermToRole u pr p pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'admin 'user) (opa r2 'admin pr)))))
	  (GrntUsrPermToRoleViaRoleCls = (grantUserPermToRole u pr p pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'admin pu) (cpa r2 'admin 'role)))))
	  (GrntUsrPermToRoleViaUsrClsAndRoleCls = (grantUserPermToRole u pr p pu) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (cpa r1 'admin 'user) (cpa r2 'admin 'role)))))

	  (RvkUsrPermFromRole = (revokeUserPermFromRole u pr p pu) :- (exists r Role (and (ua u r) (or (opa r 'admin pu) (opa r 'admin pr)))))
	  (RvkUsrPermFromRoleViaRoleCls = (revokeUserPermFromRole u pr p pu) :- (exists r Role (and (ua u r) (cpa r 'admin 'role))))

	  (GrntRolePermToRole = (grantRolePermToRole u pr p spr) :- (exists r1 Role (exists r2 Role (and (ua u r1) (ua u r2) (opa r1 'admin pr) (opa r2 'admin spr)))))
	  (GrntRolePermToRoleViaRoleCls = (grantRolePermToRole u pr p spr) :- (exists r Role (and (ua u r) (cpa r 'admin 'role))))

	  (RvkRolePermFromRole = (revokeRolePermFromRole u pr p spr) :- (exists r Role (and (ua u r) (or (opa r 'admin pr) (opa r 'admin spr)))))
	  (RvkRolePermFromRoleViaRoleCls = (revokeRolePermFromRole u pr p spr) :- (exists r Role (and (ua u r) (cpa r 'admin 'role))))))