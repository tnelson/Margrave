#lang racket

(require "../../margrave/margrave.rkt")

(start-margrave-engine #:margrave-params '("-log")
                       ;#:margrave-path "M:\\RktMargrave\\margrave"
                       #:margrave-path "../../margrave"
                       
                       #:jvm-params '("-Xmx512m"))


(m-load-policy "uarbac" "uarbac.p")


(m-let "Q"
      ; '([qu1 User] [qu2 User] [qr1 Role] [qp Permission] [qc Class] [qo Object])
       '()
       '(and

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Miscellaneous examples:                                              ;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       ; ([uarbac permit] qu1 qp qo)
       ; ([uarbac grantRoleToUser] 'craig qr1 qu1)
       ; ([uarbac grantRoleToUser] 'craig 'manager-alas 'salman)
         ([uarbac revokeRoleFromUser] 'craig 'manager-alas 'dan)
       ; ([uarbac revokeRoleFromUser] 'dan 'manager-alas 'kathi)

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Program synthesis examples:                                          ;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


       ;; Kathi's queries:
       ; 1.
       ; ([uarbac permit] 'salman 'research 'alas)

       ; 2.
       ; (ua 'salman 'ra-alas)
       ; (opa 'ra-alas 'research 'alas)

       ; 3.
       ; ([uarbac grantRoleToUser] 'kathi 'ra-alas 'salman)
        ;([uarbac grantObjectPermToRole] 'kathi 'ra-alas 'research 'alas)  ;-- NO RESPONSE

       ;; Craig's queries:
       ; 5.
       ; ([uarbac grantRoleToUser] 'kathi 'ra-alas 'salman)  ;-- NO RESPONSE
	 
       ; 6.
       ; (uipa 'faculty-cs 'empower 'salman)
       ; (ripa 'manager-alas 'grant 'ra-alas)

       ; 7.
       ; ([uarbac grantUserPermToRole] 'craig 'faculty-cs 'empower 'salman)
       ;([uarbac grantRolePermToRole] 'craig 'manager-alas 'grant 'ra-alas)

       ;; SSO's queries:
       ; 9.
       ; (ripa 'craig 'empower 'manager-alas)
       ; (ripa 'craig 'admin 'ra-alas)

       ; 10.
       ; ([uarbac grantRolePermToRole] 'sso 'head-cs 'empower 'manager-alas)
       ; ([uarbac grantRolePermToRole] 'sso 'head-cs 'admin 'ra-alas)

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; Program Verification examples:                                       ;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       ; Craig's goal: Kathi should be able to grant TAship for cs2102
       ;([uarbac grantRoleToUser] 'kathi 'ta-cs2102 'tim)
       ;; ([uarbac grantRoleToUser] 'kathi 'ta-cs2102 'theo)
       ;; ([uarbac grantRoleToUser] 'kathi 'ta-cs2102 'salman)

       ;; (opa 'professor-cs2102 'grant 'ta-cs2102)
       ;; (opa 'faculty-cs 'empower 'tim)
       ;; (opa 'faculty-cs 'empower 'theo)
       ;; (opa 'faculty-cs 'empower 'salman)
       

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; UARBAC state definition:                                             ;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       ;RH(role1, role2)
       (forall role1 Role
	       (forall role2 Role		       
		       (and
			(implies 				 
			 (or
			  (and (= role1 'head-cs) (= role2 'manager-hci))
			  (and (= role1 'head-cs) (= role2 'manager-alas))
			  (and (= role1 'ta-cs2102) (= role2 'student-csgrad))
			  (and (= role1 'ra-alas) (= role2 'student-csgrad)))
			 (rh role1 role2))
			(implies 				 
			 (not
			  (or
			   (and (= role1 'head-cs) (= role2 'manager-hci))
			   (and (= role1 'head-cs) (= role2 'manager-alas))
			   (and (= role1 'ta-cs2102) (= role2 'student-csgrad))
			   (and (= role1 'ra-alas) (= role2 'student-csgrad))))
			 (not
			  (rh role1 role2))))))

       ;UA(user, role)
       (forall auser User
	       (forall arole Role		       
		       (and
			(implies 				 
			 (or
			  (and (= auser 'sso) (= arole 'administrator))
			  (and (= auser 'craig) (= arole 'head-cs))
			  (and (= auser 'craig) (= arole 'manager-hci))
			  (and (= auser 'craig) (= arole 'faculty-cs))
			  (and (= auser 'dan) (= arole 'manager-alas))
			  (and (= auser 'dan) (= arole 'professor-cs521))
			  (and (= auser 'dan) (= arole 'faculty-cs))
			  (and (= auser 'kathi) (= arole 'manager-alas))
			  (and (= auser 'kathi) (= arole 'professor-cs2102))
			  (and (= auser 'kathi) (= arole 'faculty-cs))
			  (and (= auser 'tim) (= arole 'ra-alas))
			  (and (= auser 'tim) (= arole 'student-csgrad))
			  (and (= auser 'theo) (= arole 'ta-cs2102))
			  (and (= auser 'theo) (= arole 'student-csgrad))
			  (and (= auser 'salman) (= arole 'ra-alas))
			  (and (= auser 'salman) (= arole 'ta-cs2102))
			  (and (= auser 'salman) (= arole 'student-csgrad)))
			 (ua auser arole))
			(implies 				 
			 (not
			  (or
			   (and (= auser 'sso) (= arole 'administrator))
			   (and (= auser 'craig) (= arole 'head-cs))
			   (and (= auser 'craig) (= arole 'manager-hci))
			   (and (= auser 'craig) (= arole 'faculty-cs))
			   (and (= auser 'dan) (= arole 'manager-alas))
			   (and (= auser 'dan) (= arole 'professor-cs521))
			   (and (= auser 'dan) (= arole 'faculty-cs))
			   (and (= auser 'kathi) (= arole 'manager-alas))
			   (and (= auser 'kathi) (= arole 'professor-cs2102))
			   (and (= auser 'kathi) (= arole 'faculty-cs))
			   (and (= auser 'tim) (= arole 'ra-alas))
			   (and (= auser 'tim) (= arole 'student-csgrad))
			   (and (= auser 'theo) (= arole 'ta-cs2102))
			   (and (= auser 'theo) (= arole 'student-csgrad))
			   (and (= auser 'salman) (= arole 'ra-alas))
			   (and (= auser 'salman) (= arole 'ta-cs2102))
			   (and (= auser 'salman) (= arole 'student-csgrad))))
			 (not
			  (ua auser arole))))))
       
       ;PA(role, permission, class)
       (forall arole Role
	       (forall ap Permission
		       (forall cls Class
			       (and
				(implies 				 
				 (or
				  (and (= arole 'head-cs) (= ap 'admin) (= cls 'course)))
				 (cpa arole ap cls))
				(implies 				 
				 (not
				  (or
				   (and (= arole 'head-cs) (= ap 'admin) (= cls 'course))))
				  (not
				   (cpa arole ap cls)))))))

       
       ;PA(role, permission, object) ;;The object is neither a role or a user.
       (forall arole Role
	       (forall ap Permission
		       (forall obj Object
			       (and
				(implies
				 (or
				  (and (= arole 'head-cs) (= ap 'admin) (= obj 'cs))
				  (and (= arole 'head-cs) (= ap 'recruit) (= obj 'cs))
				  (and (= arole 'manager-hci) (= ap 'admin) (= obj 'hci))
				  (and (= arole 'manager-hci) (= ap 'research) (= obj 'hci))
				  (and (= arole 'ta-cs2102) (= ap 'grade) (= obj 'cs2102))
				  (and (= arole 'ra-alas) (= ap 'research) (= obj 'alas))
				  (and (= arole 'manager-alas) (= ap 'admin) (= obj 'alas))
				  (and (= arole 'manager-alas) (= ap 'research) (= obj 'alas))
				  (and (= arole 'professor-cs2102) (= ap 'admin) (= obj 'cs2102))
				  (and (= arole 'professor-cs2102) (= ap 'lecture) (= obj 'cs2102))
				  (and (= arole 'professor-cs2102) (= ap 'grade) (= obj 'cs2102))
				  (and (= arole 'professor-cs521) (= ap 'admin) (= obj 'cs521))
				  (and (= arole 'professor-cs521) (= ap 'lecture) (= obj 'cs521))
				  (and (= arole 'professor-cs521) (= ap 'grade) (= obj 'cs521))
       				  (and (= arole 'head-cs) (= ap 'grant) (= obj 'student-csgrad))
       				  (and (= arole 'head-cs) (= ap 'grant) (= obj 'faculty-cs))
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'faculty-cs))
       				  (and (= arole 'head-cs) (= ap 'grant) (= obj 'manager-hci))
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'manager-hci))
       				  (and (= arole 'head-cs) (= ap 'grant) (= obj 'manager-alas))
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'manager-alas))
				  (and (= arole 'manager-alas) (= ap 'grant) (= obj 'ra-alas))
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'craig))
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'kathi))				  
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'dan))
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'tim))
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'theo))				  
       				  (and (= arole 'head-cs) (= ap 'empower) (= obj 'salman))
				  (and (= arole 'faculty-cs) (= ap 'empower) (= obj 'tim))
				  (and (= arole 'faculty-cs) (= ap 'empower) (= obj 'theo))
				  (and (= arole 'faculty-cs) (= ap 'empower) (= obj 'salman)))
				 (opa arole ap obj))
				(implies 				 
				 (not
				  (or
				   (and (= arole 'head-cs) (= ap 'admin) (= obj 'cs))
				   (and (= arole 'head-cs) (= ap 'recruit) (= obj 'cs))
				   (and (= arole 'manager-hci) (= ap 'admin) (= obj 'hci))
				   (and (= arole 'manager-hci) (= ap 'research) (= obj 'hci))
				   (and (= arole 'ta-cs2102) (= ap 'grade) (= obj 'cs2102))
				   (and (= arole 'ra-alas) (= ap 'research) (= obj 'alas))
				   (and (= arole 'manager-alas) (= ap 'admin) (= obj 'alas))
				   (and (= arole 'manager-alas) (= ap 'research) (= obj 'alas))
				   (and (= arole 'professor-cs2102) (= ap 'admin) (= obj 'cs2102))
				   (and (= arole 'professor-cs2102) (= ap 'lecture) (= obj 'cs2102))
				   (and (= arole 'professor-cs2102) (= ap 'grade) (= obj 'cs2102))
				   (and (= arole 'professor-cs521) (= ap 'admin) (= obj 'cs521))
				   (and (= arole 'professor-cs521) (= ap 'lecture) (= obj 'cs521))
				   (and (= arole 'professor-cs521) (= ap 'grade) (= obj 'cs521))
				   (and (= arole 'head-cs) (= ap 'grant) (= obj 'student-csgrad))
       				   (and (= arole 'head-cs) (= ap 'grant) (= obj 'faculty-cs))
       				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'faculty-cs))
       				   (and (= arole 'head-cs) (= ap 'grant) (= obj 'manager-hci))
       				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'manager-hci))
       				   (and (= arole 'head-cs) (= ap 'grant) (= obj 'manager-alas))
       				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'manager-alas))
				   (and (= arole 'manager-alas) (= ap 'grant) (= obj 'ra-alas))
				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'craig))
				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'kathi))				  
				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'dan))
				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'tim))
				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'theo))				  
				   (and (= arole 'head-cs) (= ap 'empower) (= obj 'salman))
				   (and (= arole 'faculty-cs) (= ap 'empower) (= obj 'tim))
				   (and (= arole 'faculty-cs) (= ap 'empower) (= obj 'theo))
				   (and (= arole 'faculty-cs) (= ap 'empower) (= obj 'salman))))
				 (not
				  (opa arole ap obj))))))))

       #:ceiling '([User 7]
		   [Role 10]
		   [Course 2]
		   [Lab 2]
		   [Department 1]
		   [Object 22]
		   [Class 5]
		   [Permission 10]
		   [univ 37]))

;(time (m-get "Q"))
;       #:include '(([uarbac permit] 'dan 'recruit c)))

;; Retursn the relation that we are interested in.
; To pretty-print, use m-show:
(display (m-show "Q"))


