#lang racket

(require margrave)

(start-margrave-engine #:margrave-params '("-log")
                       ;#:margrave-path "M:\\RktMargrave\\margrave"
                      ; #:margrave-path "F:\\msysgit\\git\\margrave\\margrave"
                       #:jvm-params '("-Xmx512m"))


(m-load-policy "uarbac" "uarbac.p")

(m-let "Q"
       '([qu1 User] [qu2 User] [qr1 Role] [qp Permission] [qc Class] [qo Object])
       '(and
       ; Miscellaneous examples:
       ; ([uarbac permit] qu1 qp qo)
       ; ([uarbac grantRoleToUser] 'craig qr1 qu1)
       ; ([uarbac grantRoleToUser] 'craig 'manager-alas 'salman)
       ; ([uarbac revokeRoleFromUser] 'craig 'manager-alas 'dan)
       ; ([uarbac revokeRoleFromUser] 'dan 'manager-alas 'kathi)

       ;; Kathi's queries:
       ; 1.
       ([uarbac permit] 'salman 'research 'alas)  ;-- NO RESPONSE

       ; 2.
       ; (ua 'salman 'ra-alas)
       ; (opa 'ra-alas 'research 'alas)

       ; 3.
       ; ([uarbac grantRoleToUser] 'kathi 'ra-alas 'salman) ;-- NO RESPONSE
       ; ([uarbac grantObjectPermToRole] 'kathi 'ra-alas 'research 'alas)  ;-- NO RESPONSE

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

       ;UARBAC state definition:


       ;; (forall auser User
       ;; 		(= (ob auser) 'user))
       ;; (forall arole Role
       ;; 		(= (ob arole) 'role))
       (= (ob 'sso) 'user)
       (= (ob 'craig) 'user)
       (= (ob 'dan) 'user)
       (= (ob 'kathi) 'user)
       (= (ob 'tim) 'user)
       (= (ob 'theo) 'user)
       (= (ob 'salman) 'user)
       (= (ob 'administrator) 'role)
       (= (ob 'head-cs) 'role)
       (= (ob 'professor-cs2102) 'role)
       (= (ob 'professor-cs521) 'role)
       (= (ob 'student-csgrad) 'role)
       (= (ob 'manager-alas) 'role)
       (= (ob 'manager-hci) 'role)
       (= (ob 'faculty-cs) 'role)
       (= (ob 'ta-cs2102) 'role)
       (= (ob 'ra-alas) 'role)
       (= (ob 'cs2102) 'course)
       (= (ob 'cs521) 'course)
       (= (ob 'alas) 'lab)
       (= (ob 'hci) 'lab)
       (= (ob 'cs) 'department)

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
				  (and (= arole 'ta-cs2102) (= ap 'grade) (= obj 'cs521))
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
				   (and (= arole 'ta-cs2102) (= ap 'grade) (= obj 'cs521))
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
		   [DomainObject 5]
		   [Object 22]
		   [Class 5]
		   [Permission 10]
		   [univ 37]))

;(time (m-get "Q"))
;       #:include '(([uarbac permit] 'dan 'recruit c)))

;; Retursn the relation that we are interested in.
; To pretty-print, use m-show:
;(display (m-show "Q"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define mainScenario  (m-get "Q"))


(struct dictionaryElement (word atoms) #:transparent)



;; Returns a relation of the scenario indicated by relName
(define (getRelations scenario relName) 
  (first (filter (lambda (x) (string=? (m-relation-name x) relName))
	  (m-scenario-relations scenario))))


;;(display temp)


;; Returns all the mappings for a given atom.
(define (getMappings atom relations modelRelations)
  (map
   (lambda (relation) (m-relation-name relation))
   (filter 
    (lambda (x) (and (member atom (foldr append '() (m-relation-tuples x)))
		     (not (member (m-relation-name x) modelRelations))))
    relations)))


;; Makes the dictionary for a given relation:
(define (makeDictionary relation relations modelRelations)
  (map (lambda (elm) (dictionaryElement elm (getMappings elm relations modelRelations)))
   ; Returns a list of unique atoms used in the relation
   (filter (lambda (atm) (not (string=? (substring atm 0 (min (string-length atm) 12)) "DomainObject")))
	   (remove-duplicates (foldr append '() (m-relation-tuples relation))))))





;;; (display (makeDictionary (getRelations mainScenario "opa") (m-scenario-relations mainScenario) '("rh" "opa" "cpa" "ua" "ob")))


