#lang racket

; Salman's experimental RBAC query
; re-written by Tim in Dec 2011


(require "../margrave/margrave.rkt")

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "M:\\RktMargrave\\margrave"
                       #:jvm-params '("-Xmx512m"))

(m-load-policy "before" "M:\\RktMargrave\\salman\\rbac_before.p")
(m-load-policy "after" "M:\\RktMargrave\\salman\\rbac_after.p")

; The query is the conjunction of a bunch of conditions:
(define qrysexpr '(and 
                   
                   ; All roles are one of the constants
                   (forall arole Role (or (= arole 'professor) (= arole 'student) (= arole 'ta) (= arole 'ra)))
  
                   ; All users are one of the constants
                   (forall auser User (or (= auser 'tim) (= auser 'kathi) (= auser 'salman)))
                   
                   ; All permissions are one of the constants
                   (forall ap Permission (or (= ap 'grade) (= ap 'teach) (= ap 'enterLab) (= ap 'takeExam) (= ap 'submitPaper)))
                   
                   ; ^^^ TODO note to self: Seems to be possible optimization here? Could have a (all-are-constants Sortname) constraint in axioms.
                   
                   ; Before and After policies both permit!
                   ([after permit] u p)
                   ([before permit] u p)   
                   
                   ; Roles and permissions (and negations)
                   (hadRole 'kathi 'professor)
                   (hadRole 'salman 'student)
                   (hadRole 'salman 'ta)
                   (hadRole 'salman 'ra)
                   (hadRole 'tim 'student)                       
                   (hadRole 'tim 'ra)       
                   (not (hadRole 'tim 'professor))
                   (not (hadRole 'tim 'ta))
                   (not (hadRole 'salman 'professor))
                   (not (hadRole 'kathi 'ta))
                   (not (hadRole 'kathi 'ra))
                   (not (hadRole 'kathi 'student))
                   (hadPermission 'professor 'submitPaper)
                   (hadPermission 'professor 'grade)
                   (hadPermission 'professor 'teach)
                   (hadPermission 'professor 'enterLab)
                   (not (hadPermission 'professor 'takeExam))
                   (hadPermission 'student 'takeExam)
                   (not (hadPermission 'student 'submitPaper))
                   (not (hadPermission 'student 'grade))
                   (not (hadPermission 'student 'teach))
                   (not (hadPermission 'student 'enterLab))                       
                   (hadPermission 'ta 'grade)
                   (hadPermission 'ta 'enterLab)
                   (not (hadPermission 'ta 'submitPaper))
                   (not (hadPermission 'ta 'teach))
                   (not (hadPermission 'ta 'takeExam))
                   (hadPermission 'ra 'submitPaper)
                   (hadPermission 'ra 'enterLab)
                   (not (hadPermission 'ra 'grade))
                   (not (hadPermission 'ra 'teach))
                   (not (hadPermission 'ra 'takeExam))
                   
                   ; permissions are EXACTLY THE SAME in new policy
                   (forall rx Role 
                           (forall px Permission 
                                   (iff (hadPermission rx px) 
                                        (hasPermission rx px))))
                   
                   ; permits are EXACTLY THE SAME in new policy except Tim Grading.
                   (forall ux User 
                           (forall px Permission 
                                   (or (and (= ux 'tim) (= px 'grade))                                       
                                       (iff ([before permit] ux px) 
                                            ([after permit] ux px)))))
                   ))


        
(time (m-let "Q" '([u User] [p Permission]) 
                   qrysexpr))   
;(time (m-let "Q" '([u User] [p Permission]) 
;                   qrysexpr
;                   #:debug 2))   


(time (m-is-poss? "Q"))
(time (m-is-poss? "Q"))