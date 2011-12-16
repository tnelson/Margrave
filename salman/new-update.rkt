#lang racket

; Salman's experimental RBAC query
; re-written by Tim in Dec 2011


(require "../margrave/margrave.rkt")

(start-margrave-engine #:margrave-params '("-log")
                       ;#:margrave-path "M:\\RktMargrave\\margrave"
                       #:margrave-path "F:\\msysgit\\git\\margrave\\margrave"
                       #:jvm-params '("-Xmx512m"))

;(m-load-policy "before" "M:\\RktMargrave\\salman\\rbac_before.p")
;(m-load-policy "after" "M:\\RktMargrave\\salman\\rbac_after.p")
(m-load-policy "before" "F:\\msysgit\\git\\margrave\\salman\\rbac_before.p")
(m-load-policy "after" "F:\\msysgit\\git\\margrave\\salman\\rbac_after.p")

; The query is the conjunction of a bunch of conditions:
(define qrysexpr '(and 
                   
                   ; All roles are one of the constants
                   (forall arole Role (or (= arole 'professor) (= arole 'student) (= arole 'ta) (= arole 'ra)))
  
                   ; All users are one of the constants
                   (forall auser User (or (= auser 'tim) (= auser 'kathi) (= auser 'salman)))
                   
                   ; All permissions are one of the constants
                   (forall ap Permission (or (= ap 'grade) (= ap 'teach) (= ap 'enterLab) (= ap 'takeExam) (= ap 'submitPaper)))
                   
                   ; All are disjoint
                   (not (= 'tim 'kathi))
                   (not (= 'tim 'salman))
                   (not (= 'kathi 'salman))
                   (not (= 'professor 'student))
                   (not (= 'professor 'ta))
                   (not (= 'professor 'ra))
                   (not (= 'student 'ta))
                   (not (= 'student 'ra))
                   (not (= 'ta 'ra))
                   (not (= 'grade 'teach))
                   (not (= 'grade 'enterLab))
                   (not (= 'grade 'takeExam))
                   (not (= 'grade 'submitPaper))
                   (not (= 'teach 'enterLab))
                   (not (= 'teach 'takeExam))
                   (not (= 'teach 'submitPaper))
                   (not (= 'enterLab 'takeExam))
                   (not (= 'enterLab 'submitPaper))                   
                   (not (= 'takeExam 'submitPaper))

                   
                   ; ^^^ TODO note to self: Seems to be possible optimization here? Could have a (all-are-constants Sortname) constraint in axioms.
                   ; TODO also: should be able to do better lower-bounds here. (if two terms are necessarily !=, can add them both. Beyond just caused by disj.)
                   
                   ; Wasn't permitted before but is permitted now.
                   ([after permit] u p)
                   (not ([before permit] u p))
                   
                   ; Roles and permissions (and negations)                   
                   (hadRole 'salman 'student)
                   (hadRole 'salman 'ta)
                   (hadRole 'salman 'ra)
                   (not (hadRole 'salman 'professor))
                   
                   (hadRole 'tim 'student)                       
                   (hadRole 'tim 'ra)       
                   (not (hadRole 'tim 'professor))
                   (not (hadRole 'tim 'ta))   
                   
                   (hadRole 'kathi 'professor)
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
                   
                   (= u 'tim)
                   (= p 'grade)
                                      
                   ; role permissions are EXACTLY THE SAME in new policy
                   (forall rx Role 
                           (forall px Permission 
                                   (iff (hadPermission rx px) 
                                        (hasPermission rx px))))
                   
                   ; ^^^ TODO: axiom for subset constraints on _predicates_. will eliminate this forall.
                   
                    ;permits are EXACTLY THE SAME in new policy except: (Tim, Grading), which can change.  
                   (forall uy User 
                           (forall py Permission 
                                   (implies (or (not (= uy 'tim)) 
                                                (not (= py 'grade))) 
                                            (iff
                                             ([before permit] uy py)
                                             ([after permit] uy py) ))))
                   ; ^^^ note: this statement induces 2 Skolem functions from (User x Permission) -> Role
                   ; because each policy's permit fmla has 1 existential in it...
                   ))


        
(time (m-let "Q" '([u User] [p Permission]) 
                   qrysexpr
                   #:debug 2))   
(time (m-is-poss? "Q"))

;(time (m-let "QMinimalComb" '([u User] [p Permission]) 
;             '(and ([before permit] u p)
;                   ([after permit] u p))))

;(define num-iterations 20)
;(define timers
;  (for/list ([i (build-list num-iterations add1)])
;    (define-values (result1 t1a t1b t1c) (time-apply m-let 
;                                                     (list (string-append "Q" (number->string i))
;                                                           '([u User] [p Permission])  
;                                                           qrysexpr)))
;    (define-values (result2 t2a t2b t2c) (time-apply m-is-poss? (list (string-append "Q" (number->string i)))))
;    (list t1b t2b)))

;(define let-timers (map first timers))
;(define poss-timers (map second timers))
;(define let-time-avg (/ (apply + let-timers) num-iterations))
;(define poss-time-avg (/ (apply + poss-timers) num-iterations))

;let-time-avg
;poss-time-avg


