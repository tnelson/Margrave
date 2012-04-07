#lang racket

; Salman's experimental RBAC query
; re-written by Tim in Dec 2011; Modifications in March 2012


(require "../../racket/margrave.rkt")

(start-margrave-engine #:margrave-params '("-log")                       
                       #:margrave-path "../../racket"
                       #:jvm-params '("-Xmx512m"))

(m-load-policy "before" "rbac_before.p")
(m-load-policy "after" "rbac_after.p")

; The query is the conjunction of a bunch of conditions:
(define qrysexpr '(and 
                                      
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
                   
                   (or (= u 'tim) (= u 'kathi))
    ;               (= u 'salman)
     ;              (not (= p 'teach))
                   
                                      
                   ; role-permissions are EXACTLY THE SAME in new policy
                   (forall rx Role 
                           (forall px Permission 
                                   (iff (hadPermission rx px) 
                                        (hasPermission rx px))))
                   
                   ; ^^^ TODO: axiom for subset constraints on _predicates_. will eliminate this forall.
                   
                    ;permits are EXACTLY THE SAME in new policy except this particular request...
                   (forall uy User 
                           (forall py Permission 
                                   (implies (or (not (= uy u))              ; 'tim
                                                (not (= py p))       ; 'grade
                                                ) 
                                            (iff
                                             ([before permit] uy py)
                                             ([after permit] uy py) ))))
                   ; ^^^ note: this statement induces 2 Skolem functions from (User x Permission) -> Role
                   ; because each policy's permit fmla has 1 existential in it...
                   ))


; Create the query above with ID="Q"
; Wrapping the m-let in the time function prints performance info.
(time (m-let "Q" '([u User] [p Permission]) 
                   qrysexpr))   
; Is the query satisfiable?
;(time (m-is-poss? "Q"))
; Get an actual solution.
;(time (m-get-scenario "Q"))
; Reset the iterator, start at the beginning of the solution list again.
;(time (m-reset-scenario-iterator "Q"))
; Get an actual solution that includes whether <u, p> is permitted before and after.
;(time (m-get-scenario "Q" #:include '( ([before permit] u p) ([after permit] u p))))

; Create the same query, but bound the ceiling.
(time (m-let "Q3" '([u User] [p Permission]) 
                   qrysexpr
                  ; #:ceiling '([Role 4]
                  ;             [User 3]
                  ;             [univ 12]))) 
                   ))
;(time (m-get-scenario "Q3"))

; Test #lang margrave
;(define xmlresult (mtext "LET Q4[u : User, p: Permission] BE before.permit(u, p) CEILING 3 User, 5 Permission, 4 Role, 12 univ"))
;(mtext "show Q4")

; This will NOT override the computed univ bound. Need to provide explicitly as above.
;(time (m-let "Q2" '([u User] [p Permission]) 
;                   qrysexpr
;                   #:ceiling '([Role 4]
;                               [User 3]))) 
;(time (m-get "Q2"))



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


