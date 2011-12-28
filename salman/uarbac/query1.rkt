#lang racket

(require "../../margrave/margrave.rkt")

(start-margrave-engine #:margrave-params '("-log")
                       ;#:margrave-path "M:\\RktMargrave\\margrave"
                       #:margrave-path "F:\\msysgit\\git\\margrave\\margrave"
                       #:jvm-params '("-Xmx512m"))


;LOAD POLICY "/Users/Salman/Dropbox/PhD projects/programs/access control/UARBAC/margrave/uarbac.p";
(m-load-policy "uarbac" "uarbac.p")

(m-let "Q"
       '([u User] [p Permission] [c Class])
       '(and ([uarbac permit] u p c) 
        (ob 'course 'cs2102)
        (ob 'course 'cs521)        
        (ob 'lab 'alas)
        (ob 'lab 'hci)
        (ob 'department 'cs)
        (rh 'head 'professor)
        (rh 'ta 'student)
        (rh 'ra 'student)
        (ua 'craig 'head)
        ; Dan wasn't declared. Should this be joshua?
       ; (ua 'dan 'professor)
        (ua 'kathi 'professor)
        (ua 'salman 'student)
        (ua 'theo 'student)
        (ua 'tim 'student)
        (ua 'salman 'ra)
        (ua 'tim 'ra)
        (ua 'salman 'ta)
        (ua 'theo 'ta)
        (paa 'head 'admin 'department)
        (paa 'head 'admin 'course)
        (paa 'head 'admin 'lab)
        (pa 'head 'recruit 'department)
        (pa 'professor 'lecture 'course)
        (pa 'student 'register 'course)
        (pa 'ta 'grade 'course)
        (pa 'ra 'research 'lab)
        (= 'department c)))
        ;INCLUDE uarbac:permit
	;CEILING 12;

(m-get "Q"
       #:include '(([uarbac permit] u p c)
                   ([uarbac permit] 'tim p c)
                   ([uarbac permit] 'kathi 'lecture 'course)))

; To pretty-print, use m-show:
(display (m-show "Q"))
