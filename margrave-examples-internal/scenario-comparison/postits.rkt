#lang racket

(require "../../margrave/margrave.rkt"
         rackunit)

(start-margrave-engine #:margrave-params '("-log")
                       #:margrave-path "../../margrave")
(m-load-policy "mypol2" "small-conference.p")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(m-let "Qrestrictedsize" '([s Subject] [a Action] [r Resource]) 
       '(and ([mypol2 permit] s a r)             
             (ReadPaper a)
             ([mypol2 paperAssigned_applies] s a r)
             (not ([mypol2 paperNoConflict_matches] s a r))
             (forall tr TechReport (forall tr2 TechReport (= tr tr2)))))
(define num-scenarios (m-count-scenarios "Qrestrictedsize"))
(check-true (and num-scenarios
                 (> num-scenarios 0))) 
(save-all-scenarios "Qrestrictedsize")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(stop-margrave-engine)