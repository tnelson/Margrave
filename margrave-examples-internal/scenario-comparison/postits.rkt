#lang racket

(require "../../margrave/margrave.rkt"
         rackunit)

; Helpers:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Want to use m-scenario? etc. but pretty-printer isn't refactored 
; to accept structs instead of xml yet. Instead...
(define (display-scenarios-to-file qid fileport)
  ;(define result (m-get qid))
  (define resultstr (m-show qid))
  ;(when (m-scenario? result)
  (unless (string-contains? resultstr "---> No more solutions! <---")
    (display resultstr fileport)
    (display "\n\n" fileport)
    (display-scenarios-to-file qid fileport)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (save-all-scenarios qid)    
  (define out (open-output-file (string-append "scenarios-" qid ".txt") #:exists 'replace))
  (display (format "*** Found ~v scenarios for query with ID: ~v. ***~n~n" (m-count-scenarios qid) qid) out)
  (display-scenarios-to-file qid out)
  (close-output-port out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(stop-margrave-engine)