;; To load Margrave.
;; You might have to change the path to where you have Margrave installed.
(load-relative "../../code/Margrave.scm")

;; To include Margrave's functions in this namespace.
(require margrave)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Properties
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pr_1
;; Policy -> bool
;; "There do not exist members of Student who can Assign ExternalGrades."
;; This is implimented by ensuring the no request falls into both the set of 
;; requests that include the role Student, resource-class ExternalGrades,
;; and command Assign and the set of requests that are permited.
(define (Pr_1 policy)
  (avc-false? (avc-and (restrict-policy-to-dec 'P policy)
                       (avc-and (make-avc 'Subject 'role 'Student) 
                                (make-avc 'Resource 'resource-class 'ExternalGrades)
                                (make-avc 'Action 'command 'Assign)))))

;; Policy -> avc
;; This produces all the counter-examples
;; to Pr_1.  
;; If none exist (the property holds),
;; the empty avc is returned.
(define (find-Pr_1-counter-example policy)
  (avc-and (restrict-policy-to-dec 'P policy)
           (avc-and (make-avc 'Subject 'role 'Student) 
                    (make-avc 'Resource 'resource-class 'ExternalGrades)
                    (make-avc 'Action 'command 'Assign))))

;; Pr_2
;; Policy -> bool
;; "All members of Faculty can Assign both InternalGrades and ExternalGrades."
;; This is implimented by checking to ensure that all requests that include the 
;; role Faculty, either the resouce-class InternalGrades or ExternalGrades and
;; the command Assign are within the set of permited requests.
(define (Pr_2 policy)
  (avc-implication? (avc-and (make-avc 'Subject 'role 'Faculty)
                             (avc-or (make-avc 'Resource 'resource-class 'InternalGrades)
                                     (make-avc 'Resource 'resource-class 'ExternalGrades))
                             (make-avc 'Action 'command 'Assign))
                    (avc-or (restrict-policy-to-dec 'P policy)
                            (restrict-policy-to-dec 'E policy))))

;; Policy -> avc
;; This produces all the counter-examples
;; to Pr_2.  
;; If none exist (the property holds),
;; the empty avc is returned.
(define (find-Pr_2-counter-example policy)
  (avc-difference (avc-and (make-avc 'Subject 'role 'Faculty)
                           (avc-or (make-avc 'Resource 'resource-class 'InternalGrades)
                                   (make-avc 'Resource 'resource-class 'ExternalGrades))
                           (make-avc 'Action 'command 'Assign))
                  (avc-or (restrict-policy-to-dec 'P policy)
                          (restrict-policy-to-dec 'E policy))))


;; Pr_3
;; Policy -> bool
;; "There exists no combination of roles such that a user with those roles can both
;;  Receive and Assign the resource ExternalGrades."
;; This is implimented by first getting the set of requests that can 
;; Assign ExternalGrades (assign) and the set of requests that can Receive 
;; ExternalGrades (receive).  Then we check that the intersection between the 
;; set of role combinations found in each set of requests is empty.
(define (Pr_3 policy)
  (let [ [assign (avc-and (restrict-policy-to-dec 'P policy)
                          (avc-and (make-avc 'Resource 'resource-class 'ExternalGrades) 
                                   (make-avc 'Action 'command 'Assign)))]
         [receive (avc-and (restrict-policy-to-dec 'P policy)
                           (avc-and (make-avc 'Resource 'resource-class 'ExternalGrades) 
                                    (make-avc 'Action 'command 'Receive)))] ]
    (avc-false? (avc-and (present-combo-attrValues 'Subject 'role assign)
                         (present-combo-attrValues 'Subject 'role receive)))))
;; Policy -> avc
;; This produces all the counter-examples
;; to Pr_3.  
;; If none exist (the property holds),
;; the empty avc is returned.
(define (find-Pr_3-counter-example policy)
  (let [ [assign (avc-and (restrict-policy-to-dec 'P policy)
                          (avc-and (make-avc 'Resource 'resource-class 'ExternalGrades) 
                                   (make-avc 'Action 'command 'Assign)))]
         [receive (avc-and (restrict-policy-to-dec 'P policy)
                           (avc-and (make-avc 'Resource 'resource-class 'ExternalGrades) 
                                    (make-avc 'Action 'command 'Receive)))] ]
    (avc-and (present-combo-attrValues 'Subject 'role assign)
             (present-combo-attrValues 'Subject 'role receive))))

;; Pr_4
;; Policy -> bool
;; "All members of FacultyFamily can Receive ExternalGrades."
;; This is implimented by checking to ensure that all requests that include the 
;; role FacultyFamily, the resource-class ExternalGrades and the command Assign 
;; are within the set of permited requests.
(define (Pr_4 policy)
  (avc-implication? (avc-and (make-avc 'Subject 'role 'FacultyFamily) 
                             (make-avc 'Resource 'resource-class 'ExternalGrades) 
                             (make-avc 'Action 'command 'Receive)) 
                    (avc-or (restrict-policy-to-dec 'P policy)
                            (restrict-policy-to-dec 'E policy))))
;; Policy -> avc
;; This produces all the counter-examples
;; to Pr_4.  
;; If none exist (the property holds),
;; the empty avc is returned.
(define (find-Pr_4-counter-example policy)
  (avc-difference (avc-and (make-avc 'Subject 'role 'FacultyFamily) 
                           (make-avc 'Resource 'resource-class 'ExternalGrades) 
                           (make-avc 'Action 'command 'Receive))
                  (avc-or (restrict-policy-to-dec 'P policy)
                          (restrict-policy-to-dec 'E policy))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test Suit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; policy * [bool] -> bool
(define test-suit1 
  (opt-lambda (policy [printing? #t])
    ;(print-add-memory-use policy)
    
    (printf "> Pr_1: ")
    (let [ [result1 (time (Pr_1 policy))] ]         
      (if printing?
          (if result1
              (printf "Passed Pr_1~n")
              (begin
                (printf "FAILED Pr_1~n Counter-example:~n")
                (print-avc (find-Pr_1-counter-example policy)))))
      
      
      (printf "> Pr_2: ")
      (let [ [result2 (time (Pr_2 policy))] ]
        (if printing?
            (if result2
                (printf "Passed Pr_2~n")
                (begin
                  (printf "FAILED Pr_2~n Counter-example:~n")
                  (print-avc (find-Pr_2-counter-example policy)))))
        
        (printf "> Pr_3: ")
        (let [ [result3 (time (Pr_3 policy))] ]
          (if printing?
              (if result3
                  (printf "Passed Pr_3~n")
                  (begin
                    (printf "FAILED Pr_3~n Counter-example:~n")
                    (print-avc (find-Pr_3-counter-example policy)))))
          
          (and result1 result2 result3))))))

;; policy * [bool] -> bool
(define test-suit2 
  (opt-lambda (policy [printing? #t])
    (let [ [test1-result (test-suit1 policy)] ]
      (printf "> Pr_4: ")
      (let [ [result4 (time (Pr_4 policy))] ]
        (if printing?
            (if result4
                (printf "Passed Pr_4~n")
                (begin
                  (printf "FAILED Pr_4~n Counter-example:~n")
                  (print-avc (find-Pr_4-counter-example policy)))))
        (and test1-result result4)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test Runs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(newline)(newline)(display "===== Pol_1 =====")(newline)
(display "== the first attempt")(newline)
(printf "loading: ")

(define Pol_1 (time (load-xacml-policy "CodeA/" "RPSlist.xml")))

(test-suit1 Pol_1)


;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_2 =====")(newline)
(display "== make command and resource-class single value attributes")(newline)
(printf "loading: ")

(define Pol_2 (time (make-singleton-attribute 'Action 'command
                                              (make-singleton-attribute 'Resource 'resource-class
                                                                        Pol_1))))

(test-suit1 Pol_2)

;;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_3 =====")(newline)
(display "== constrain out student-faculty")(newline)
(printf "loading: ")

(define Pol_3 (time (constrain-policy-disjoint (list (make-avc 'Subject 'role 'Faculty)
                                                     (make-avc 'Subject 'role 'Student))
                                               Pol_2)))
;(print-policy Pol_3)
(test-suit1 Pol_3)


;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_4 =====")(newline)
(display "== add TAs")(newline)
(printf "loading: ")

(define Pol_4
  (time (constrain-policy-disjoint 
         (list (make-avc 'Subject 'role 'Faculty)
               (make-avc 'Subject 'role 'Student))
         (make-singleton-attribute 
          'Action 
          'command
          (make-singleton-attribute 
           'Resource 
           'resource-class
           (load-xacml-policy "CodeB/" "RPSlist.xml"))))))
           
(test-suit1 Pol_4)



;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_5 =====")(newline)
(display "== fix TAs")(newline)
(printf "loading: ")

(define Pol_5
  (time (constrain-policy-disjoint 
         (list (make-avc 'Subject 'role 'Faculty)
               (make-avc 'Subject 'role 'Student))
         (make-singleton-attribute 
          'Action 
          'command
          (make-singleton-attribute 
           'Resource 
           'resource-class
           (load-xacml-policy "CodeC/" "RPSlist.xml"))))))
           
(test-suit1 Pol_5)

;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_6 =====")(newline)
(display "== add FacultyFamily")(newline)
(printf "loading: ")

(define Pol_6
  (time (constrain-policy-disjoint 
         (list (make-avc 'Subject 'role 'Faculty)
               (make-avc 'Subject 'role 'Student))
         (make-singleton-attribute 
          'Action 
          'command
          (make-singleton-attribute 
           'Resource 
           'resource-class
           (load-xacml-policy "CodeD/" "RPSlist.xml"))))))

(test-suit2 Pol_6)


;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_7 =====")(newline)
(display "== constrain out subjects with both Faculty and FacultyFamily")(newline)
(printf "loading: ")

(define Pol_7 (time (constrain-policy-disjoint (list  (make-avc 'Subject 'role 'Faculty)  
                                                      (make-avc 'Subject 'role 'FacultyFamily))
                                               Pol_6)))

(test-suit2 Pol_7)
