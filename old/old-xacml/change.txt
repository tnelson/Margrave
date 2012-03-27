;; To load Margrave.
;; You might have to change the path to where you have Margrave installed.
(load-relative "../../code/Margrave.scm")

;; To include Margrave's functions in this namespace.
(require margrave)

;; An example of a query over a change impact (not in section 3.2):
(define (external-grades-no-change comp)  
  (avc-false? (avc-and (get-combo-attrVal-involved-in-change 'Resource 'resource-class comp)
                       (make-avc 'Resource 'resource-class 'ExternalGrades))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test Runs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(newline)(newline)(display "===== Pol_3 vs Pol_4 =====")(newline)

(printf "loading Pol_3: ")
(define Pol_3 (time (constrain-policy-disjoint 
                     (list (make-avc 'Subject 'role 'Faculty)
                           (make-avc 'Subject 'role 'Student))
                     (make-singleton-attribute 
                      'Action 
                      'command
                      (make-singleton-attribute
                       'Resource
                       'resource-class
                       (load-xacml-policy "CodeA/" "RPSlist.xml"))))))

(printf "loading Pol_4: ")
(define Pol_4 (time (constrain-policy-disjoint 
                     (list (make-avc 'Subject 'role 'Faculty)
                           (make-avc 'Subject 'role 'Student))
                     (make-singleton-attribute 
                      'Action 
                      'command
                      (make-singleton-attribute
                       'Resource
                       'resource-class
                       (load-xacml-policy "CodeB/" "RPSlist.xml"))))))

(printf "comparing Pol_3 and Pol_4: ")
(define comp3to4 (time (compare-policies Pol_3 Pol_4)))
(print-comparison-changes comp3to4)
;; note that print-comparison-changes only prints the parts of the comparison 
;; that have changes in it (e.g. N->P, but not N->N).  
;; This will print the whole comparision: (print-comparison comp3to4)
;(print-add-memory-use comp3to4)

;; example of query
(printf "~nquery: ~a~n" (external-grades-no-change comp3to4))
 
;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_3 vs Pol_5 =====")(newline)
(display "== fix TAs")(newline)

(printf "loading Pol_5: ")
(define Pol_5 (time (constrain-policy-disjoint 
                     (list (make-avc 'Subject 'role 'Faculty)
                           (make-avc 'Subject 'role 'Student))
                     (make-singleton-attribute 
                      'Action 
                      'command
                      (make-singleton-attribute
                       'Resource
                       'resource-class
                       (load-xacml-policy "CodeC/" "RPSlist.xml"))))))


(printf "comparing Pol_3 and Pol_5: ")
(define comp3to5 (time (compare-policies Pol_3 Pol_5)))
(print-comparison-changes comp3to5)
;(print-add-memory-use comp3to5)

;; example of query
(printf "~nquery: ~a~n" (external-grades-no-change comp3to5))

;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_5 vs Pol_6 =====")(newline)
(display "== add FacultyFamily")(newline)

(printf "loading Pol_6: ")
(define Pol_6 (time (constrain-policy-disjoint 
                     (list (make-avc 'Subject 'role 'Faculty)
                           (make-avc 'Subject 'role 'Student))
                     (make-singleton-attribute 
                      'Action 
                      'command
                      (make-singleton-attribute
                       'Resource
                       'resource-class
                       (load-xacml-policy "CodeD/" "RPSlist.xml"))))))

(printf "comparing Pol_5 and Pol_6: ")
(define comp5to6 (time (compare-policies Pol_5 Pol_6)))
(print-comparison-changes comp5to6)
;(print-add-memory-use comp5to6)


;;;;;;;;;;;;;;;;;;;;;;;;;
(newline)(newline)(display "===== Pol_5 vs Pol_7 =====")(newline)
(display "== no fac facfam contr")(newline)

(printf "loading Pol_7: ")
(define Pol_7 (time (constrain-policy-disjoint (list  (make-avc 'Subject 'role 'Faculty)  
                                                      (make-avc 'Subject 'role 'FacultyFamily))
                                               Pol_6)))

(printf "comparing Pol_5 and Pol_7: ")
(define comp5to7 (time (compare-policies Pol_5 Pol_7)))
(print-comparison-changes comp5to7)
;(print-add-memory-use comp5to7)
