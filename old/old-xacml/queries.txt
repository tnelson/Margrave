;; To load Margrave.
;; You might have to change the path to where you have Margrave installed.
(load-relative "../../code/Margrave.scm")

;; To include Margrave's functions in this namespace.
(require margrave)

;; To remove the timing printouts, uncomment the following line.
;(define time (lambda (x) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   A note about avc-false-test and avc-implication-test
;;
;; These two functions are for testing.  They test for the avc-false? and avc-implication?
;; property.  If the property holds, they return true.  Otherwise, they call the fail-thunk,
;; which is the last argument to these functions.  They pass in the counter-examples to
;; the fail-thunk.
;;
;; For time testing, uncomment the following two lines to remove printing from the timing.
;(define avc-false-test (lambda (set thunk) (avc-false? set)))
;(define avc-implication-test (lambda (l r thunk) (avc-implication? l r)))
;;
;; This will turn off the functions ablity to call the fail thunk and will have it just
;; return #f in the case failure.  This is important for time testing since the fail-thunks
;; include print statments, which are much slower than the actual process that is to be timed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A set of resource classes that are useful to have around for writing properties.
;; These are all the resource classes that have to do with reviews.
(define review-content-set (avc-or (make-avc 'Resource 'resource-class 'paper-review-content_rc)
                                   (make-avc 'Resource 'resource-class 'paper-review-content-commentsAll_rc)
                                   (make-avc 'Resource 'resource-class 'paper-review-content-commentsPc_rc)
                                   (make-avc 'Resource 'resource-class 'paper-review-content-rating_rc)))

(define review-set (avc-or review-content-set
                           (make-avc 'Resource 'resource-class 'paper-review_rc)
                           (make-avc 'Resource 'resource-class 'paper-review-info_rc)
                           (make-avc 'Resource 'resource-class 'paper-review-info-reviewer_rc)
                           (make-avc 'Resource 'resource-class 'paper-review-info-submissionStatus_rc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Properties
;;
;; Note that some of the following properties should fail.
;; Each will state the expected out come.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; No legal request is mapped to NA, i.e., every legal request is decided by either deny or permit.
;; This should hold.
(define (no-nas policy)
  (let [ [the-set (restrict-policy-to-dec 'N policy)] ]
    (avc-false-test the-set (lambda (ccc)
                              (printf "> FAIL since ")
                              (print-avc ccc)
                              #f))))

;; A pc-member may edit reviews of which he is the owner.
;; This should hold.
(define (may-edit-your-review policy)
  (let [ [avc (avc-and (restrict-policy-to-dec 'P policy)
                       (avc-and (avc-not (make-avc 'Subject 'role 'pc-member))
                                (make-avc 'Subject 'isEq-subjUserId-resUserId 'true)
                                (make-avc 'Action 'action-type 'write)))] ]
    (avc-not-empty-test avc (lambda (ccc) 
                              (printf "> FAIL since")
                              (print-avc ccc)
                              #f))))

;; If a subject is not a pc-chair or admin, then he may not set the meeting flag.
;; This should hold.
(define (only-chair-or-admin-call-meeting policy)
  (let [ [a (avc-and (restrict-policy-to-dec 'P policy)
                     (avc-and (avc-not (avc-or (make-avc 'Subject 'role 'pc-chair)
                                               (make-avc 'Subject 'role 'admin)))
                              (make-avc 'Resource 'resource-class 'isMeetingFlag_rc)
                              (make-avc 'Action 'action-type 'write)))] ]
    (avc-false-test a (lambda (ccc) 
                        (printf "> FAIL since ")
                        (print-avc ccc)
                        #f))))

;; If the subject role attribute is empty, return no permits.
;; This should fail.
(define (no-role->no-access policy)
  (let [ [avc (avc-and (restrict-policy-to-dec 'P policy)
                       (apply avc-and (map (lambda (aRole) 
                                             (avc-not (make-avc 'Subject 'role aRole)))   
                                           (get-all-attrValues 'Subject 'role))))] ]
    (avc-false-test avc (lambda (ccc) 
                          (printf "> FAIL since ")
                          (print-avc ccc)
                          #f) )))

;; If the subject role attribute is empty 
;; and the resouce-class is not conferenceInfo_rc, 
;; return no permits.
;; This should hold.
(define (no-role->conf-info-only policy)
  (let [ [avc (avc-and (restrict-policy-to-dec 'P policy)
                       (apply avc-and (map (lambda (aRole) 
                                             (avc-not (make-avc 'Subject 'role aRole)))   
                                           (get-all-attrValues 'Subject 'role)))
                       (avc-not (make-avc 'Resource 'resource-class 'conferenceInfo_rc)))] ]
    (avc-false-test avc (lambda (ccc) 
                          (printf "> FAIL since ")
                          (print-avc ccc)
                          (printf "~a~n" (get-present-attrValues 'Resource 'resource-class ccc))
                          (printf "~a~n" (get-present-attrValues 'Subject 'role ccc))
                          (printf (invariants->string (avc-invariants ccc)))
                          #f))))

;; If some one is not a pc-chair or admin, 
;; he can never see paper-review_rc for which he is conflicted.
;; This should hold.
(define (not-chair-admin->no-see-conf-paper-review_rc policy)
  (let [ [avc (avc-and (restrict-policy-to-dec 'P policy)
                       (avc-and (avc-not (avc-or (make-avc 'Subject 'role 'pc-chair)
                                                 (make-avc 'Subject 'role 'admin)))
                                (make-avc 'Subject 'isConflicted 'true)
                                (make-avc 'Resource 'resource-class 'paper-review_rc)))] ]
    (avc-false-test avc (lambda (ccc) 
                          (printf "> FAIL since ")
                          (print-avc ccc)
                          #f))))

;; If some one is not a pc-chair or admin, 
;; he can never see any part of reviews for which he is conflicted and not his.
;; This should hold.
(define (not-chair-admin->no-see-conflicted-review-content-or-reviewer policy)
  (avc-false-test (avc-and (restrict-policy-to-dec 'P policy)
                           (avc-and (avc-not (avc-or (make-avc 'Subject 'role 'pc-chair)
                                                     (make-avc 'Subject 'role 'admin)))
                                    (make-avc 'Subject 'isConflicted 'true)
                                    (avc-not (make-avc 'Subject 'isEq-subjUserId-resUserId 'true))
                                    (avc-or review-content-set
                                            (make-avc 'Resource 
                                                      'resource-class  
                                                      'paper-review-info-reviewer_rc))))
                  (lambda (ccc) 
                    (printf "> FAIL since ")
                    (print-avc ccc)
                    (printf "~a" (get-present-attrValues 'Resource 'resource-class ccc))
                    (printf (invariants->string (avc-invariants ccc)))
                    #f)))

;; If the subject is a pc-chair who called a meeting, 
;; then he can read anything if the meeting is about it.
;; This should fail.
(define (chair-meeting->see-everything policy)
  (let [ [the-avc (avc-difference (avc-and (make-avc 'Subject 'role 'pc-chair)
                                           (make-avc 'Subject 'isSubjectsMeeting 'true)
                                           (make-avc 'Resource 'isEq-meetingPaper-resId 'true)
                                           (make-avc 'Action 'action-type 'read))
                                  (restrict-policy-to-dec 'E policy))] ]
    (avc-implication-test the-avc 
                          (restrict-policy-to-dec 'P policy)
                          (lambda (ccc) 
                            (printf "> FAIL since ")
                            (print-avc ccc)
                            #f))))

;; If the subject is a pc-chair who called a meeting, 
;; then he can read any part of a review if the meeting is about it.
;; This should pass.
(define (chair-meeting->see-all-reviews policy)
  (let [ [the-avc (avc-difference
                   (avc-and (make-avc 'Subject 'role 'pc-chair)
                            (make-avc 'Subject 'isSubjectsMeeting 'true)
                            (make-avc 'Action 'action-type 'read)
                            (make-avc 'Resource 'isEq-meetingPaper-resId 'true)
                            review-set)
                   (restrict-policy-to-dec 'E policy))] ]
    (avc-implication-test the-avc 
                          (restrict-policy-to-dec 'P policy)
                          (lambda (ccc)
                            (printf "> FAIL since ")
                            (print-avc ccc)
                            (printf "~a~n" (get-present-attrValues 'Resource 'resource-class ccc))
                            (printf "~a~n" (get-present-attrValues 'Subject 'role ccc))
                            (printf (invariants->string (avc-invariants ccc)))
                            #f))))

;; If the subject is a pc-member and the phase is dicussion,
;; then the subject can see all parts of the reviews.
;; This should pass.
(define (no-conflict-discuss->pc-see-reviews policy)
  (avc-implication-test (avc-and (make-avc 'Subject 'isConflicted 'false)
                                 (make-avc 'Subject 'role 'pc-member)
                                 (make-avc 'Action 'action-type 'read)
                                 (make-avc 'Resource 'phase 'discussion)
                                 review-set
                                 (avc-not (restrict-policy-to-dec 'E policy)))
                        (restrict-policy-to-dec 'P policy)
                        (lambda (ccc) 
                          (printf "> FAIL since ")
                          (print-avc ccc)
                          (printf "~a" (get-present-attrValues 'Resource 'resource-class ccc))
                          #f)))

;; If the subject is just a pc-member and submitted the review for a paper,
;; then the subject can see all parts of others reveiws for that paper.
;; This should pass.
(define (no-conflict-submitted->pc-see-reviews policy)
  (avc-implication-test (avc-and (make-avc 'Subject 'isConflicted 'false)
                                 (make-avc 'Subject 'role 'pc-member)
                                 (make-avc 'Subject 'hasSubmittedReviewForResPaper 'true)
                                 (make-avc 'Subject 'subjReviewsThisResPaper 'true)
                                 (make-avc 'Action 'action-type 'read)
                                 review-set
                                 (avc-not (restrict-policy-to-dec 'E policy)))
                        (restrict-policy-to-dec 'P policy)
                        (lambda (ccc) 
                          (printf "> FAIL since ")
                          (print-xadd ccc)
                          (printf "~a" (get-present-attrValues 'Resource 'resource-class ccc))
                          #f)))

;; If the subject is a pc-member, it is not the discussion phase, 
;; and unsubmitted for the review for a paper despite being assigned it,
;; then the subject cannot see all parts of other's reveiws for that paper.
;; This should pass.
(define (unsubmitted-no-discuss->pc-no-see-review-content policy)
  (avc-false-test (avc-and (avc-and (make-avc-singleton 'Subject 'role 'pc-member)
                                    (avc-not (make-avc 'Resource 'phase 'discussion))
                                    (avc-not (make-avc 'Subject 'hasSubmittedReviewForResPaper 'true))
                                    (avc-not (make-avc 'Subject 'isEq-subjUserId-resUserId 'true))
                                    (make-avc 'Subject 'subjReviewsThisResPaper 'true)
                                    (make-avc 'Action 'action-type 'read)
                                    review-content-set)                                    
                           (restrict-policy-to-dec 'P policy))
                  (lambda (ccc) 
                    (printf "> FAIL since ")
                    (print-xadd ccc)
                    (printf "~a~n" (get-present-attrValues 'Resource 'resource-class ccc))
                    (printf (invariants->string (avc-invariants ccc)))
                    #f)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     General Queries
;;
;; The user is not limited to checking properties.
;; Below are examples of queries that return non-boolean information
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prints out the requests that subviewers can proform.
(define (what-can-subviewer-do policy)
  (let [ [set (avc-and (restrict-policy-to-dec 'P policy)
                       (make-avc 'Subject 'role 'subreviewer))] ]
    (print-avc set)
    (get-present-attrValues 'Resource 'resource-class set)))

;; Compares a conflicted and nonconflicted pc-chair.
;; Prints out the requests that a non-conflicted pc-chair can perform, but a conflicted one cannot.
;; The prints out the requests that a conflicted pc-chair can perform, but a non-conflicted one cannot.
(define (conflicted-chair-vs-nonconflicted-chair policy)
  (let [ [non (avc-and (restrict-policy-to-dec 'P policy)
                       (avc-and (make-avc 'Subject 'role 'pc-chair)
                                (avc-not (make-avc 'Subject 'isConflicted 'true))))]
         [con (avc-and (restrict-policy-to-dec 'P policy)
                       (avc-and (make-avc 'Subject 'role 'pc-chair)
                                (make-avc 'Subject 'isConflicted 'true)))] ]
    (printf " non can, con cannot:")
    (print-avc (avc-difference non con))
    (printf "~n con can, non cannot:")
    (print-avc (avc-difference con non))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Testing Fuction
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test only the ones that should hold.
(define (test-true-ones policy)
  (printf "__ no-nas: ") ; fails
  ;  (printf "~a~n" (time (no-nas policy)))
  
  (printf "__ may-edit-your-review: ")
  (printf "~a~n" (time (may-edit-your-review policy)))
  
  (printf "__ only-chair-or-admin-call-meeting: ")
  (printf "~a~n" (time (only-chair-or-admin-call-meeting policy)))
  
  (printf "__ no-role->conf-info-only: ")
  (printf "~a~n" (time (no-role->conf-info-only policy)))
  
  (printf "__ not-chair-admin->no-see-conf-paper-review_rc: ")
  (printf "~a~n" (time (not-chair-admin->no-see-conf-paper-review_rc policy)))  
  
  (printf "__ not-chair-admin->no-see-conflicted-review-content-or-reviewer: ")
  (printf "~a~n" (time (not-chair-admin->no-see-conflicted-review-content-or-reviewer policy)))
  
  (printf "__ chair-meeting->see-all-reviews: ") ; fail
  (printf "~a~n" (time (chair-meeting->see-all-reviews policy)))
  
  (printf "__ no-conflict-discuss->pc-see-reviews: ") ; fail
  (printf "~a~n" (no-conflict-discuss->pc-see-reviews policy))
  
  (printf "__ no-conflict-submitted->pc-see-reviews: ") ; fail
  (printf "~a~n" (no-conflict-submitted->pc-see-reviews policy))
  
  (printf "__ unsubmitted-no-discuss->pc-no-see-reviews: ") ; failed in CodeA
  (printf "~a~n" (unsubmitted-no-discuss->pc-no-see-review-content policy))
  )


;; Will run all the above tests.
(define (test-all policy)
  (test-true-ones policy)
  
  (printf "__ no-role->no-access: ")
  (printf "~a~n" (time (no-role->no-access policy)))
  
  (printf "__ chair-meeting->see-everything: ")
  (printf "~a~n" (time (chair-meeting->see-everything policy)))
  
  (printf "__ what-can-subviewer-do: ")
  (printf "~a~n" (time (what-can-subviewer-do policy)))
  
  (printf "__ conflicted-chair-vs-nonconflicted-chair")
  (printf "~a~n" (time (conflicted-chair-vs-nonconflicted-chair policy)))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Loading and Contraining the Policy
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To load the raw policy code and convert it into a Margrave Policy (an ADD).
(printf "parse and convert: ")    
(define cont-policy-un (time (load-xacml-policy "/pro/web/web/research/plt/tmp/icse2005sub/continue/CodeB/"
                                            "RPSlist.xml")))

(printf "restricting the policy: ")
(define cont-policy (time (make-nonmulti-attribute*
                           (list (list 'Resource 'isPending)
                                 (list 'Subject 'isEq-subjUserId-resUserId) 
                                 (list 'Subject 'isMeeting)
                                 (list 'Subject 'isSubjectsMeeting)
                                 (list 'Subject 'hasSubmittedReviewForResPaper)
                                 (list 'Resource 'isEq-meetingPaper-resId))
                           (make-singleton-attribute*
                            (list (list 'Resource 'resource-class)
                                  (list 'Action 'action-type))
                            (constrain-policy-disjoint 
                             (list (make-avc 'Subject 'role 'subreviewer) 
                                   (avc-or (make-avc 'Subject 'role 'pc-chair)
                                           (make-avc 'Subject 'role 'pc-member)
                                           (make-avc 'Subject 'role 'admin)))
                             (constrain-policy-disjoint 
                              (list (make-avc 'Subject 'role 'pc-chair)
                                    (avc-not (make-avc 'Subject 'role 'pc-member)))
                              (constrain-policy
                               (avc-difference (avc-or (make-avc 'Subject 'isConflicted 'true) 
                                                       (make-avc 'Subject 'isConflicted 'false))
                                               (avc-and (make-avc 'Subject 'isConflicted 'true) 
                                                        (make-avc 'Subject 'isConflicted 'false)))
                               cont-policy-un)))))))

;; Uncomment to have it print the memory usage to the terminal
;(print-add-memory-use cont-policy-un)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Running the Tests
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncomment to see all the different attrId attrValue pairs.
(get-all-attrValues)

;; uncomment to test all the ones that should hold.
;(test-true-ones cont-policy)

;; uncomment to run all tests 
;; (a lot of output, you might what to run one at a time)
(test-all cont-policy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Change Impact
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define old-policy-un (time (load-xacml-policy "/pro/web/web/research/plt/tmp/icse2005sub/continue/CodeA/"
;                                           "RPSlist.xml")))
;
;(define old-policy (time (make-nonmulti-attribute*
;                           (list (list 'Resource 'isPending)
;                                 (list 'Subject 'isEq-subjUserId-resUserId) 
;                                 (list 'Subject 'isMeeting)
;                                 (list 'Subject 'isSubjectsMeeting)
;                                 (list 'Subject 'hasSubmittedReviewForResPaper)
;                                 (list 'Resource 'isEq-meetingPaper-resId))
;                           (make-singleton-attribute*
;                            (list (list 'Resource 'resource-class)
;                                  (list 'Action 'action-type))
;                            (constrain-policy-disjoint 
;                             (list (make-avc 'Subject 'role 'subreviewer) 
;                                   (avc-or (make-avc 'Subject 'role 'pc-chair)
;                                           (make-avc 'Subject 'role 'pc-member)
;                                           (make-avc 'Subject 'role 'admin)))
;                             (constrain-policy-disjoint 
;                              (list (make-avc 'Subject 'role 'pc-chair)
;                                    (avc-not (make-avc 'Subject 'role 'pc-member)))
;                              (constrain-policy
;                               (avc-difference (avc-or (make-avc 'Subject 'isConflicted 'true) 
;                                                       (make-avc 'Subject 'isConflicted 'false))
;                                               (avc-and (make-avc 'Subject 'isConflicted 'true) 
;                                                        (make-avc 'Subject 'isConflicted 'false)))
;                               old-policy-un)))))))
;
;(define change-impact (compare-policies old-policy cont-policy))
;(print-comparison-changes change-impact)

