#lang racket/gui

; Not (require margrave) for now; in development.
(require (file "../../margrave.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Program to discover which role-additions result in increased access.
; TN July 2011
;
; Implementation notes:
; - The library ought to provide reflection. 
; - Should use racket/set w/ iteration over set instead of lists.
;
; Most importantly: this code looks for scenario pairs, caring only
; about role information. If there are other predicates that matter:
; e.g. state or information about supervision, conflicts, etc. they
; will be ignored. 

; One soln would be to seek a single
; scenario (over subj1, subj2, a, r) and hard-code equivalence between
; subj1 and subj2 in the query. This is a pain to write without
; reflection, which is forthcoming.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Set up the hard-coded list of roles and disjointness constraints.
(define disjoint-pairs (make-hash))
(define roles '(isFaculty isStudent isAdministrator))
(hash-set! disjoint-pairs 'isFaculty '(isAdministrator))
(hash-set! disjoint-pairs 'isAdministrator '(isFaculty))

; Caches for set-containment relationships and query results
(define immediate-children (make-hash))
(define can-be-permitted (make-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; power-set: list(role) -> list(list(role))
; References disjoint-pairs.
; Produce the power set of a set, respecting disjointness constraints.
(define (power-set aset)
  (cond [(empty? aset) empty]
        [(equal? 1 (length aset)) 
         (list aset empty)]
        [else
         (define pivot (first aset))
         (define other-elements (rest aset))         
         
         ; Weed out necessary disjointness now, rather than filtering later. 
         ; Since we consider disjointness, need to handle the +pivot, -pivot options separately.
         (define smaller-power-set-F (power-set other-elements))
         (define smaller-power-set-T
           (if (hash-has-key? disjoint-pairs pivot)
               (power-set (remove* (hash-ref disjoint-pairs pivot) other-elements))
               smaller-power-set-F))
                   
         ; Treat singleton case separately, because otherwise 
         ; there will be nothing to map over in smaller-power-set-T here.
         (define new-sets-with-pivot (map (lambda (sps) (cons pivot sps))
                                          smaller-power-set-T))
                  
         ; return the new sets union the old.
         (append new-sets-with-pivot smaller-power-set-F)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compute the power set.
(define powerset-of-roles (power-set roles))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; all-minus-one-element: list(role) -> list(list(role))
; Given a set of size n, produce the set of subsets of size (n-1).
(define (all-minus-one-element aset)
  (map (lambda (x) (remove x aset))
       aset))

; Compute the immediate children of every role-set. An immediate
; child is missing only one role.
(for-each (lambda (aset) 
            (unless (empty? aset)
              (hash-set! immediate-children 
                         aset 
                         (all-minus-one-element aset))))
          powerset-of-roles)
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(start-margrave-engine #:margrave-params '("-log"))

; Hard-coded policy path for now. Easy enough to lift this to a function later.
;(m-load-policy "Mypol" "M:/RktMargrave/margrave/examples/test-role-overlap/testroleoverlap.p")
(m-load-policy "Mypol" "F:/msysgit/git/Margrave/margrave/examples/test-role-overlap/testroleoverlap.p")

; cover-roles-qry: list(role) -> s-expression
; Produce a list of (Role s) or (not (Role s)) expressions for use in query.
(define (cover-roles-qry aroleset)
  (map (lambda (arole) 
         (if (member arole aroleset)
             `(,arole s)
             `(not (,arole s))))
       roles))

; test-role-combo: list(role) -> void
; Populates the can-be-permitted cache for each role-set.
; Specifically checking for WRITE action.
(define (test-role-combo aroleset)
  (define thisid (string-upcase (symbol->string (gensym))))
    
  (define fmla-sexpr `(and ([Mypol permit] s a r)
                           (Write a)
                           ,@(cover-roles-qry aroleset))) 
  (m-let thisid 
         '([s Subject]
           [a Action]
           [r Resource])
         fmla-sexpr)

  (define result (m-is-poss? thisid))
 ; (printf "Testing set: ~a.~nResult was: ~a~n" aroleset result)
  (hash-set! can-be-permitted aroleset result))

; Compute which role combinations are permitted. One Margrave query
; for each element of the power set of roles.
(for-each test-role-combo powerset-of-roles)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now, which elements have a "disagreeable" child?
; e.g. Student+Faculty permits, but Student does not.
; We have: (a) a table from sets to boolean [can be permitted?] and
;          (b) a table from sets to lists of sets [children of].

(define (check-disagreeable-children aset)
  (unless (empty? aset)
    (for-each (lambda (achild) (unless (equal? (hash-ref can-be-permitted aset)
                                               (hash-ref can-be-permitted achild))
                                 (printf "Disagreement between ~a and ~a: ~a vs. ~a.~n" 
                                         aset achild (hash-ref can-be-permitted aset) (hash-ref can-be-permitted achild)))) 
              (hash-ref immediate-children aset))))
(for-each check-disagreeable-children powerset-of-roles)
  