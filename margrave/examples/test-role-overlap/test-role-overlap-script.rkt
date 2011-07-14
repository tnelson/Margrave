#lang racket

; !!! Annoying problem w/ # in interactions window (no need to switch to text mode re: definitions, though.)
; !!! reflection is impossible, it's silly not to allow it
; !!! scripting still ugly, half imported from LISA10 version. module exports from margrave s/b cleaner too.

;(require margrave)
(require (file "../../margrave.rkt"))

; !!! Should use racket/set instead of lists

(define (power-set aset disjoint-pairs)
  (cond [(empty? aset)
         empty]
        [(equal? 1 (length aset))         
         (list aset empty)]
        [else
         (define pivot (first aset))
         (define other-elements (rest aset))         
         
         ; Weed out necessary disjointness now, rather than filtering later.
         (define smaller-power-set-F (power-set other-elements disjoint-pairs))
         (define smaller-power-set-T
           (if (hash-has-key? disjoint-pairs pivot)
               (power-set (remove* (hash-ref disjoint-pairs pivot) other-elements) disjoint-pairs)
               smaller-power-set-F))
          
         
         ; Treat singleton case separately, because otherwise 
         ; there will be nothing to map over in smaller-power-set here.
         (append (map (lambda (sps) (cons pivot sps)) smaller-power-set-T) smaller-power-set-F)]))


(start-margrave-engine #:margrave-params '("-log"))
;(m-load-policy "Mypol" "M:/RktMargrave/margrave/examples/test-role-overlap/testroleoverlap.p")
(m-load-policy "Mypol" "F:/msysgit/git/Margrave/margrave/examples/test-role-overlap/testroleoverlap.p")

(define disjoint-pairs (make-hash))
(define roles '(isFaculty isStudent isAdministrator))
(hash-set! disjoint-pairs 'isFaculty '(isAdministrator))
(hash-set! disjoint-pairs 'isAdministrator '(isFaculty))
(define powerset-of-roles (power-set roles disjoint-pairs))

; SR supports only single atomic fmlas for now. Thus, we need to resort to ispossible to check each set in the powerset.

; Want to support sexprs so we can get rid of this stupid string append business.

;(define (cover-roles-qry aroleset)
;  (fold-append-with-separator 
;   (map (lambda (arole) (if (member arole aroleset)
;                            (string-append (symbol->string arole) "(s)")
;                            (string-append "not " (symbol->string arole) "(s)"))) 
;        roles)
 ;   " and ") )

(define/contract (cover-roles-qry aroleset)
  [list? list?]
  (map (lambda (arole) 
         (if (member arole aroleset)
             `(,arole s)
             `(not (,arole s))))
       roles))


;(define (test-role-combo aroleset)
;  (define thisid (string-upcase (symbol->string (gensym))))
;  (define mystr (string-append 
;                 "let " 
;                 thisid 
;                 ;" [s : Subject ,a : Action,r : Resource] be (Mypol.permit(s,a,r) or Mypol.deny(s,a,r)) and "
;                 
;                 ; orient toward which decisions? here is easy since only 2
;                 " [s : Subject ,a : Action,r : Resource] be Mypol.permit(s,a,r) and Write(a) and "
;                 (cover-roles-qry aroleset)))
;  ;(printf "~a~n" mystr)  
;  (mtext mystr)
;  (define result (xml-bool-response->bool (mtext (string-append "is poss? " thisid))))
;  (printf "Testing set: ~a.~nResult was: ~a~n" aroleset result))

(define (test-role-combo aroleset)
  (define thisid (string-upcase (symbol->string (gensym))))
  
  ; why isn't this (and load) syntax?
  ; then it wouldn't have to be evaluated, right? !!!
  
  ; Can require (for syntax?) the helpers...
  
  ; 
  
  (m-let thisid 
         '([s Subject]
           [a Action]
           [r Resource])
         `(and ((Mypol permit) s a r)
               (Write a)
               ,@(cover-roles-qry aroleset)))

  (define result (m-is-poss? thisid))
  (printf "Testing set: ~a.~nResult was: ~a~n" aroleset result))

(for-each test-role-combo powerset-of-roles)
