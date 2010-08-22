#lang racket

; Originally written for SISC by tn
; Re-written for Racket by tn

; todo: string buffer in Racket (stream?) to avoid long query-creation time

; todo: too much string manipulation going on here. Is it possible to send back more structured IDB data?
;       for example, why not send a triple: (collectionname, idbname, tupling-data)?

; todo: Use sets rather than lists for efficiency (we do intersections)


(require (file "margrave.rkt"))

; ********************************************************
; Helper functions
; ********************************************************


(define (keys-not-mapped-to-empty a-map)
  (filter (lambda (x) (not (equal? #f x)))
          (hash-map a-map
                    (lambda (key value)
                      (if (not (empty? (hash-ref a-map key)))
                          key
                          #f)))))

(define (makeIdbList lst)
  (if (equal? '() lst)
      ""
      (let ([ idbname (first lst)])
        (string-append idbname 
                       "("
                       reqVector
                       ")"
                       (if (equal? '() (rest lst))
                           ""
                           ", ")
                       (makeIdbList (rest lst))))))

(define (list-intersection lst1 lst2)
  ;(printf "INTERSECT: ~a ~n ~a ~n" lst1 lst2)
  (filter (lambda (x) (not (equal? (member x lst2) #f))) lst1))

; for -keep-collection
; replace rule-name with (string-append pol-name ":" rule-name)

(define (cleanup-idb-list-no-applies thelist)
  (map (lambda (idbname)
         (let* ([qualified-rule (first (regexp-split "_applies\\[" idbname))]
                [fragments (regexp-split ":" qualified-rule)]
                [pol-name (first fragments)]
                [rule-name (string-join (rest fragments) ":")])
           rule-name))
       thelist))

(define reqVector "hostname, entry-interface, src-addr-in, src-addr-out, dest-addr-in, dest-addr-out, protocol, message, flags, src-port-in, src-port-out, dest-port-in, dest-port-out, length, next-hop, exit-interface")

; *********************************************************************

; ********************************************************
; Detecting overlaps for shadowed rules
; ********************************************************

(define (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)

  (let* ([denyOverlapPermitId 
          (xml-explore-result->id (mtext (string-append
                                          "EXPLORE true "
                                          "UNDER inboundacl "
                                          "INCLUDE " (string-append idblistda ", " idblistpn-sup)
                                          " TUPLING")))]                  
         [denyOverlapPermitGet (string-append "SHOW POPULATED " denyOverlapPermitId " " idblistda " FOR CASES " idblistpn-sup)])
    
    (define themap (xml-map-response->map (mtext denyOverlapPermitGet)))
    (define overlapped (keys-not-mapped-to-empty themap))
    
    (printf "Number of never-firing permits overlapped by denies: ~a ~nTime: ~a~n" (length overlapped) (time-since-last))))

(define (find-overlaps-2 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
  (let* ([permitOverlapDenyId 
          (xml-explore-result->id (mtext (string-append 
                                          "EXPLORE true "
                                          "UNDER inboundacl "
                                          "INCLUDE " (string-append idblistpa ", " idblistdn-sup)
                                          " TUPLING")))]         
         
         [permitOverlapDenyGet (string-append "SHOW POPULATED " permitOverlapDenyId " " idblistpa " FOR CASES " idblistdn-sup)])
    
    (define themap (xml-map-response->map (mtext permitOverlapDenyGet)))
    (define overlapped (keys-not-mapped-to-empty themap))
    (printf "Number of never-firing denies overlapped by permits: ~a ~nTime: ~a~n" (length overlapped) (time-since-last))))


; ********************************************************
; Detecting superfluous rules
; ********************************************************


(define (run-timed-script pFileName)
  ; Start the Java process
  (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g") '("-log"))   ;; remove log for benchmarking
  
  ; Start the timer
  (time-since-last)
  
  ; Load the policy
  (let ([polname (load-policy pFileName)])
        
    (printf "Loading took: ~a milliseconds.~n" (time-since-last)) 
    
    (let* ([all-rules (get-rule-list polname)]
           [all-applied (make-applies-list all-rules "InboundACL") ]
           [all-matches (make-matches-list all-rules "InboundACL") ]
           [all-permit-rules (get-rule-list polname "permit")]
           [all-deny-rules (get-rule-list polname "deny")]
           [all-permit-applied (make-applies-list all-permit-rules "InboundACL")]
           [all-deny-applied (make-applies-list all-deny-rules "InboundACL")]
           [all-permit-matches (make-matches-list all-permit-rules "InboundACL")]
           [all-deny-matches (make-matches-list all-deny-rules "InboundACL")]
                
           [idblistmatches (makeIdbList all-matches)]
           [idblistapplied (makeIdbList all-applied)]         
           [neverApplyId 
            (xml-explore-result->id  (mtext  "EXPLORE true "
                                             "UNDER inboundacl "
                                             "INCLUDE " idblistapplied
                                             " TUPLING"))])
      
      
      (printf "Time to make lists, strings, etc.: ~a milliseconds.~n" (time-since-last))
      
      (printf "Number of permit rules: ~a.~nNumber of deny rules: ~a.~nTotal rules: ~a~n" (length all-permit-rules) (length all-deny-rules) (length all-rules))
      (printf "Running superfluous-rule finder...~n")
      
      ; **********************************************************************************************************
      
      (let* ([neverApplyList (cleanup-idb-list-no-applies (xml-set-response->list (mtext (string-append "SHOW UNPOPULATED " neverApplyId " " idblistapplied ))))]
             [prnt (printf "superfluous-rule finder took: ~a milliseconds.~n" (time-since-last))]
             [dbg (printf "~a~n" neverApplyList)]
             [idblistpa (makeIdbList all-permit-applied)]
             [idblistdn-sup (makeIdbList (make-matches-list (list-intersection all-deny-rules neverApplyList) "InboundACL"))]
             [idblistda (makeIdbList all-deny-applied)]
             [idblistpn-sup (makeIdbList (make-matches-list (list-intersection all-permit-rules neverApplyList) "InboundACL"))])
        
        
        (printf "Creating list intersections took: ~a milliseconds.~n" (time-since-last)) 
               
        (printf "superfluous rules count:~n~a~n" (length neverApplyList))
        ;(printf "~a ~n~n~n ~a ~n~n~n" idblistpn-sup idblistdn-sup)
        
        ;(printf "idblists: ~a~n~n~a~n~n" idblistdn-sup idblistpn-sup)
        
       
        
        ; Look for permits overlapping denies (and vice versa)
        (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
        (find-overlaps-2 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
        
        (stop-margrave-engine))))) ; close JVM and end function
