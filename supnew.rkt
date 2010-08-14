#lang racket

; Originally written for SISC by tn
; Re-written for Racket by tn

; IN PROGRESS
; todo: string buffer in Racket (stream?) to avoid long query-creation time

; todo: too much string manipulation going on here. Is it possible to send back more structured IDB data?
;       for example, why not send a triple: (collectionname, idbname, tupling-data)?
; todo: similarly, why lists everywhere? Does Racket have a decent set implementation?

(require (file "margrave.rkt"))

; ********************************************************
; Helper functions
; ********************************************************

; Easy timer function
; Initial value
(define tick-tock #f)
(define (time-since-last)
  (if (eq? tick-tock #f)
      (begin 
        (set! tick-tock (current-inexact-milliseconds))
        #f)
      (let ([ms-to-return (- (current-inexact-milliseconds) tick-tock)]) 
        (set! tick-tock (current-inexact-milliseconds))
        ms-to-return)))

;(define (string-endswith str end)
;  (if (> (string-length end) (string-length str))
;      #f
;      (string=? end
;                (substring str
;                           (- (string-length str) (string-length end))
;                           (string-length str)))))

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

; Strip everything up to and including the last :
;(define (unqualified-part idbname)
;  (last (regexp-split ":" idbname)))

;(define (unqualified-non-applied-part idbname)
;  (unqualified-part (if (string-endswith idbname "_applies")
;                        (substring idbname 0 (- (string-length idbname) 8))
;                        idbname)))

; kludge: in general there may be brackets in the idb name. For our example, there aren't. (see todo above re: structured data)
;(define (cleanup-idb-list thelist)
;  (map (lambda (idbname) 
;         (second (regexp-split ":" (first (regexp-split "\\[" idbname))))) 
;       thelist))
;(define (cleanup-idb-list-no-applies thelist)
;  (map (lambda (idbname) 
;         (second (regexp-split ":" (first (regexp-split "_applies\\[" idbname))))) 
;       thelist))
(define (cleanup-idb-list-no-applies-keep-collection thelist)
  (map (lambda (idbname) 
         (first (regexp-split "_applies\\[" idbname)))
       thelist))

(define reqVector "hostname, entry-interface, src-addr-in, src-addr-out, dest-addr-in, dest-addr-out, protocol, message, src-port-in, src-port-out, dest-port-in, dest-port-out, length, next-hop, exit-interface")

; *********************************************************************

; ********************************************************
; Detecting overlaps for shadowed rules
; ********************************************************


(define (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
  ; !!! todo: grossly in-efficient to play with lists rather than appropriately structured sets
  (let* ([denyOverlapPermitId 
          (xml-explore-result->id (mtext (string-append
                                          "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                          " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                          " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                          " Protocol(protocol) AND Hostname(hostname)  "
                                          "UNDER inboundacl "
                                          "INCLUDE " (string-append idblistda ", " idblistpn-sup)
                                          " TUPLING")))]                  
         [denyOverlapPermitGet (string-append "SHOW POPULATED " denyOverlapPermitId " " idblistda " FOR CASES " idblistpn-sup)])
    
    ;(printf " --->   ~a~n" permitOverlapDenyGet)
    ;(printf " ===> ~a~n" idblistpa)
    ;(printf " +++> ~a~n" idblistdn-sup)
    
    ; Some empties are to be expected (denies only overlapped by deny, etc.)
    
;    (m denyOverlapPermitGet)
    (define themap (mtext denyOverlapPermitGet))
    (printf "Time: ~a~n" (time-since-last))))

(define (find-overlaps-2 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
  ; !!! todo: grossly in-efficient to play with lists rather than appropriately structured sets
  (let* ([permitOverlapDenyId 
          (xml-explore-result->id (mtext (string-append 
                                          "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                          " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                          " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                          " Protocol(protocol) AND Hostname(hostname)  "
                                          "UNDER inboundacl "
                                          "INCLUDE " (string-append idblistpa ", " idblistdn-sup)
                                          " TUPLING")))]         
         
         [permitOverlapDenyGet (string-append "SHOW POPULATED " permitOverlapDenyId " " idblistpa " FOR CASES " idblistdn-sup)])
    
    (define themap (mtext permitOverlapDenyGet))
    (printf "Time: ~a~n" (time-since-last))))


(define (make-applied-list rlist)
  (map (lambda (idbname)
         (string-append idbname "_applies"))
       rlist))

; ********************************************************
; Detecting superfluous rules
; ********************************************************


(define (run-timed-script pFileName)
  ; Start the Java process
  (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g"))
  
  ; Start the timer
  (time-since-last)
  
  ; Load the policy
  (let ([polname (load-policy pFileName)])
        
    (printf "Loading took: ~a milliseconds.~n" (time-since-last)) 
    
    (let* ([all-rules (get-qualified-rule-list polname)]
           [all-applied (make-applied-list all-rules) ]
           [all-permit-rules (get-qualified-rule-list polname "permit")]
           [all-deny-rules (get-qualified-rule-list polname "deny")]
           [all-permit-applied (make-applied-list all-permit-rules)]
           [all-deny-applied (make-applied-list all-deny-rules)]
           
          ; [dbg (printf "~a ~n ~a~n" all-deny-rules all-permit-applied)]       

           [idblistrules (makeIdbList all-rules)]
           [idblistapplied (makeIdbList all-applied)]         
           
           [neverApplyId 
            (xml-explore-result->id  (mtext (string-append 
                                             "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                             " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                             " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                             " Protocol(protocol) AND Hostname(hostname)  "
                                             "UNDER inboundacl "
                                             "INCLUDE " idblistapplied
                                             " TUPLING")))])
      
      
      (printf "Time to make lists, strings, etc.: ~a milliseconds.~n" (time-since-last))
      (printf "Running superfluous-rule finder...~n")
      
      ; **********************************************************************************************************
      ;; !!! todo: needs to be SHOW (a bit confusing); also id comes first...
      
      (let* ([neverApplyList (cleanup-idb-list-no-applies-keep-collection (xml-set-response->list (mtext (string-append "SHOW UNPOPULATED " neverApplyId " " idblistapplied ))))]
             [prnt (printf "superfluous-rule finder took: ~a milliseconds.~n" (time-since-last))]
             [prnt2 (printf "superfluous rules: ~a~n"  neverApplyList)]
             [idblistpa (makeIdbList all-permit-applied)]
             [idblistdn-sup (makeIdbList (list-intersection all-deny-rules neverApplyList))]
             [idblistda (makeIdbList all-deny-applied)]
             [idblistpn-sup (makeIdbList (list-intersection all-permit-rules neverApplyList))])
        
        
        (printf "Creating list intersections took: ~a milliseconds.~n" (time-since-last)) 
               
        (printf "superfluous rules count:~n~a~n" (length neverApplyList))
        ;(printf "~a ~n ~a ~n" idblistpn-sup idblistdn-sup)
        
        ;(printf "idblists: ~a~n~n~a~n~n" idblistdn-sup idblistpn-sup)
        
        ; Look for permits overlapping denies (and vice versa)
        (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
        (find-overlaps-2 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
        
        (stop-margrave-engine))))) ; close JVM and end function

; bugs
; (2) have to use all vars in the condition? can't introduce in idbout/pop clauses
