#lang racket

; Originally written for SISC by tn
; Re-written for Racket by tn

; IN PROGRESS
; todo: string buffer in Racket (stream?) to avoid long query-creation time
; todo: limit by superfluous
; todo: xml-list->list
;       xml-map->map
;       xml-id->id

; TEMPORARY! Will be a nice module path soon.
;(require (file "M:\\RktMargrave\\margrave.scm"))
(require (file "F:\\msysgit\\git\\Margrave\\margrave.scm"))


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

(define (string-endswith str end)
  (if (> (string-length end) (string-length str))
      #f
      (string=? end
                (substring str
                           (- (string-length str) (string-length end))
                           (string-length str)))))

(define (makeIdbList lst)
  (if (eqv? '() lst)
      ""
      (let ([ idbname (car lst)])
        (string-append idbname 
                       "("
                       reqVector
                       ")"
                       (if (eqv? '() (cdr lst))
                           ""
                           ", ")
                       (makeIdbList (cdr lst))))))

(define reqVector "hostname, entry-interface, src-addr-in, src-addr-out, dest-addr-in, dest-addr-out, protocol, message, src-port-in, src-port-out, dest-port-in, dest-port-out, length, next-hop, exit-interface")

; *********************************************************************

(define (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn idblistdn)
  (let* ([permitOverlapDenyId (string-append "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                             " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                             " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                             " Protocol(protocol) AND Hostname(hostname)  "
                                             "UNDER inboundacl "
                                             "IDBOUTPUT " (string-append idblistpa ", " idblistdn)
                                             " TUPLING")]
         [denyOverlapPermitId (string-append "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                             " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                             " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                             " Protocol(protocol) AND Hostname(hostname)  "
                                             "UNDER inboundacl "
                                             "IDBOUTPUT " (string-append idblistda ", " idblistpn)
                                             " TUPLING")]
         
         [permitOverlapDenyGet (string-append "SHOW POPULATED " idblistpa " FOR CASES " idblistdn)]
         [denyOverlapPermitGet (string-append "SHOW POPULATED " idblistda " FOR CASES " idblistpn)])
    
    (printf "P overlap Superf. D: ~a ~n" (xml-map->map (m permitOverlapDenyGet)))
    (printf "Time: ~a~n" (time-since-last))
    
    (printf "D overlap Superf. P: ~a ~n" (xml-map->map (m denyOverlapPermitGet)))
    (printf "Time: ~a~n" (time-since-last))
    
    ; !!! todo: limit cases by neverapplylist
    
    ))

; Strip everything up to and including the last :
(define (unqualified-part idbname)
  (last (regexp-split ":" idbname)))

(define (unqualified-non-applied-part idbname)
  (unqualified-part (if (string-endswith idbname "_applies")
                        (substring idbname 0 (- (string-length idbname) 8))
                        idbname)))

(define (run-timed-script pFileName)
  ; Start the Java process
  (start-margrave-engine "-Xss2048k -Xmx1g")
  
  ; Start the timer
  (time-since-last)
  
  ; Load the policy
  (let ([polname (load-policy pFileName)])
        
    (printf "Loading took: ~a milliseconds.~n" (time-since-last)) 
    
    (let* ([ allIDBs (get-qualified-idbname-list polname)]
         ;  [dbg (printf "~a~n" allIDBs)]
           [listOfApplied (filter (lambda (idbname)
                                    (string-endswith idbname "_applies"))
                                  allIDBs)]
           
           ; (filter (lambda (idbname) (not (string?  (get-decision-for-rule-idbname "inboundacl" (unqualified-non-applied-part idbname))))) (get-qualified-idbname-list "inboundacl"))
           [listOfNonApplied (remove* (append listOfApplied 
                                              (list "inboundacl:permit" "inboundacl:deny" "inboundacl:drop" "inboundacl:advertise" 
                                                    "inboundacl:forward" "inboundacl:translate" "inboundacl:pass" "inboundacl:encrypt" "inboundacl:route")) allIDBs)]
           
           ; get-decision-for-rule-idbname works only for the base rule name IDB, no idb collection name, no _applies
           [listOfPermitNonApplied (filter (lambda (idbname) (string=? (get-decision-for-rule-idbname polname (unqualified-part idbname)) "permit")) listOfNonApplied)]
           [listOfDenyNonApplied (filter (lambda (idbname) (string=? (get-decision-for-rule-idbname polname (unqualified-part idbname)) "deny")) listOfNonApplied)]
           [listOfPermitApplied (map (lambda (idbname) (string-append idbname "_applies")) listOfPermitNonApplied)]
           [listOfDenyApplied (map (lambda (idbname) (string-append idbname "_applies")) listOfDenyNonApplied)]
           [idblistrules (makeIdbList listOfNonApplied)]
           [idblistapplied (makeIdbList listOfApplied)]
           [idblistpa (makeIdbList listOfPermitApplied)]
           [idblistdn (makeIdbList listOfDenyNonApplied)]
           [idblistda (makeIdbList listOfDenyApplied)]
           [idblistpn (makeIdbList listOfPermitNonApplied)]
           [neverApplyId (xml-id->id (m (string-append "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                                       " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                                       " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                                       " Protocol(protocol) AND Hostname(hostname)  "
                                                       "UNDER inboundacl "
                                                       "IDBOUTPUT " idblistapplied
                                                       " TUPLING")))])
      
      
      (printf "Time to make lists, strings, etc.: ~a~n." (time-since-last))
      (printf "Running superfluous-rule finder...~n")
     ; (printf "List of all IDBs: ~a~n" allIDBs)
      
      ; **********************************************************************************************************
      ;; !!! todo: needs to be SHOW (a bit confusing); also id comes first...
      (let ([neverApplyList (xml-list->list (m (string-append "SHOW UNPOPULATED " neverApplyId " "idblistapplied )))])
        
        
        ;; !!! todo it's a SET, not a LIST.
        
        (printf "superfluous-rule finder took: ~a ~n" (time-since-last))        
        (printf "Results:~n~n~a~n" neverApplyList)
        ; Look for permits overlapping denies (and vice versa)
        (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn idblistdn)
        
        
        (stop-margrave-engine))))) ; close JVM and end function

; bugs
; (2) have to use all vars in the condition? can't introduce in idbout/pop clauses


