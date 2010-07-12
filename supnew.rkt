#lang racket

; Originally written for SISC by tn
; Re-written for Racket by tn

; IN PROGRESS
; todo: set all the queries up in order
; todo: string buffer in Racket (stream?) to avoid long query-creation time
; todo: limit by superfluous
; todo: changes to margrave source to make it a module (on the way to becoming a lang?)
; todo: response-result-id, response-list, response-map

; TEMPORARY! Will be a nice module path soon.
(require (file "M:\\RktMargrave\\margrave.scm"))

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

(define (run-timed-script pFileName)
  ; Start the Java process
  (start-margrave-engine)
  
  ; Start the timer
  (time-since-last)

  ; Load the policy
  (load-policy pFileName)
  
  (define loadedTime (time-since-last))
  (printf "Loading took: ~a milliseconds.~n" loadedTime) 
  
  
  (define allIDBs (get-qualified-idbname-list polobj))
  (define listOfApplied (filter-helper (lambda (idbname)
                                         (string-endswith idbname "_applies"))
                                       allIDBs))
  (define listOfNonApplied (removeall (append listOfApplied (list "inboundacl:permit" "inboundacl:deny")) allIDBs))
  
  ; get-decision-for-rule-idbname works only for the base rule name IDB
  (define listOfPermitNonApplied (filter-helper (lambda (idbname) (string=? (get-decision-for-rule-idbname polobj idbname) "permit")) listOfNonApplied))
  (define listOfDenyNonApplied (filter-helper (lambda (idbname) (string=? (get-decision-for-rule-idbname polobj idbname) "deny")) listOfNonApplied))
  
  (define listOfPermitApplied (map (lambda (idbname) (string-append idbname "_applies")) listOfPermitNonApplied))
  (define listOfDenyApplied (map (lambda (idbname) (string-append idbname "_applies")) listOfDenyNonApplied))
  
  
  ;(define allPriorIDBs (rule-idbs-with-higher-priority polobj "ace-line-512"))
  
  
  (printf "Computing IDB strings.~n") 
  (define idblistrules (makeIdbList listOfNonApplied))
  (define idblistapplied (makeIdbList listOfApplied))
  (define idblistpa (makeIdbList listOfPermitApplied))
  (define idblistdn (makeIdbList listOfDenyNonApplied))
  (define idblistda (makeIdbList listOfDenyApplied))
  (define idblistpn (makeIdbList listOfPermitNonApplied))
  
  (define makeStringTime (time-since-last))
  (printf "Time to make lists, strings, etc.: ~a~n." makeStringTime )
  
  ; **********************************************************************************************************
  
  
  ;Which rules never apply?
  (define neverApplyQuery (string-append "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                         " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                         " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                         " Protocol(protocol) AND Hostname(hostname)  "
                                         "UNDER inboundacl "
                                         "IDBOUTPUT " idblistapplied
                                         " TUPLING"))
  (define neverApplyShow (string-append "SHOW UNPOPULATED " idblistapplied))
  (printf "Running superfluous-rule finder.~n")
  
  ; Create the query
  (define superResult (response-result-id (m neverApplyQuery)))
  ; Run the query
  (define superfluouslist (response-list (m neverApplyShow)))
  (define superFindTime (time-since-last))
  (printf "Query took: ~n ~n" superFindTime)
  
  ; **********************************************************************************************************
  
  
  ; Which overlap which?
  ;(define theQuery (string-append "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
  ;                                " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
  ;                                " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
  ;                                " Protocol(protocol) AND Hostname(hostname)  "
  ;                                "UNDER inboundacl "
  ;                                "SHOW POPULATED " idblistapplied
  ;                                "FOR CASES " idblistrules
  ;                                "IDBOUTPUT " idblistall
  ;                                " TUPLING"))
  
  
  
  ; !!!
  ; Need to limit by the superfluous ones?
  
  ; Permit overlapping Deny
  (define theQuery1 (string-append "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                   " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                   " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                   " Protocol(protocol) AND Hostname(hostname)  "
                                   "UNDER inboundacl "
                                   "SHOW POPULATED " idblistpa
                                   "FOR CASES " idblistdn
                                   "IDBOUTPUT " (string-append idblistpa ", " idblistdn)
                                   " TUPLING"))
  
  ; Deny overriding Permit
  (define theQuery2 (string-append "EXPLORE IPAddress(src-addr-in) AND IPAddress(src-addr-out) AND IPAddress(dest-addr-in) AND IPAddress(dest-addr-out) AND "
                                   " Port(dest-port-out) AND Port(dest-port-in) AND Port(src-port-out) AND Port(src-port-in) AND IPAddress(next-hop) AND "
                                   " ICMPMessage(message) AND Interface(entry-interface) AND Interface(exit-interface) and Length(length) AND "
                                   " Protocol(protocol) AND Hostname(hostname)  "
                                   "UNDER inboundacl "
                                   "SHOW POPULATED " idblistda
                                   "FOR CASES " idblistpn
                                   "IDBOUTPUT " (string-append idblistda ", " idblistpn)
                                   " TUPLING"))
  
  
  
  (display "Running query...") (newline)
  ;(define qStartTime (get-system-clock-ms))
  ;(display theQuery) (newline)
  (define qResult1 (m theQuery1))
  (define qResult2 (m theQuery2))
  ;(define qEndTime (get-system-clock-ms))
  (display "Queries took: ") (display (- qEndTime qStartTime)) (display " ms.") (newline)
  
  
  (stop-margrave-engine) ) ; close JVM and end function

; bugs
; (2) have to use all vars in the condition? can't introduce in idbout/pop clauses


