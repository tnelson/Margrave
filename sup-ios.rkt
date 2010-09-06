; Copyright © 2009-2010 Brown University and Worcester Polytechnic Institute.
;
; This file is part of Margrave.

; Margrave is free software: you can redistribute it and/or modify
; it under the terms of the GNU Lesser General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; Margrave is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU Lesser General Public License for more details.
;
; You should have received a copy of the GNU Lesser General Public License
; along with Margrave. If not, see <http://www.gnu.org/licenses/>.

#lang racket

; Originally written for SISC by tn
; Re-written for Racket by tn

; todo: string buffer in Racket to avoid long query-creation time

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

(define (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup log-file)
  (time-since-last) ;; reset
  (let* ([denyOverlapPermitId 
          (xml-explore-result->id (mtext (string-append
                                          "EXPLORE true "
                                          "UNDER inboundacl "
                                          "INCLUDE " (string-append idblistda ", " idblistpn-sup)
                                          " TUPLING")))]                  
         [denyOverlapPermitGet (string-append "SHOW REALIZED " denyOverlapPermitId " " idblistda " FOR CASES " idblistpn-sup)])
    
    (define themap (xml-map-response->map (mtext denyOverlapPermitGet)))
    (define n-runtime (time-since-last))
    (define overlapped (keys-not-mapped-to-empty themap))
    (write-string (string-append (number->string n-runtime) ", ") log-file)
    (printf "Number of never-firing permits overlapped by denies: ~a ~nTime: ~a~n" (length overlapped) n-runtime)))

(define (find-overlaps-2 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup log-file)
  (time-since-last) ;; reset
  (let* ([permitOverlapDenyId 
          (xml-explore-result->id (mtext (string-append 
                                          "EXPLORE true "
                                          "UNDER inboundacl "
                                          "INCLUDE " (string-append idblistpa ", " idblistdn-sup)
                                          " TUPLING")))]         
         
         [permitOverlapDenyGet (string-append "SHOW REALIZED " permitOverlapDenyId " " idblistpa " FOR CASES " idblistdn-sup)])
    
    (define themap (xml-map-response->map (mtext permitOverlapDenyGet)))    
    (define n-runtime (time-since-last))
    (define overlapped (keys-not-mapped-to-empty themap))
    (write-string (string-append (number->string n-runtime) ", ") log-file)
    (printf "Number of never-firing denies overlapped by permits: ~a ~nTime: ~a~n" (length overlapped) n-runtime)))


; ********************************************************
; Detecting superfluous rules
; ********************************************************


(define (run-timed-script pFileName)
  ; Start the Java process
;  (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g") '("-log"))   ;; remove log for benchmarking
    (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g") '())   ;; remove log for benchmarking
      
  ; Start the timer
  (time-since-last)
  
  ; Load the policy
  (let* ([log-file (open-output-file "sup-benchmarking.csv" #:exists 'append)]
         [polname (load-policy pFileName)]
         [n-load-time (time-since-last)])
        
    
    
    (printf "Loading took: ~a milliseconds.~n" n-load-time)
    (write-string (string-append (number->string n-load-time) ", ") log-file)
    
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
      
      (let* ([neverApplyList (cleanup-idb-list-no-applies (xml-set-response->list (mtext (string-append "SHOW UNREALIZED " neverApplyId " " idblistapplied ))))]
             [n-sup-time (time-since-last)]
             [prnt (printf "superfluous-rule finder took: ~a milliseconds.~n" n-sup-time)]
             ;[dbg (printf "~a~n" neverApplyList)]
             [idblistpa (makeIdbList all-permit-applied)]
             [idblistdn-sup (makeIdbList (make-matches-list (list-intersection all-deny-rules neverApplyList) "InboundACL"))]
             [idblistda (makeIdbList all-deny-applied)]
             [idblistpn-sup (makeIdbList (make-matches-list (list-intersection all-permit-rules neverApplyList) "InboundACL"))]
             [n-list-time (time-since-last)])
        
        
        (printf "Creating list intersections took: ~a milliseconds.~n" n-list-time) 
               
        (write-string (string-append (number->string n-sup-time) ", ") log-file)
        (write-string (string-append (number->string n-list-time) ", ") log-file)
        
        (printf "superfluous rules count:~n~a~n" (length neverApplyList))
        ;(printf "~a ~n~n~n ~a ~n~n~n" idblistpn-sup idblistdn-sup)
        
        ;(printf "idblists: ~a~n~n~a~n~n" idblistdn-sup idblistpn-sup)
        
       
        
        ; Look for permits overlapping denies (and vice versa)
        (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup log-file)
        (find-overlaps-2 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup log-file)
        
        (write-string "\n" log-file)
        (close-output-port log-file)
        
        (stop-margrave-engine))))) ; close JVM and end function

(define (benchmark num-trials file-name)
  (when (> num-trials 0)   
    (printf " ~a trials left...~n" num-trials)
    (run-timed-script file-name)    
    (benchmark (- num-trials 1) file-name)))
