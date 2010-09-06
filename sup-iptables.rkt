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


;; For use with iptables parser


; todo: string buffer in Racket (stream?) to avoid long query-creation time

; todo: too much string manipulation going on here. Is it possible to send back more structured IDB data?
;       for example, why not send a triple: (collectionname, idbname, tupling-data)?
; todo: similarly, why lists everywhere? Use sets for efficiency



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IMPORTANT
; This code is not yet complete, needs updating from the new sup-ios.rkt code.
; Will not run out-of-box.
; - TN
;
; IMPORTANT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require (file "margrave.rkt"))

; ********************************************************
; Helper functions
; ********************************************************

;(define (string-endswith str end)
;  (if (> (string-length end) (string-length str))
;      #f
;      (string=? end
;                (substring str
;                           (- (string-length str) (string-length end))
;                           (string-length str)))))

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

(define (make-applied-list rlist)
  (map (lambda (idbname)
         (string-append idbname "_applies"))
       rlist))

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

(define reqVector "chain, in-interface, src-addr, src-port, protocol, message, state, flag, dest-addr, dest-port, out-interface")

; *********************************************************************

; ********************************************************
; Detecting overlaps for shadowed rules
; ********************************************************


(define (find-overlaps-1 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)

  (let* ([denyOverlapPermitId 
          (xml-explore-result->id (mtext (string-append
                                          "EXPLORE Address(src-addr) AND Address(dest-addr) AND "
                                          " Port(dest-port) AND Port(src-port) AND "
                                          " Interface(in-interface) AND Interface(out-interface) and Chain(chain) AND "
                                          " Protocol(protocol) AND Message(message) AND State(state) AND Flag(flag) "
                                          "UNDER iptables-firewall "
                                          "INCLUDE " (string-append idblistda ", " idblistpn-sup)
                                          " TUPLING")))]                  
         [denyOverlapPermitGet (string-append "SHOW REALIZED " denyOverlapPermitId " " idblistda " FOR CASES " idblistpn-sup)])
    
    (define themap (xml-map-response->map (mtext denyOverlapPermitGet)))
    (define overlapped (keys-not-mapped-to-empty themap))
    
    (printf "Number of never-firing permits overlapped by denies: ~a ~nTime: ~a~n" (length overlapped) (time-since-last))))

(define (find-overlaps-2 neverApplyList idblistpa idblistda idblistpn-sup idblistdn-sup)
  (let* ([permitOverlapDenyId 
          (xml-explore-result->id (mtext (string-append 
                                          "EXPLORE Address(src-addr) AND Address(dest-addr) AND "
                                          " Port(dest-port) AND Port(src-port) AND "
                                          " Interface(in-interface) AND Interface(out-interface) and Chain(chain) AND "
                                          " Protocol(protocol) AND Message(message) AND State(state) AND Flag(flag) "
                                          "UNDER iptables-firewall "
                                          "INCLUDE " (string-append idblistpa ", " idblistdn-sup)
                                          " TUPLING")))]         
         
         [permitOverlapDenyGet (string-append "SHOW REALIZED " permitOverlapDenyId " " idblistpa " FOR CASES " idblistdn-sup)])
    
    (define themap (xml-map-response->map (mtext permitOverlapDenyGet)))
    (define overlapped (keys-not-mapped-to-empty themap))
    (printf "Number of never-firing denies overlapped by permits: ~a ~nTime: ~a~n" (length overlapped) (time-since-last))))


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
           [all-permit-rules (get-qualified-rule-list polname "accept")]
           [all-deny-rules (get-qualified-rule-list polname "drop")]
           [all-permit-applied (make-applied-list all-permit-rules)]
           [all-deny-applied (make-applied-list all-deny-rules)]
           
          ; [dbg (printf "~a ~n ~a~n" all-deny-rules all-permit-applied)]       

           [idblistrules (makeIdbList all-rules)]
           [idblistapplied (makeIdbList all-applied)]         
           
           [neverApplyId 
            (xml-explore-result->id  (mtext (string-append 
                                          "EXPLORE Address(src-addr) AND Address(dest-addr) AND "
                                          " Port(dest-port) AND Port(src-port) AND "
                                          " Interface(in-interface) AND Interface(out-interface) and Chain(chain) AND "
                                          " Protocol(protocol) AND Message(message) AND State(state) AND Flag(flag) "
                                             "UNDER iptables-firewall "
                                             "INCLUDE " idblistapplied
                                             " TUPLING")))])
      
      
      (printf "Time to make lists, strings, etc.: ~a milliseconds.~n" (time-since-last))
      (printf "There were ~a rules. ~a were ACCEPT rules; ~a were DROP (or REJECT) rules." (length all-rules) (length all-permit-rules) (length all-deny-rules))
      (printf "Running superfluous-rule finder...~n")
      
      ; **********************************************************************************************************
      
      (let* ([neverApplyList (cleanup-idb-list-no-applies-keep-collection (xml-set-response->list (mtext (string-append "SHOW UNREALIZED " neverApplyId " " idblistapplied ))))]
             [prnt (printf "superfluous-rule finder took: ~a milliseconds.~n" (time-since-last))]
            ; [prnt2 (printf "superfluous rules: ~a~n"  neverApplyList)]
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
; why 270 instead of 274?