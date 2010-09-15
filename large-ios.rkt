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

(require "margrave.rkt"
         "margrave-ios.rkt")
         
(define reqpol "(hostname, entry-interface, src-addr-in, src-addr-out,
                 dest-addr-in, dest-addr-out, protocol, message, flags,
                 src-port-in, src-port-out, dest-port-in, dest-port-out,
                 length, next-hop, exit-interface)")

(define reqfull1 "(hostname, entry-interface, src-addr-in, src-addr_-1, src-addr-out-1,
                  dest-addr-in, dest-addr_-1, dest-addr-out-1, protocol, message, flags,
                  src-port-in, src-port_-1, src-port-out-1, dest-port-in, dest-port_-1,
                  dest-port-out-1, length, next-hop-1, exit-interface-1)")

(define reqfull2 "(hostname, entry-interface, src-addr-in, src-addr_-2, src-addr-out-2,
                  dest-addr-in, dest-addr_-2, dest-addr-out-2, protocol, message, flags,
                  src-port-in, src-port_-2, src-port-out-2, dest-port-in, dest-port_-2,
                  dest-port-out-2, length, next-hop-2, exit-interface-2)")



;(define (run-timed-script pFileName1 pFileName2 target-interface target-src-address)
  
(define (run-timed-script pFileName1 pFileName2)

  ; Start the Java process
  (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g")  ) 
   ;(start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g") '("-log") ) 
  
  ; Print out baseline memory consumption (before policies loaded)
;  (display-response (mtext "INFO"))
  
  (define log-file (open-output-file "enterprise-acl-benchmarking.csv" #:exists 'append))

  ; Start the timer
  (time-since-last)

  ; load the policy. last parameter is verbosity (include timing)
  ;(load-ios-policies (build-path pDirectoryName "initial") "" "1" #t)  
  (load-policy pFileName1)
  (display-response (mtext "RENAME InboundACL InboundACL1"))
    
  (printf "1~n")
  (display-response (mtext "INFO"))
  
  (define n-load-one (time-since-last))
  (write-string (string-append (number->string n-load-one) ", ") log-file)
  
  (printf "loading Time: ~a~n " n-load-one)
  
  ;(load-ios-policies (build-path pDirectoryName "changes") "" "2" #t)  
  (load-policy pFileName2)
  (display-response (mtext "RENAME InboundACL InboundACL2"))

  (printf "2~n")
  (display-response (mtext "INFO"))
  
  (printf "~nLoading complete.~n~n")
  
  (time-since-last) ; reset timer
   
  
;  
;  
;  ; ------------------------------------------------------------------
;  ;   Which-packets #1: Can a certain address ever be permitted?
;  ;   Identity of server is passed as param in order to preserve privacy
;  ;   of the network.
;  ;   second version of the policy was changed to permit this blacklisted
;  ;   addr through
;  ; ------------------------------------------------------------------
; 
;  (printf "Which-packets #1~n")
;  (display-response (mtext "EXPLORE InboundACL2:permit" reqpol " AND "
;                           ;" entry-interface = " target-interface
;                           target-interface "(entry-interface) AND "
;                           ;" AND src-addr-in = " target-src-address
;                           target-src-address "(src-addr-in) "
;                           " TUPLING"))
;  ;(if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
;  ;    (begin
;  ;      (printf "-------------------------------------------------~n")
;  ;      (printf "It is indeed possible:~n")
;  ;      (printf "-------------------------------------------------~n")
;        (display-response (mtext "GET ONE"))   ;;)
;  ;    (printf "No, the query was impossible.~n~n"))
;
;  (define n-which-1 (time-since-last))
;  (write-string (string-append (number->string n-which-1) ", ") log-file)  
;  (printf "Time: ~a~n " n-which-1)
;  
;  
;  
;  ; ------------------------------------------------------------------
;  ;   Which-packets #1: same, with rule-blaming
;  ; ------------------------------------------------------------------
; 
;  ; assemble query (no "all rule applies" keyword in include yet)
  (define all-rules-1 (get-rule-list "InboundACL1"))
  (define all-applied-1 (make-applies-list all-rules-1 "InboundACL1"))
  (define all-rules-2 (get-rule-list "InboundACL2"))
  (define all-applied-2 (make-applies-list all-rules-2 "InboundACL2"))
  (define idblistapplied-1 (makeIdbList all-applied-1))
  (define idblistapplied-2 (makeIdbList all-applied-2))
  (define all-matches-1 (make-matches-list all-rules-1 "InboundACL1"))
  (define all-matches-2 (make-matches-list all-rules-2 "InboundACL2"))
  (define idblistmatches-1 (makeIdbList all-matches-1))
  (define idblistmatches-2 (makeIdbList all-matches-2))
;
;  (define n-processing (time-since-last))
;  (write-string (string-append (number->string n-processing) ", ") log-file)  
;  (printf "Time to construct strings: ~a~n " n-processing)
;  
;  (printf "Which-packets #2~n")
;  (display-response (mtext "EXPLORE InboundACL2:permit" reqpol " AND "
;                           ;" entry-interface = " target-interface
;                           target-interface "(entry-interface) AND "
;                           ;" AND src-addr-in = " target-src-address
;                           target-src-address "(src-addr-in) "
;                           " UNDER InboundACL1 "
;                           " INCLUDE " idblistapplied-1 ", " idblistapplied-2       
;                           " TUPLING"))
;  ;(if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
;  ;    (begin
;  ;      (printf "-------------------------------------------------~n")
;  ;      (printf "It is indeed possible:~n")
;  ;      (printf "-------------------------------------------------~n")
;        (display-response (mtext "GET ONE"))   ;)
;  ;    (printf "No, the query was impossible.~n~n"))
;
;  (define n-which-2 (time-since-last))
;  (write-string (string-append (number->string n-which-2) ", ") log-file)  
;  (printf "Time: ~a~n " n-which-2)
;  
;  
;  
;    (printf "3~n")
;  (display-response (mtext "INFO"))

  
  
  ; ------------------------------------------------------------------
  ;   Change-impact #1: InboundACL vs. InboundACL
  ; ------------------------------------------------------------------
 
  ; Query does not take advantage of binary nature of the policy; could
  ; technically check only permit vs. !permit (half size of this)
  (printf "Change-impact #1: InboundACL vs. InboundACL~n")
  (display-response (mtext "EXPLORE (InboundACL1:permit" reqpol " AND NOT InboundACL2:permit" reqpol ")"
                           " OR (InboundACL2:permit" reqpol " AND NOT InboundACL1:permit" reqpol ")"
                           " OR (InboundACL1:deny" reqpol " AND NOT InboundACL2:deny" reqpol ")"
                           " OR (InboundACL2:deny" reqpol " AND NOT InboundACL1:deny" reqpol ")"                           
                           " TUPLING"))
 ; (if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
 ;     (begin
 ;       (printf "-------------------------------------------------~n")
 ;       (printf "Found semantic differences. First scenario found:~n")
 ;       (printf "-------------------------------------------------~n")
        (display-response (mtext "GET ONE"))   ;)
    ;  (printf "No semantic differences found.~n~n"))

  (define n-ch-imp-1 (time-since-last))
  (write-string (string-append (number->string n-ch-imp-1) ", ") log-file)

  
  (printf "Time: ~a~n " n-ch-imp-1)
  
  
  
  
  ; ------------------------------------------------------------------
  ;   Change-impact #2: InboundACL vs. InboundACL with rule blaming
  ; ------------------------------------------------------------------
    
  
  
  (time-since-last)
  
  (printf "Change-impact #1: InboundACL vs. InboundACL (w/ rule blaming)~n")
  (display-response (mtext "EXPLORE ((InboundACL1:permit" reqpol " AND NOT InboundACL2:permit" reqpol ")"
                           " OR (InboundACL2:permit" reqpol " AND NOT InboundACL1:permit" reqpol ")"
                           " OR (InboundACL1:deny" reqpol " AND NOT InboundACL2:deny" reqpol ")"
                           " OR (InboundACL2:deny" reqpol " AND NOT InboundACL1:deny" reqpol "))" 
                           
                           " INCLUDE " idblistapplied-1 ", " idblistapplied-2                         
                           " TUPLING"))
 ; (if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
  ;    (begin
    ;    (printf "-------------------------------------------------~n")
    ;    (printf "Found semantic differences. First scenario found:~n")
    ;    (printf "-------------------------------------------------~n")
        (display-response (mtext "GET ONE"))   ;)
     ; (printf "No semantic differences found.~n~n"))
  
  
  (define n-ch-imp-2 (time-since-last))
  (write-string (string-append (number->string n-ch-imp-2) "\n") log-file)
  
  (printf "Time: ~a~n " n-ch-imp-2)
 
  
  #|
  
   ; ------------------------------------------------------------------
  ;   Change-impact #3: InboundACL vs. InboundACL, matches rule blaming
  ;  (fast, less obvious)
  ; ------------------------------------------------------------------
  
  ; assemble query (no "all rule applies" keyword in include yet)
  
  (time-since-last)
  
  (printf "Change-impact #1: InboundACL vs. InboundACL (w/ rule blaming)~n")
  (display-response (mtext "EXPLORE ((InboundACL1:permit" reqpol " AND NOT InboundACL2:permit" reqpol ")"
                           " OR (InboundACL2:permit" reqpol " AND NOT InboundACL1:permit" reqpol ")"
                           " OR (InboundACL1:deny" reqpol " AND NOT InboundACL2:deny" reqpol ")"
                           " OR (InboundACL2:deny" reqpol " AND NOT InboundACL1:deny" reqpol "))" 
                           
                           " INCLUDE " idblistmatches-1 ", " idblistmatches-2                         
                           " TUPLING"))
;  (if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
;      (begin
;        (printf "-------------------------------------------------~n")
;        (printf "Found semantic differences. First scenario found:~n")
;        (printf "-------------------------------------------------~n")
        (display-response (mtext "GET ONE")) ;)
;      (printf "No semantic differences found.~n~n"))
  
  
  (define n-ch-imp-3 (time-since-last))
  (write-string (string-append (number->string n-ch-imp-3) ", ") log-file)
  
  (printf "Time: ~a~n " n-ch-imp-3)
  
  |#
  #|
  ; ------------------------------------------------------------------
  ;   entire firewall config vs. entire new config
  ; ------------------------------------------------------------------

  (printf "Change-impact #3: Entire config comparison~n")
  (display-response (mtext "EXPLORE internal-result1" reqfull1 " AND internal-result2" reqfull2 " AND"
                           
                           ;; Did the routing change?
                           ;; Did the packet's fields change?
                           
                           "( NOT (exit-interface-1 = exit-interface-2) OR "
                           
                           
                           " NOT (next-hop-1 = next-hop-2) OR "
                           
                           
                           
                           ;; TN ==================
                           ;; Deal with this issue: NAT, scenarios. Use translate?
                           
                           
                           " NOT (src-addr-out-1 = src-addr-out-2) OR "
                           " NOT (dest-addr-out-1 = dest-addr-out-2) OR "
                           " NOT (src-port-out-1 = src-port-out-2)  OR "
                           " NOT (dest-port-out-1 = dest-port-out-2) OR "
                           
                           ;; Did the firewall's ACL act differently?
                           
                           " (InboundACL1:permit" reqpol " AND NOT InboundACL2:permit" reqpol ") OR "
                           " (InboundACL2:permit" reqpol " AND NOT InboundACL1:permit" reqpol ") OR "
                           " (InboundACL1:deny" reqpol " AND NOT InboundACL2:deny" reqpol ") OR "
                           " (InboundACL2:deny" reqpol " AND NOT InboundACL1:deny" reqpol ") )"
                           " TUPLING"))
  (if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
      (begin
        (printf "-------------------------------------------------~n")
        (printf "Found semantic differences. First scenario found:~n")
        (printf "-------------------------------------------------~n")
        (display-response (mtext "GET ONE")))
      (printf "No semantic differences found.~n~n"))
  
    (printf "Time: ~a~n " (time-since-last))
  
  |#
  
    (printf "4~n")
  (display-response (mtext "INFO"))

  
    (close-output-port log-file)  
  
  (stop-margrave-engine)
  )



(define (makeIdbList lst)
  (if (equal? '() lst)
      ""
      (let ([ idbname (first lst)])
        (string-append idbname         
                       reqpol
                       (if (equal? '() (rest lst))
                           ""
                           ", ")
                       (makeIdbList (rest lst))))))

(define (benchmark num-trials file-name1 file-name2 target-interface target-src-address)
  (when (> num-trials 0)   
    (printf " ~a trials left...~n" num-trials)
    (run-timed-script file-name1 file-name2 target-interface target-src-address)    
    (benchmark (- num-trials 1) file-name1 file-name2 target-interface target-src-address)))