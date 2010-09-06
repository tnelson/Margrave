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

(require (file "../margrave.rkt")
         (file "../margrave-ios.rkt"))

;; Full request vector plus the intermediate NAT vars 
;; that are bound by internal-result.
(define vector "(ahostname, entry-interface, 
        src-addr-in, src-addr_, src-addr-out, 
        dest-addr-in, dest-addr_, dest-addr-out, 
        protocol, message, flags,
        src-port-in, src-port_, src-port-out, 
        dest-port-in, dest-port_, dest-port-out, 
        length, next-hop, exit-interface)")

;; Policy request vector (no intermediate NAT vars)
(define policyvector "(ahostname, entry-interface, 
        src-addr-in,  src-addr-out, 
        dest-addr-in,  dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)")

; Src-addr <---> dest-addr
; Src-port <---> dest-port
; entry-interface <---> exit-interface
; Used for reflexive ACL axiom

; Note that this check DOES NOT CONSIDER NAT!
; As the paper says, Margrave does not support general 
; dynamic NAT, only models the *outgoing* packets.
; 6.1 fixes the ephemeral port issue, which comes up before
; NAT has a chance to apply.

(define reversepolicyvector "(ahostname, exit-interface, 
        dest-addr-in,  dest-addr-out,         
        src-addr-in,  src-addr-out,  
        protocol, message, flags,
        dest-port-in, dest-port-out,         
        src-port-in,  src-port-out,         
        length, next-hop, entry-interface)")

;(define reversepolicyvector "(ahostname, exit-interface, 
;        dest-addr-out,  dest-addr-in,         
;        src-addr-out,  src-addr-in,  
;        protocol, message, flags,
;        dest-port-out, dest-port-in,         
;        src-port-out,  src-port-in,         
;       length, prior-next-hop, entry-interface)")



(define (run-queries-for-forum-1)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  (start-margrave-engine (build-path (current-directory) 'up) '() '( "-log" ))  
  
  (define log-file (open-output-file "forum1-benchmarking.csv" #:exists 'append))
  (time-since-last) ; reset
  
  ; Load all the policies 
  ; InboundACL -> InboundACL1, InboundACL2, InboundACL3 respectively.
  (load-ios-policies (build-path (current-directory) "config") "" "1")
  
  (define n-load-one (time-since-last))
  (write-string (string-append (number->string n-load-one) ", ") log-file)
  
  (load-ios-policies (build-path (current-directory) "config-revised") "" "2")
  
  (define n-load-two (time-since-last))
  (write-string (string-append (number->string n-load-two) ", ") log-file)
  
  (load-ios-policies (build-path (current-directory) "config-reflexive") "" "3")
  
  (define n-load-three (time-since-last))
  (write-string (string-append (number->string n-load-three) ", ") log-file)
  
  
  (display-response (mtext "INFO"))
  
  (write-string (string-append (number->string (+ n-load-one n-load-two n-load-three)) ", ") log-file)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------1--------------~n------------------------------~n")
  
  
  (display-response (mtext "EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0 = entry-interface AND
prot-TCP = protocol AND
port-80 = src-port-in AND
passes-firewall1" policyvector " AND
internal-result1" vector                  
" TUPLING"))
  
  (mtext "IS POSSIBLE?")
  (mtext "GET ONE")
  
  (display-response (mtext  "SHOW REALIZED port-80 = dest-port-in" 
                                          ", port-20=dest-port-in" 
                                          ", port-21=dest-port-in" 
                                          ", port-23 = dest-port-in" 
                                          ", port-3389=dest-port-in"))
  
  
  
  (printf "^^^ Expected 80, 21, 20, 23 above.~n")
  
  
  
  (display-response (mtext "EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0=entry-interface AND
prot-TCP=protocol AND
port-80=src-port-in AND
passes-firewall1" policyvector " AND
internal-result1" vector " AND
 NOT port-80=dest-port-in AND
 NOT port-20=dest-port-in AND
 NOT port-21=dest-port-in AND
 NOT port-23=dest-port-in AND
 NOT port-3389=dest-port-in"
 " TUPLING"))
  
  (display-response (mtext "IS POSSIBLE?"))   
  
  (printf "^^^ Expected false above.~n")
 
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 2
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------2--------------~n------------------------------~n")
  
  (display-response (mtext "EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0=entry-interface AND
prot-TCP=protocol AND
port-80=src-port-in AND
passes-firewall2" policyvector " AND
internal-result2" vector " AND
 NOT port-80=dest-port-in AND
 NOT port-20=dest-port-in AND
 NOT port-21=dest-port-in AND
 NOT port-23=dest-port-in AND
 NOT port-3389=dest-port-in"
                  " TUPLING"))
  
  (display-response (mtext "IS POSSIBLE?"))
  
    (printf "^^^ Expected true above.~n")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 3
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------3--------------~n------------------------------~n")
  
  
  ; Same as above, but assert that something in connection-returntcp must
  ; have had its source in the permit rule that populates the temporary list.    
  (display-response (mtext "EXPLORE
NOT src-addr-in IN 192.168.2.0/255.255.255.0 AND
FastEthernet0=entry-interface AND
prot-TCP=protocol AND
port-80=src-port-in AND
passes-firewall3" policyvector " AND
internal-result3" vector "AND
( Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in) 
  IMPLIES
  InboundACL3:Router-Vlan1-line29_applies" reversepolicyvector ") "
                                                                    
"AND NOT port-80=dest-port-in AND
 NOT port-20=dest-port-in AND
 NOT port-21=dest-port-in AND
 NOT port-23=dest-port-in AND
 NOT port-3389=dest-port-in"                                                                    
 " TUPLING"))
  
  (display-response (mtext "IS POSSIBLE?"))
  
  
    (printf "^^^ Expected true above.~n")
  
  (display-response (mtext "GET ONE"))
  
    (printf "^^^ Expected a model above.~n")
    
  (define n-so-far (time-since-last))
  
  
  ; Did any changes NOT involve the Connection predicate?
  
    (display-response (mtext "EXPLORE
( Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in) 
  IMPLIES
  InboundACL3:Router-Vlan1-line29_applies" reversepolicyvector ") AND "

"(InboundACL1:Permit" policyvector " AND NOT InboundACL3:Permit" policyvector ") OR "
"(InboundACL3:Permit" policyvector " AND NOT InboundACL1:Permit" policyvector ")"

" AND Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in) "
                                                                                                                                       
 " TUPLING"))
  
  (display-response (mtext "GET ONE"))  
  
  (define n-last-query (time-since-last))
  
  
  (write-string (string-append (number->string n-last-query) ", ") log-file)
  (write-string (string-append (number->string (+ n-so-far n-last-query)) "\n") log-file)
  
  (display-response (mtext "INFO"))
  
  (close-output-port log-file)  
 ; (stop-margrave-engine)
  )

(define (benchmark num-trials)
  (when (> num-trials 0)   
    (printf " ~a trials left...~n" num-trials)
    (run-queries-for-forum-1)    
    (benchmark (- num-trials 1))))