; Copyright © 2009-2012 Brown University and Worcester Polytechnic Institute.
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

(require "../../../racket/margrave.rkt"
         "../../../racket/margrave-ios.rkt"
         rackunit)

(define (run-queries-for-example)
  
  ; Start Margrave's java engine  
  (start-margrave-engine #:margrave-params '("-log")
                         #:margrave-path "../../../racket")
  
  ; Load all the policies 
  ; InboundACL -> InboundACL1, InboundACL2, InboundACL3 respectively.
  (parse-and-load-ios "demo.txt" "initial" "" "1")
  (parse-and-load-ios "change1.txt" "change1" "" "2")
  (parse-and-load-ios "change2.txt" "change2" "" "3")

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; which-packets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
 ; (printf "~n~nWhich-packets:~n")
  
  ;        AND fe0(entry-interface)
  ;    AND prot-tcp(protocol)
  ;    AND ip-192-168-5-10(dest-addr-in)
  ;    AND ip-10-1-1-2(ip-addr-in)
  
  
    (define vardec-16 '([ahostname Hostname]
                      [entry-interface Interf-real]
                      [src-addr-in IPAddress]
                      [src-addr-out IPAddress]
                      [dest-addr-in IPAddress]
                      [dest-addr-out IPAddress]
                      [protocol Protocol-any]
                      [message ICMPMessage]
                      [flags TCPFlags]
                      [src-port-in Port]
                      [src-port-out Port]
                      [dest-port-in Port]
                      [dest-port-out Port] 
                      [length Length]
                      [next-hop IPAddress]
                      [exit-interface Interface]))
  (define vars-16 '(ahostname entry-interface src-addr-in src-addr-out
                      dest-addr-in dest-addr-out
                      protocol message flags
                      src-port-in src-port-out dest-port-in dest-port-out 
                      length next-hop exit-interface))

  (m-let "Q1" vardec-16
         `([InboundACL1 permit] ,@vars-16))
  
  (check-true (m-scenario? (m-get-scenario "Q1")))
  ;(display (m-show-scenario "Q1"))  
     
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; verification
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;(printf "~n~nVerification:~n")
  

        ;AND src-addr-in = 10.1.1.2 
        ;AND fe0 = entry-interface"

  (m-let "Q2" vardec-16
         `(and ([InboundACL1 permit] ,@vars-16)
               (IP-10.1.1.2 src-addr-in)
               (Fe0 entry-interface)))
  
  ;(printf "Is there a counterexample to the property? ~v~n" (m-is-poss? "Q2"))
  (check-false (m-is-poss? "Q2"))
  
  
 
  
  
  ;; due to gensym use, rule names will change along with line numbers and on each re-parse
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; rule responsibility
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;  (printf "~n~nRule-blaming:~n")
  
  ; Why are those packets denied?
  (m-let "Q3" vardec-16
         `(and ([InboundACL1 deny] ,@vars-16)
               (IP-10.1.1.2 src-addr-in)
               (Fe0 entry-interface))
         #:debug 3)
 
  (m-show-realized "Q3" `( ([InboundACL1 Router-Fe0-line9_applies] ,@vars-16)
                           ([InboundACL1 Router-Fe0-line12_applies] ,@vars-16)) 
                   empty)
  
  ; arity too large for a universe of size 24. 
  ; kodkod.engine.CapacityExceededException
  ; edu.wpi.margrave.MQueryResult.makeBounds(MQueryResult.java:673), edu.wpi.margrave.MCNFSpyQueryResult.createCNFFor(MQueryResult.java:99)
  
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ; change-impact
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  
;  (printf "~n~nChange-impact:~n")
;  
;  ; vs change 1    
;  (display-response (mtext "EXPLORE (InboundACL1:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface)
;
;       AND NOT InboundACL2:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface) )
;
;       OR
;       (InboundACL2:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface)
;
;       AND NOT InboundACL1:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface) )
;
;     TUPLING"))  
;  (display-response (mtext "IS POSSIBLE?"))
;  
;  
;  ; Vs. change 2
;  (display-response (mtext "EXPLORE (InboundACL1:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface)
;
;       AND NOT InboundACL3:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface) )
;
;       OR
;       (InboundACL3:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface)
;
;       AND NOT InboundACL1:Permit(ahostname, entry-interface, 
;        src-addr-in, src-addr-out, 
;        dest-addr-in, dest-addr-out, 
;        protocol, message, flags,
;        src-port-in,  src-port-out, 
;        dest-port-in, dest-port-out, 
;        length, next-hop, exit-interface) )
;
;     TUPLING")  )
; (display-response  (mtext "IS POSSIBLE?"))
;  
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  ; Rule relationships
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  
;  (printf "~n~nRule relationships:~n")
;  
;  ;; This involves rules in the first change (InboundACL2)
;  ; line 12 wants to apply: what prevents it from doing so?
;  
;  (display-response (mtext (string-append "EXPLORE InboundACL2:Router-fe0-line12_matches" my-vector
;                        
;                        " INCLUDE InboundACL2:Router-fe0-line9_applies" my-vector ","
;                        "InboundACL2:Router-fe0-line10_applies" my-vector ","
;                        "InboundACL2:Router-fe0-line11_applies" my-vector
;                        " TUPLING"))) 
;  (display-response (mtext (string-append "SHOW REALIZED InboundACL2:Router-fe0-line9_applies" my-vector ","
;                        "InboundACL2:Router-fe0-line10_applies" my-vector ","
;                        "InboundACL2:Router-fe0-line11_applies" my-vector)))
;  
;  
;  
;  
;  
;  
;  ; Computing superfluous rules
;  ; ----> In sup-ios.rkt
;  
;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  
;  ; 2.1 in other file
;  
;  
;  
  ;(stop-margrave-engine)
  )