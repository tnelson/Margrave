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

(require margrave
         margrave/margrave-ios)


(define my-vector "(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)")


(define (run-queries-for-example)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
;  (start-margrave-engine (build-path (current-directory) 'up))
  (start-margrave-engine)
  
  ; Load all the policies 
  ; InboundACL -> InboundACL1, InboundACL2, InboundACL3 respectively.
  (load-ios-policies (build-path (current-directory) "initial") "" "1")
  (load-ios-policies (build-path (current-directory) "change1") "" "2")
  (load-ios-policies (build-path (current-directory) "change2") "" "3")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; which-packets
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nWhich-packets:~n")
  
  ;        AND fe0(entry-interface)
  ;    AND prot-tcp(protocol)
  ;    AND ip-192-168-5-10(dest-addr-in)
  ;    AND ip-10-1-1-2(ip-addr-in)
  
  
  (display-response (mtext "EXPLORE InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)
     TUPLING")  )
  ; The TUPLING keyword activates the tupling optimization, which is very useful for firewalls.
  
  (display-response (mtext "GET ONE"))
     
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; verification
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nVerification:~n")
  
  (display-response (mtext "EXPLORE InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

        AND src-addr-in = 10.1.1.2 
        AND fe0 = entry-interface

     TUPLING") )
  (display-response (mtext "IS POSSIBLE?"))
  
  
  
  ;; due to gensym use, rule names will change along with line numbers and on each re-parse
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; rule responsibility
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nRule-blaming:~n")
  
  (display-response (mtext (string-append "EXPLORE InboundACL1:Deny" my-vector
                        
                        " AND 10.1.1.2 = src-addr-in"
                        " AND fe0 = entry-interface "
                        
                        " INCLUDE InboundACL1:Router-fe0-line9_applies" my-vector ","
                        "InboundACL1:Router-fe0-line12_applies" my-vector
                        " TUPLING")))
  (display-response (mtext (string-append "SHOW REALIZED InboundACL1:Router-fe0-line9_applies" my-vector ","
                        "InboundACL1:Router-fe0-line12_applies" my-vector)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; change-impact
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nChange-impact:~n")
  
  ; vs change 1    
  (display-response (mtext "EXPLORE (InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL2:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

       OR
       (InboundACL2:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

     TUPLING"))  
  (display-response (mtext "IS POSSIBLE?"))
  
  
  ; Vs. change 2
  (display-response (mtext "EXPLORE (InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL3:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

       OR
       (InboundACL3:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

     TUPLING")  )
 (display-response  (mtext "IS POSSIBLE?"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Rule relationships
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nRule relationships:~n")
  
  ;; This involves rules in the first change (InboundACL2)
  ; line 12 wants to apply: what prevents it from doing so?
  
  (display-response (mtext (string-append "EXPLORE InboundACL2:Router-fe0-line12_matches" my-vector
                        
                        " INCLUDE InboundACL2:Router-fe0-line9_applies" my-vector ","
                        "InboundACL2:Router-fe0-line10_applies" my-vector ","
                        "InboundACL2:Router-fe0-line11_applies" my-vector
                        " TUPLING"))) 
  (display-response (mtext (string-append "SHOW REALIZED InboundACL2:Router-fe0-line9_applies" my-vector ","
                        "InboundACL2:Router-fe0-line10_applies" my-vector ","
                        "InboundACL2:Router-fe0-line11_applies" my-vector)))
  
  
  
  
  
  
  ; Computing superfluous rules
  ; ----> In sup-ios.rkt
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; 2.1 in other file
  
  
  
  ;(stop-margrave-engine)
  )