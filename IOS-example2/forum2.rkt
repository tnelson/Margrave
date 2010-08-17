#lang racket

(require (file "../margrave.rkt")
         (file "../margrave-ios.rkt"))

; Packet arriving at BAZ router and BAZ's action on it
(define bazvectorfull-frombaz "(baz, baz-entry-interface, 
        baz-src-addr-in, baz-src-addr_, baz-src-addr-out, 
        baz-dest-addr-in, baz-dest-addr_, baz-dest-addr-out, 
        protocol, message,
        baz-src-port-in, baz-src-port_, baz-src-port-out, 
        baz-dest-port-in, baz-dest-port_, baz-dest-port-out, 
        length, baz-next-hop, baz-exit-interface)")

; Packet arriving at TAS router
; (after passing through BAZ, which is why we use baz-src-addr-out,
; etc.) and TAS's action on it
(define tasvectorfull-frombaz "(tas, tas-entry-interface, 
        baz-src-addr-out, tas-src-addr_, tas-src-addr-out, 
        baz-dest-addr-out, tas-dest-addr_, tas-dest-addr-out, 
        protocol, message,
        baz-src-port-out, tas-src-port_, tas-src-port-out, 
        baz-dest-port-out, tas-dest-port_, tas-dest-port-out, 
        length, tas-next-hop, tas-exit-interface)")


; -----------------------
; other direction

; Packet arriving at TAS router and TAS' action on it
(define tasvectorfull-fromtas "(tas, tas-entry-interface, 
        tas-src-addr-out, tas-src-addr_, tas-src-addr-out, 
        tas-dest-addr-out, tas-dest-addr_, tas-dest-addr-out, 
        protocol, message, flags,
        tas-src-port-out, tas-src-port_, tas-src-port-out, 
        tas-dest-port-out, tas-dest-port_, tas-dest-port-out, 
        length, tas-next-hop, tas-exit-interface)")
(define tasvectorpol-fromtas "(tas, tas-entry-interface, 
        tas-src-addr-out, tas-src-addr-out, 
        tas-dest-addr-out, tas-dest-addr-out, 
        protocol, message, flags,
        tas-src-port-out, tas-src-port-out, 
        tas-dest-port-out, tas-dest-port-out, 
        length, tas-next-hop, tas-exit-interface)")



;ahostname, entry-interface, 
;src-addr-in, src-addr_, src-addr-out, 
;dest-addr-in, dest-addr_, dest-addr-out, 
;protocol, message, flags,
;src-port-in, src-port_, src-port-out, 
;dest-port-in, dest-port_, dest-port-out, 
;length, next-hop, exit-interface



; Packet arriving at BAZ router (after TAS this time)
; and BAZ's action on it
(define bazvectorfull-fromtas "(baz, baz-entry-interface, 
        tas-src-addr-out, baz-src-addr_, baz-src-addr-out, 
        tas-dest-addr-out, baz-dest-addr_, baz-dest-addr-out, 
        protocol, message, flags,
        tas-src-port-out, baz-src-port_, baz-src-port-out, 
        tas-dest-port-out, baz-dest-port_, baz-dest-port-out, 
        length, baz-next-hop, baz-exit-interface)")
(define bazvectorpol-fromtas "(baz, baz-entry-interface, 
        tas-src-addr-out, baz-src-addr-out, 
        tas-dest-addr-out, baz-dest-addr-out, 
        protocol, message, flags,
        tas-src-port-out, baz-src-port-out, 
        tas-dest-port-out, baz-dest-port-out, 
        length, baz-next-hop, baz-exit-interface)")

(define (run-queries-for-forum-2)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  (start-margrave-engine (build-path (current-directory) 'up))
  
  ; Load all the policies 
  (load-ios-policies (build-path (current-directory) "config") "" "1")
  ;;;;;;(load-ios-policies (build-path (current-directory) "revised") "" "2")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------1--------------~n----------------------------~n")
  
  ; What is happening to packets from 10.232.0.0/22
  ; on the way to 10.232.100.0/22? Do they traverse TAS ok?
  ;   * ":" is a special character, but we can handle it by quoting
  ;   * the hostname parameter of internal-result dictates which router's
  ;     results are bound to the rest of the vector
  (display-response (mtext (string-append "EXPLORE
hostname-tas(tas) AND
hostname-baz(baz) AND

internal-result1" tasvectorfull-fromtas " AND
internal-result1" bazvectorfull-fromtas " AND
firewall-passed1" tasvectorpol-fromtas " AND
firewall-passed1" bazvectorpol-fromtas " AND

GigabitEthernet0/0(tas-entry-interface) AND
ip-10-232-0-0/ip-255-255-252-0(tas-src-addr-in) AND
ip-10-232-100-0/ip-255-255-252-0(tas-dest-addr-in) AND
\"Serial0/3/0:0\"(tas-exit-interface) AND

\"Serial0/3/0:0\"(baz-entry-interface) AND
GigabitEthernet0/0(baz-exit-interface)

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))
    
  
  
  ; The packets aren't getting through. Ok, but we knew that already.
  ; Are they getting through just the TAS router? 
  
; Restricted version of prior query, just asking about TAS.
(display-response (mtext (string-append "EXPLORE
hostname-tas(tas) AND

internal-result1" tasvectorfull-fromtas " AND
firewall-passed1" tasvectorpol-fromtas " AND

GigabitEthernet0/0(tas-entry-interface) AND
ip-10-232-0-0/ip-255-255-252-0(tas-src-addr-in) AND
ip-10-232-100-0/ip-255-255-252-0(tas-dest-addr-in) AND
\"Serial0/3/0:0\"(tas-exit-interface)

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))  
  
  ;; ERROR: getting packets here. Did I miss a restriction?
  (display-response (mtext "GET ONE")) 
  (display-response (mtext "GET NEXT")) 
  
  
  
  ; Still nothing. So the TAS router is either dropping the packets
  ; or routing them wrong. Which is it?
  
  (display-response (mtext (string-append "EXPLORE
hostname-tas(tas) AND

internal-result1" tasvectorfull-fromtas " AND
NOT firewall-passed1" tasvectorpol-fromtas " AND

GigabitEthernet0/0(tas-entry-interface) AND
ip-10-232-0-0/ip-255-255-252-0(tas-src-addr-in) AND
ip-10-232-100-0/ip-255-255-252-0(tas-dest-addr-in) AND
\"Serial0/3/0:0\"(tas-exit-interface)

TUPLING")))
  (display-response (mtext "IS POSSIBLE?")) 
  
  
  
  ; Nothing. So the firewall isn't at fault. Must be routing?
  ; (remove exit-interface restriction)
  
    (display-response (mtext (string-append "EXPLORE
hostname-tas(tas) AND

internal-result1" tasvectorfull-fromtas " AND
firewall-passed1" tasvectorpol-fromtas " AND

GigabitEthernet0/0(tas-entry-interface) AND
ip-10-232-0-0/ip-255-255-252-0(tas-src-addr-in) AND
ip-10-232-100-0/ip-255-255-252-0(tas-dest-addr-in) 

TUPLING")))
  (display-response (mtext "IS POSSIBLE?")) 
  
#|  
  
  (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
routed-packets1" vector " AND " ; *1*
                 "NOT port-80(dest-port-in) AND
 NOT port-20(dest-port-in) AND
 NOT port-21(dest-port-in) AND
 NOT port-23(dest-port-in) AND
 NOT port-3389(dest-port-in)"
                 " TUPLING"))
  
  (mtext "IS POSSIBLE? 0")    
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 2
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------2--------------~n----------------------------~n")
  
  ; TODO: No "other" or "unreferenced" keyword makes us enumerate those 5 ports.
  
  (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
routed-packets2" vector " AND " ; *2*
                 "NOT port-80(dest-port-in) AND
 NOT port-20(dest-port-in) AND
 NOT port-21(dest-port-in) AND
 NOT port-23(dest-port-in) AND
 NOT port-3389(dest-port-in)"
                 " TUPLING"))
  
  (mtext "IS POSSIBLE? 0")
  
  
  
  |#
  
  ;(stop-margrave-engine)
  )