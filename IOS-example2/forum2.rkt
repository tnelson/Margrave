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

(require margrave/margrave
         margrave/margrave-ios)

; Packet arriving at BAZ router and BAZ's action on it
(define bazvectorfull-frombaz "(baz, baz-entry-interface, 
        baz-src-addr-in, baz-src-addr_, baz-src-addr-out, 
        baz-dest-addr-in, baz-dest-addr_, baz-dest-addr-out, 
        protocol, message, flags,
        baz-src-port-in, baz-src-port_, baz-src-port-out, 
        baz-dest-port-in, baz-dest-port_, baz-dest-port-out, 
        length, baz-next-hop, baz-exit-interface)")

; Packet arriving at TAS router
; (after passing through BAZ, which is why we use baz-src-addr-out,
; etc.) and TAS's action on it
(define tasvectorfull-frombaz "(tas, tas-entry-interface, 
        baz-src-addr-out, tas-src-addr_, tas-src-addr-out, 
        baz-dest-addr-out, tas-dest-addr_, tas-dest-addr-out, 
        protocol, message, flags,
        baz-src-port-out, tas-src-port_, tas-src-port-out, 
        baz-dest-port-out, tas-dest-port_, tas-dest-port-out, 
        length, tas-next-hop, tas-exit-interface)")


; -----------------------
; other direction

; Packet arriving at TAS router and TAS' action on it
(define tasvectorfull-fromtas "(tas, tas-entry-interface, 
        tas-src-addr-in, tas-src-addr_, tas-src-addr-out, 
        tas-dest-addr-in, tas-dest-addr_, tas-dest-addr-out, 
        protocol, message, flags,
        tas-src-port-in, tas-src-port_, tas-src-port-out, 
        tas-dest-port-in, tas-dest-port_, tas-dest-port-out, 
        length, tas-next-hop, tas-exit-interface)")
(define tasvectorpol-fromtas "(tas, tas-entry-interface, 
        tas-src-addr-in, tas-src-addr-out, 
        tas-dest-addr-in, tas-dest-addr-out, 
        protocol, message, flags,
        tas-src-port-in, tas-src-port-out, 
        tas-dest-port-in, tas-dest-port-out, 
        length, tas-next-hop, tas-exit-interface)")

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


(define routingpol-tas "(tas, tas-entry-interface, tas-src-addr_, tas-src-addr_,
  tas-dest-addr_, tas-dest-addr_, protocol, message, flags, tas-src-port_,
 tas-src-port_, tas-dest-port_, tas-dest-port_, length, tas-next-hop,
  tas-exit-interface)")


; NAT vectors, for testing

(define nat1-tas "(tas, tas-entry-interface, tas-src-addr-in, tas-src-addr_,
  tas-dest-addr-in, tas-dest-addr_, protocol, message, flags, tas-src-port-in, tas-src-port_,
  tas-dest-port-in, tas-dest-port_, length, tas-next-hop, tas-exit-interface)")
  
(define nat2-tas "(tas, tas-entry-interface, tas-src-addr_, tas-src-addr-out,
  tas-dest-addr_, tas-dest-addr-out, protocol, message, flags, tas-src-port_,
 tas-src-port-out, tas-dest-port_, tas-dest-port-out, length, tas-next-hop,
  tas-exit-interface)")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run-queries-for-forum-2)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  (start-margrave-engine #:margrave-params '( "-log" )) 
  
  (define log-file (open-output-file "forum2-benchmarking.csv" #:exists 'append))
  (time-since-last) ; reset
  
  ; Load all the policies 
  
  ; Original configuration
  (load-ios-policies (build-path (current-directory) "config") "" "1")
  
  (define n-load-one (time-since-last))
  (write-string (string-append (number->string n-load-one) ", ") log-file)
  
  ; Configuration with "default" keyword
  (load-ios-policies (build-path (current-directory) "revised") "" "2")
  
  (define n-load-two (time-since-last))
  (write-string (string-append (number->string n-load-two) ", ") log-file)
  
  ; Config with changed topology?
  (load-ios-policies (build-path (current-directory) "revised-address") "" "3")
  
  (define n-load-three (time-since-last))
  (write-string (string-append (number->string n-load-three) ", ") log-file)
  
  (write-string (string-append (number->string (+ n-load-one n-load-two n-load-three)) ", ") log-file)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------1--------------~n-------------Problem 1-------------~n")
  
  ; What is happening to packets from 10.232.0.0/22
  ; on the way to 10.232.100.0/22? Do they traverse TAS ok?
  ;   * ":" is a special character, but we can handle it by quoting
  ;   * the hostname parameter of internal-result dictates which router's
  ;     results are bound to the rest of the vector
  (display-response (mtext (string-append "EXPLORE
hostname-tas = tas AND
hostname-baz = baz AND

internal-result1" tasvectorfull-fromtas " AND
internal-result1" bazvectorfull-fromtas " AND
passes-firewall1" tasvectorpol-fromtas " AND
passes-firewall1" bazvectorpol-fromtas " AND

GigabitEthernet0/0 = tas-entry-interface AND
tas-src-addr-in IN 10.232.0.0/255.255.252.0 AND
tas-dest-addr-in IN 10.232.100.0/255.255.252.0 AND
\"Serial0/3/0:0\" = tas-exit-interface AND

\"Serial0/3/0:0\" = baz-entry-interface AND
baz-exit-interface = GigabitEthernet0/0 

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))    
  
  (printf "^^^ Expected false above~n")
  
  ; The packets aren't getting through. Ok, but we knew that already.
  ; Are they getting through just the TAS router? 
  
; Restricted version of prior query, just asking about TAS.
(display-response (mtext (string-append "EXPLORE
hostname-tas = tas AND

internal-result1" tasvectorfull-fromtas " AND
passes-firewall1" tasvectorpol-fromtas " AND

GigabitEthernet0/0 = tas-entry-interface AND
tas-src-addr-in IN 10.232.0.0/255.255.252.0 AND
tas-dest-addr-in IN 10.232.100.0/255.255.252.0 AND
\"Serial0/3/0:0\" = tas-exit-interface

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))  
  
  
  (printf "^^^ Expected false above~n")

  
  
  ; Still nothing. So the TAS router is either dropping the packets
  ; or routing them wrong. Which is it?
  
  (display-response (mtext (string-append "EXPLORE
hostname-tas = tas AND

internal-result1" tasvectorfull-fromtas " AND
NOT passes-firewall1" tasvectorpol-fromtas " AND

GigabitEthernet0/0 = tas-entry-interface AND
tas-src-addr-in IN 10.232.0.0/255.255.252.0 AND
tas-dest-addr-in IN 10.232.100.0/255.255.252.0 AND
tas-exit-interface = \"Serial0/3/0:0\" 

TUPLING")))
  (display-response (mtext "IS POSSIBLE?")) 
 
  (printf "^^^ Expected false above~n")

  
  
  ; Nothing. So the firewall isn't at fault. Must be routing?
  ; Which exit-interfaces can we be routing to?
    (display-response (mtext (string-append "EXPLORE
 hostname-tas = tas 
 AND internal-result1" tasvectorfull-fromtas
" AND passes-firewall1" tasvectorpol-fromtas
" AND GigabitEthernet0/0 = tas-entry-interface
 AND tas-src-addr-in IN 10.232.0.0/255.255.252.0
 AND tas-dest-addr-in IN 10.232.100.0/255.255.252.0  
        
TUPLING")))  
  (display-response (mtext "IS POSSIBLE?")) 
  
  
    (printf "^^^ Expected true above~n")

  
  ; There are scenarios. So we are on the right track!
  ; Find the interface names with show REALIZED:
  ; (again, the serial interface is quoted since ":" is a special character)
  ; no INCLUDE since Interface is "abstract" constrained, and the tupler 
  ; keeps all its immediate children by default.
  
      (display-response (mtext (string-append "EXPLORE
 hostname-tas = tas
 AND internal-result1" tasvectorfull-fromtas
" AND passes-firewall1" tasvectorpol-fromtas
" AND GigabitEthernet0/0 = tas-entry-interface
 AND tas-src-addr-in IN 10.232.0.0/255.255.252.0
 AND tas-dest-addr-in IN 10.232.100.0/255.255.252.0  

TUPLING")))  
  (display-response (mtext "SHOW REALIZED 
                           GigabitEthernet0/0 = tas-exit-interface,
                           \"Serial0/3/0:0\" = tas-exit-interface,
                           GigabitEthernet0/1 = tas-exit-interface")) 
  
  
    (printf "^^^ Expected GigabitEthernet0/0 above~n")

  
; All those packets are being sent out GigabitEthernet0/0 ---
; even the ones that are meant for 10.232.0.100/25. Surely
; something is wrong with the routing policy. What next-hop
; router is TAS trying to reach?
 
  
   ; "Is the traffic going to the correct next-hop address?"
  
  (display-response (mtext (string-append "EXPLORE
 hostname-tas =tas
 AND internal-result1" tasvectorfull-fromtas
" AND passes-firewall1" tasvectorpol-fromtas
" AND GigabitEthernet0/0 = tas-entry-interface
 AND tas-src-addr-in IN 10.232.0.0/255.255.252.0
 AND tas-dest-addr-in IN 10.232.100.0/255.255.252.0  

INCLUDE 
10.232.0.15 = tas-next-hop, 
10.232.4.10 = tas-next-hop,
tas-next-hop IN 10.254.1.128/255.255.255.252, 
tas-next-hop IN 10.232.8.0/255.255.252.0  


TUPLING")))  
  
; INCLUDE is telling the tupler to keep those EDB facts, even if they 
; don't appear in the query proper (we want to SHOW REALIZED for them!)

  ; (In THIS case, the tupler is smart enough to include them since they are
  ; mentioned directly by internal-result. However, including the INCLUDE
  ; as an example.)
  
    (display-response (mtext (string-append "SHOW REALIZED "
"10.232.0.15 = tas-next-hop," 
"10.232.4.10 = tas-next-hop,"
"tas-next-hop IN 10.232.8.0/255.255.252.0,"
"tas-next-hop IN 10.254.1.128/255.255.255.252")))

      (printf "^^^ Expected 10.232.0.15 above~n")

  
 
  
  
  
  ; ******************
  ; Relaxation: Does this happen to _all_ traffic from 10.232.0.0?
  ; (removed dest-addr-in restriction)
  ; Also need to rule out locally switched packets (packets for 
  ; 10.254.1.128/255.255.255.252 and 
  ; 10.232.8.0/255.255.252.0
  ; will be sent directly.
   ; 
  (display-response (mtext (string-append "EXPLORE
 hostname-tas=tas 
 AND internal-result1" tasvectorfull-fromtas
" AND passes-firewall1" tasvectorpol-fromtas
" AND GigabitEthernet0/0=tas-entry-interface
 AND tas-src-addr-in IN 10.232.0.0/255.255.252.0  
 AND NOT LocalSwitching1:Forward" routingpol-tas

" INCLUDE 
10.232.0.15 = tas-next-hop, 
10.232.4.10 = tas-next-hop,
tas-next-hop IN 10.254.1.128/255.255.255.252, 
tas-next-hop IN 10.232.8.0/255.255.252.0 

TUPLING")))    
    (display-response (mtext (string-append "SHOW REALIZED "
"10.232.0.15 = tas-next-hop," 
"10.232.4.10 = tas-next-hop,"
"tas-next-hop IN 10.232.8.0/255.255.252.0,"
"tas-next-hop IN 10.254.1.128/255.255.255.252")))
    
        (printf "^^^ Expected 10.232.0.15 above~n")

  
  ; So all traffic from 10.232.0.0/255.255.252.0 to 10.232.100.0/255.255.252.0
  ; is being sent via the internet gateway 10.232.0.15, and _NOT_ the BAZ router.

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (printf "~n---------------2--------------~n----------------------------~n")
  
  ; We just added "default". Does the original query (for the 2nd config) have models now?

  ; Check just TAS:   
(display-response (mtext (string-append "EXPLORE
hostname-tas=tas AND

internal-result2" tasvectorfull-fromtas " AND
passes-firewall2" tasvectorpol-fromtas " AND

GigabitEthernet0/0=tas-entry-interface AND
tas-src-addr-in IN 10.232.0.0/255.255.252.0 AND
tas-dest-addr-in IN 10.232.100.0/255.255.252.0 AND
\"Serial0/3/0:0\" =tas-exit-interface

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))  
  
  (printf "^^^ Expected true above~n")

  
  ; Check path across both TAS and BAZ:
  (display-response (mtext (string-append "EXPLORE
hostname-tas=tas AND
hostname-baz=baz AND

internal-result2" tasvectorfull-fromtas " AND
internal-result2" bazvectorfull-fromtas " AND
passes-firewall2" tasvectorpol-fromtas " AND
passes-firewall2" bazvectorpol-fromtas " AND

GigabitEthernet0/0=tas-entry-interface AND
tas-src-addr-in IN 10.232.0.0/255.255.252.0 AND
tas-dest-addr-in IN 10.232.100.0/255.255.252.0 AND
\"Serial0/3/0:0\"=tas-exit-interface AND

\"Serial0/3/0:0\"=baz-entry-interface AND
GigabitEthernet0/0=baz-exit-interface

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))
    
  (printf "^^^ Expected true above~n")
  ; Great! Both are satisfiable now. 
 
  
  
  
  
  ; Did we accidentally let the primary reach the secondary?
    (display-response (mtext (string-append "EXPLORE
hostname-tas=tas AND

internal-result2" tasvectorfull-fromtas " AND
passes-firewall2" tasvectorpol-fromtas " AND

GigabitEthernet0/0=tas-entry-interface AND
tas-src-addr-in IN 10.232.0.0/255.255.252.0 AND
(tas-dest-addr-in IN 10.232.4.0/255.255.252.0
   OR
 tas-dest-addr-in IN 10.232.104.0/255.255.252.0)
AND \"Serial0/3/0:0\"=tas-exit-interface

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))
  ; Unsatisfiable. Good.
  (printf "^^^ Expected false above~n")
  
  
  ; But what about the other problem?
  ; The secondary network 10.232.4.0/22 could access the internet. Did 
  ; we fix that too? This should involve only the TAS router:
      (display-response (mtext (string-append "EXPLORE
hostname-tas=tas AND

internal-result2" tasvectorfull-fromtas " AND
passes-firewall2" tasvectorpol-fromtas " AND

GigabitEthernet0/0=tas-entry-interface AND
tas-src-addr-in IN 10.232.4.0/255.255.252.0 AND

GigabitEthernet0/1=tas-exit-interface AND

NOT tas-dest-addr-in IN 10.232.4.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.232.104.0/255.255.252.0
AND NOT tas-dest-addr-in IN  10.232.0.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.232.100.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.254.1.128/255.255.255.252
AND NOT tas-dest-addr-in IN 192.168.1.0/255.255.255.0
AND NOT tas-dest-addr-in IN 10.232.8.0/255.255.252.0

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))
  
  ; Unsatisfiable!
  (printf "^^^ Expected false above~n")
  
  ; Which next-hop? Which exit-interfaces? (Relax!)
        (display-response (mtext (string-append "EXPLORE
hostname-tas=tas AND

internal-result2" tasvectorfull-fromtas " AND
passes-firewall2" tasvectorpol-fromtas " AND

GigabitEthernet0/0=tas-entry-interface AND
tas-src-addr-in IN 10.232.4.0/255.255.252.0 AND

NOT tas-dest-addr-in IN 10.232.4.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.232.104.0/255.255.252.0
AND NOT tas-dest-addr-in IN  10.232.0.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.232.100.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.254.1.128/255.255.255.252
AND NOT tas-dest-addr-in IN 192.168.1.0/255.255.255.0
AND NOT tas-dest-addr-in IN 10.232.8.0/255.255.252.0

TUPLING")))
  ; Can ask for both at once, here, since neither are constrained:
  (display-response (mtext "SHOW REALIZED  
                           GigabitEthernet0/0 = tas-exit-interface,
                           tas-exit-interface= \"Serial0/3/0:0\",
                           tas-exit-interface =GigabitEthernet0/1,
                           tas-next-hop = 10.232.0.15,
                           10.232.4.10 = tas-next-hop,
                           tas-next-hop IN 10.232.8.0/255.255.252.0,
                           tas-next-hop IN 10.254.1.128/255.255.255.252 "))
  
          (printf "^^^ Expected ge0/0 and 10.232.4.10 above~n")
  
  ; result:
  ;gigabitethernet0/0[tas-exit-interface]
  ;10.232.4.10[tas-next-hop]
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (printf "~n---------------3--------------~n----------------------------~n")
  
  ; This configuration has "default" and a new gateway address that is "local".
  ; Do both problems go away?
  
    ; Hopefully primary cannot reach the secondary:
    (display-response (mtext (string-append "EXPLORE
hostname-tas=tas AND

internal-result3" tasvectorfull-fromtas " AND
passes-firewall3" tasvectorpol-fromtas " AND

GigabitEthernet0/0=tas-entry-interface AND
tas-src-addr-in IN 10.232.0.0/255.255.252.0 AND
(tas-dest-addr-in IN 10.232.4.0/255.255.252.0
   OR
 tas-dest-addr-in IN 10.232.104.0/255.255.252.0)
AND \"Serial0/3/0:0\"=tas-exit-interface

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))
  ; Unsatisfiable. Good.
  
  (printf "^^^ Expected false above~n")
  
  (define n-so-far (time-since-last))
  
  ; Can the secondary network access the internet?
        (display-response (mtext (string-append "EXPLORE
hostname-tas=tas AND

internal-result3" tasvectorfull-fromtas " AND
passes-firewall3" tasvectorpol-fromtas " AND

GigabitEthernet0/0=tas-entry-interface AND
tas-src-addr-in IN 10.232.4.0/255.255.252.0 AND

GigabitEthernet0/1=tas-exit-interface AND

NOT tas-dest-addr-in IN 10.232.4.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.232.104.0/255.255.252.0
AND NOT tas-dest-addr-in IN  10.232.0.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.232.100.0/255.255.252.0
AND NOT tas-dest-addr-in IN 10.254.1.128/255.255.255.252
AND NOT tas-dest-addr-in IN 192.168.1.0/255.255.255.0
AND NOT tas-dest-addr-in IN 10.232.8.0/255.255.252.0

TUPLING")))
  (display-response (mtext "IS POSSIBLE?"))

  (define n-last-query (time-since-last))
  
  (printf "^^^ Expected true above~n")
  
  ; TRUE! So internet access has been restored (or at least some of it.)

  (write-string (string-append (number->string n-last-query) ", ") log-file)
  (write-string (string-append (number->string (+ n-so-far n-last-query)) "\n") log-file)
  (close-output-port log-file)
  
  (display-response (mtext "INFO"))
  
    ; comment this line out to make additional queries after the function runs
  (stop-margrave-engine)
  )


(define (benchmark num-trials)
  (when (> num-trials 0)   
    (printf " ~a trials left...~n" num-trials)
    (run-queries-for-forum-2)    
    (benchmark (- num-trials 1))))