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

; Vectors for the queries in this file

(define reqfull-1 "(fw1, fw1-entry-interface, 
        fw1-src-addr-in, fw1-src-addr_, fw1-src-addr-out, 
        fw1-dest-addr-in, fw1-dest-addr_, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-in, fw1-src-port_, fw1-src-port-out, 
        fw1-dest-port-in, fw1-dest-port_, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface)")

(define reqfull-2 "(fw2, fw2-entry-interface, 
        fw1-src-addr-out, fw2-src-addr_, fw2-src-addr-out, 
        fw1-dest-addr-out, fw2-dest-addr_, fw2-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw2-src-port_, fw2-src-port-out, 
        fw1-dest-port-out, fw2-dest-port_, fw2-dest-port-out, 
        length, fw2-next-hop, fw2-exit-interface)")

(define reqpol-1 "(fw1, fw1-entry-interface, 
        fw1-src-addr-in, fw1-src-addr-out, 
        fw1-dest-addr-in, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-in, fw1-src-port-out, 
        fw1-dest-port-in, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface)")

(define reqpol-2 "(fw2, fw2-entry-interface, 
        fw1-src-addr-out, fw2-src-addr-out, 
        fw1-dest-addr-out, fw2-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw2-src-port-out, 
        fw1-dest-port-out, fw2-dest-port-out, 
        length, fw2-next-hop, fw2-exit-interface)")


; for testing

(define nat1 "(fw1, fw1-entry-interface, 
        fw1-src-addr-in, fw1-src-addr-out, 
        fw1-dest-addr-in, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-in, fw1-src-port-out, 
        fw1-dest-port-in, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface)")

(define nat2 "(fw1, fw1-entry-interface, 
        fw1-src-addr-out, fw1-src-addr-out, 
        fw1-dest-addr-out, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw1-src-port-out, 
        fw1-dest-port-out, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface)")

(define route1 "(fw1, fw1-entry-interface, 
        fw1-src-addr-out, fw1-src-addr-out, 
        fw1-dest-addr-out, fw1-dest-addr-out, 
        protocol, message, flags,
        fw1-src-port-out, fw1-src-port-out, 
        fw1-dest-port-out, fw1-dest-port-out, 
        length, fw1-next-hop, fw1-exit-interface)")

(define (run-queries-for-example)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  ; no JVM options, but turn on Margrave's logging
  (start-margrave-engine)
  
  ; Load all the policies 
  (load-ios-policies (build-path (current-directory) "network") "" "")  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nSample multi-firewall query:~n")
  
  ; Remember: AND binds tighter than OR, so wrap the OR in parens.
  ; 10.200.0.0/255.255.0.0 is "the internet" for this example: something outside the extern router
  
  
    (display-response (mtext (string-append "EXPLORE prot-TCP = protocol AND
192.168.1.2 = fw1-src-addr-in  AND
in_lan = fw1-entry-interface AND
out_dmz = fw2-entry-interface AND
hostname-int = fw1 AND
hostname-ext = fw2 AND

fw1-dest-addr-in IN 10.200.0.0/255.255.0.0 AND
NOT 10.200.200.200 = fw1-dest-addr-in AND
port-80 = fw1-dest-port-in AND

internal-result" reqfull-1 " AND

( NOT passes-firewall" reqpol-1 " OR

internal-result" reqfull-2 " AND
NOT passes-firewall" reqpol-2 ")

UNDER InboundACL
INCLUDE
InboundACL:int-in_lan-line12_applies" reqpol-1 ",
InboundACL:int-in_lan-line15_applies" reqpol-1 ",
InboundACL:ext-out_dmz-line17_applies" reqpol-2 ",
InboundACL:ext-out_dmz-line18_applies" reqpol-2 ",
InboundACL:ext-out_dmz-line20_applies" reqpol-2 "

TUPLING"))) 

  
  
  (display-response (mtext "GET ONE"))  
  
  (display-response (mtext "SHOW REALIZED 
InboundACL:int-in_lan-line12_applies" reqpol-1 ",
InboundACL:int-in_lan-line15_applies" reqpol-1 ",
InboundACL:ext-out_dmz-line17_applies" reqpol-2 ",
InboundACL:ext-out_dmz-line18_applies" reqpol-2 ",
InboundACL:ext-out_dmz-line20_applies" reqpol-2))
  
  
  
  ;(stop-margrave-engine)
  )