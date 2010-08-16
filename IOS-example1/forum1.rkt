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
(define reversepolicyvector "(ahostname, exit-interface, 
        dest-addr-in,  dest-addr-out,         
        src-addr-in,  src-addr-out,  
        protocol, message, flags,
        dest-port-in, dest-port-out,         
        src-port-in,  src-port-out,         
        length, next-hop, entry-interface)")

(define (run-queries-for-forum-1)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  (start-margrave-engine (build-path (current-directory) 'up))  
  
  ; Load all the policies 
  ; InboundACL -> InboundACL1, InboundACL2, InboundACL3 respectively.
  (load-ios-policies (build-path (current-directory) "config") "" "1")
  (load-ios-policies (build-path (current-directory) "config-revised") "" "2")
  (load-ios-policies (build-path (current-directory) "config-reflexive") "" "3")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 1
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------1--------------~n------------------------------~n")
  
  
  (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
firewall-passed1" policyvector " AND
internal-result1" vector                  
" TUPLING") #t)
  
  (mtext "IS POSSIBLE?" #t)
  (mtext "GET ONE" #t)
  
  (display-response (mtext (string-append "SHOW POPULATED port-80(dest-port-in)" 
                                          ", port-20(dest-port-in)" 
                                          ", port-21(dest-port-in)" 
                                          ", port-23(dest-port-in)" 
                                          ", port-3389(dest-port-in)")))
  
  
  
  
  
  (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
firewall-passed1" policyvector " AND
internal-result1" vector " AND
 NOT port-80(dest-port-in) AND
 NOT port-20(dest-port-in) AND
 NOT port-21(dest-port-in) AND
 NOT port-23(dest-port-in) AND
 NOT port-3389(dest-port-in)"
 " TUPLING"))
  
  (display-response (mtext "IS POSSIBLE?"))   
  
  (display-response (mtext "GET ONE"))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 2
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------2--------------~n------------------------------~n")
  
  (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
firewall-passed2" policyvector " AND
internal-result2" vector " AND
 NOT port-80(dest-port-in) AND
 NOT port-20(dest-port-in) AND
 NOT port-21(dest-port-in) AND
 NOT port-23(dest-port-in) AND
 NOT port-3389(dest-port-in)"
                  " TUPLING"))
  
  (display-response (mtext "IS POSSIBLE?"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Version 3
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n---------------3--------------~n------------------------------~n")
  
  
  ; Same as above, but assert that something in connection-returntcp must
  ; have had its source in the permit rule that populates the temporary list.    
  (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
firewall-passed3" policyvector " AND
internal-result3" vector "AND
( Connection-returntcp(src-addr-in, src-port-in, protocol, dest-addr-in, dest-port-in) 
  IMPLIES
  InboundACL3:ACE-line-30-g20001_applies" reversepolicyvector ") "
                                                                    
"AND NOT port-80(dest-port-in) AND
 NOT port-20(dest-port-in) AND
 NOT port-21(dest-port-in) AND
 NOT port-23(dest-port-in) AND
 NOT port-3389(dest-port-in)"                                                                    
 " TUPLING") #t)
  
  (display-response (mtext "IS POSSIBLE?"))
  (display-response (mtext "GET ONE"))
  
  
  
  ;(stop-margrave-engine)
  )