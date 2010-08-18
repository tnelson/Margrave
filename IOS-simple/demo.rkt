#lang racket

(require (file "../margrave.rkt")
         (file "../margrave-ios.rkt"))

; IMPORTANT! --->
; Things that need bugfixing for the paper are marked with TODO.


(define vector "(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)")


(define (run-queries-for-example)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  (start-margrave-engine (build-path (current-directory) 'up))
  
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

        AND 10.1.1.2(src-addr-in)
        AND fe0(entry-interface)

     TUPLING") )
  (display-response (mtext "IS POSSIBLE?"))
  
  
  
  ;; due to gensym use, rule names will change along with line numbers and on each re-parse
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; rule responsibility
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nRule-blaming:~n")
  
  (display-response (mtext (string-append "EXPLORE InboundACL1:Deny" vector
                        
                        " AND 10.1.1.2(src-addr-in)"
                        " AND fe0(entry-interface) "
                        
                        " INCLUDE InboundACL1:ACE-line-10-g5468_applies" vector ","
                        "InboundACL1:ACE-line-13-g5471_applies" vector
                        " TUPLING")))
  (display-response (mtext (string-append "SHOW POPULATED 0 InboundACL1:ACE-line-10-g5468_applies" vector ","
                        "InboundACL1:ACE-line-13-g5471_applies" vector)))
  
  
  ; TODO: Why is the line number off by 1? (Do we use _next_ char's line number? If so, need to subtract 1 in IOS parser)
  
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
  (display-response (mtext "IS POSSIBLE? 0"))
  
  
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
 (display-response  (mtext "IS POSSIBLE? 0"))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Rule relationships
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (printf "~n~nRule relationships:~n")
  
  ;; This involves rules in the first change (InboundACL2)
  ; line 13 wants to apply: what prevents it from doing so?
  
  (display-response (mtext (string-append "EXPLORE InboundACL2:ACE-line-13-g5484" vector
                        
                        " INCLUDE InboundACL2:ACE-line-10-g5481_applies" vector ","
                        "InboundACL2:ACE-line-11-g5482_applies" vector ","
                        "InboundACL2:ACE-line-12-g5483_applies" vector
                        " TUPLING"))) 
  (display-response (mtext (string-append "SHOW POPULATED 0 InboundACL2:ACE-line-10-g5481_applies" vector ","
                        "InboundACL2:ACE-line-11-g5482_applies" vector ","
                        "InboundACL2:ACE-line-12-g5483_applies" vector)))
  
  
  
  
  
  
  ; Computing superfluous rules
  ; ----> In sup-ios.rkt
  
  
  
  
  
  ;(stop-margrave-engine)
  )