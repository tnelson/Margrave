#lang racket

(require (file "../margrave.rkt")
         (file "../margrave-ios.rkt"))

; IMPORTANT! --->
; Things that need bugfixing for the paper are marked with TODO.


(define vector "(ahostname, entry-interface, 
        src-addr-in, src-addr_, src-addr-out, 
        dest-addr-in, dest-addr_, dest-addr-out, 
        protocol, message, flags,
        src-port-in, src-port_, src-port-out, 
        dest-port-in, dest-port_, dest-port-out, 
        length, next-hop, exit-interface)")

(define policyvector "(ahostname, entry-interface, 
        src-addr-in,  src-addr-out, 
        dest-addr-in,  dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)")


(define (run-queries-for-forum-1)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  (start-margrave-engine (build-path (current-directory) 'up))
  
  (with-handlers ((exn:fail? (lambda (v) 
                               (printf "Racket produced a failure exception.~n Stopping the Margrave engine.~n")
                               (stop-margrave-engine)
                               (raise v))))
    
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
   ; (mtext "GET ONE")
   ; (mtext "GET NEXT")
    
    ;; TODO shouldn't we have a keyword that's syntactic sugar for "all subsorts of..."? 
    ;; TODO no way to show populated "other"... without that, this isn't a very interesting show populated?
    ;;   (but abstractness is expensive. can we get "other" here without an explicit sort and abstractness?)
    ; !!! TODO tupling isn't shown in the paper
   
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
    
    ; TODO: No "other" or "unreferenced" keyword makes us enumerate those 5 ports.
    
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
    
    (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
firewall-passed3" policyvector " AND
internal-result3" vector " AND
 NOT port-80(dest-port-in) AND
 NOT port-20(dest-port-in) AND
 NOT port-21(dest-port-in) AND
 NOT port-23(dest-port-in) AND
 NOT port-3389(dest-port-in)"
" TUPLING"))

    (display-response (mtext "IS POSSIBLE?"))
    
    
    
  ;(stop-margrave-engine)
    ))