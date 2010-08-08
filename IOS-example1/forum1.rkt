#lang racket

(require (file "../margrave.rkt")
         (file "../margrave-ios.rkt"))

; IMPORTANT! --->
; Things that need bugfixing for the paper are marked with TODO.


(define oldvector "(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)")

(define vector "(ahostname, entry-interface, 
        src-addr-in, src-addr_, src-addr-out, 
        dest-addr-in, dest-addr_, dest-addr-out, 
        protocol, message,
        src-port-in, src-port_, src-port-out, 
        dest-port-in, dest-port_, dest-port-out, 
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
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Version 1
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n---------------1--------------~n----------------------------~n")
    
    
    ; TODO routed-packets needs to be separately defined with prefix/suffix
    
    (mtext (string-append "EXPLORE
NOT ip-192-168-2-0/ip-255-255-255-0(src-addr-in) AND
FastEthernet0(entry-interface) AND
prot-TCP(protocol) AND
port-80(src-port-in) AND
routed-packets1" vector
" TUPLING"))

    (mtext "IS POSSIBLE? 0")  
    
    (mtext "GET ONE 0")
    (mtext "GET NEXT 0")
    
    ;; TODO shouldn't we have a keyword that's syntactic sugar for "all subsorts of..."? 
    ;; TODO no way to show populated "other"... without that, this isn't a very interesting show populated?
    ;;   (but abstractness is expensive. can we get "other" here without an explicit sort and abstractness?)
    ; !!! TODO tupling isn't shown in the paper
   
    (mtext (string-append "SHOW POPULATED 0 port-80(dest-port-in)" 
                         ", port-20(dest-port-in)" 
                         ", port-21(dest-port-in)" 
                         ", port-23(dest-port-in)" 
                         ", port-3389(dest-port-in)"))
    

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
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Version 3
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
   ; TODO reflexive access list revision
    
    
    
  (stop-margrave-engine)))