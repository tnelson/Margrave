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
    ; 1
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
    
  ;; TODO: will it think port-20 is an IDB? yes. need to fix this in java. <------ 
                         
                         
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    #|(printf "~n~nRule-blaming:~n")
        
    (mtext (string-append "EXPLORE InboundACL1:Deny" vector

     " AND ip-10-1-1-2(src-addr-in)"
     " AND fe0(entry-interface) "

     " IDBOUTPUT InboundACL1:ACE-line-10-g21711_applies" vector ","
               "InboundACL1:ACE-line-13-g21714_applies" vector
     " TUPLING")) 
    (mtext (string-append "SHOW POPULATED 0 InboundACL1:ACE-line-10-g21711_applies" vector ","
               "InboundACL1:ACE-line-13-g21714_applies" vector))
    |#
    

    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; 
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    #|
    (printf "~n~nRule relationships:~n")
    
    ;; This involves rules in the first change (InboundACL2)
    ; line 13 wants to apply: what prevents it from doing so?
    
    (mtext (string-append "EXPLORE InboundACL2:ACE-line-13-g23723" vector

     " IDBOUTPUT InboundACL2:ACE-line-10-g23720_applies" vector ","
               "InboundACL2:ACE-line-11-g23721_applies" vector ","
               "InboundACL2:ACE-line-12-g23722_applies" vector
     " TUPLING")) 
    (mtext (string-append "SHOW POPULATED 0 InboundACL2:ACE-line-10-g23720_applies" vector ","
               "InboundACL2:ACE-line-11-g23721_applies" vector ","
               "InboundACL2:ACE-line-12-g23722_applies" vector))
    
    
|#
    
    
    
    
  (stop-margrave-engine)))