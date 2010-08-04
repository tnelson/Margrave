#lang racket

(require (file "../margrave.rkt")
         (file "../margrave-ios.rkt"))

; IMPORTANT! --->
; Things that need bugfixing for the paper are marked with TODO.


(define vector "(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)")


(define (run-queries-for-example)
  
  ; Start Margrave's java engine
  ; Pass path of the engine files: 1 level up from here.
  (start-margrave-engine (build-path (current-directory) 'up))
  
  (with-handlers ((exn:fail? (lambda (v) 
                               (printf "Racket produced a failure exception.~n Stopping the Margrave engine.~n")
                               (stop-margrave-engine)
                               (raise v))))
    
    ; Load all the policies 
    ; InboundACL -> InboundACL1, InboundACL2, InboundACL3 respectively.
    (load-ios-policies (build-path (current-directory) "initial") "" "1")
    (load-ios-policies (build-path (current-directory) "change1") "" "2")
    (load-ios-policies (build-path (current-directory) "change2") "" "3")
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; which-packets
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nWhich-packets:~n")
    
    (mtext "EXPLORE InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)
     TUPLING")  
    ; The TUPLING keyword activates the tupling optimization, which is very useful for firewalls.
    
    (mtext "GET ONE 0")
    
    ;; TODO: pretty-printer is including non-most-specific sort names. (need to make a query to the vocab?)   

    
    ;  TODO: change to make 0 unnecessary
    ; TODO: add exception-check to load-policy etc
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; verification
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nVerification:~n")
    
    (mtext "EXPLORE InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

        AND ip-10-1-1-2(src-addr-in)
        AND fe0(entry-interface)

     TUPLING")  
    (mtext "IS POSSIBLE? 0")
    
    ;; TODO: this should be show possible: needs to be added to parser
    ;; TODO also pretty-printer needs to... pretty print an unsatisfiable!
    ;; TODO: add the fe0 restriction to the document! (needs to be there, or it's sat.)
    
        ; TODO: pretty print booleans? 
    
    ;    <MARGRAVE-RESPONSE type="boolean">false<STATISTICS computed-max-size="1" max-size="1" result-id="0" user-max-size="6"/>
;
;</MARGRAVE-RESPONSE>
    
    
    ;; due to gensym use, rule names will change along with line numbers and on each re-parse
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; rule responsibility
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nRule-blaming:~n")
        
    (mtext (string-append "EXPLORE InboundACL1:Deny" vector

     " AND ip-10-1-1-2(src-addr-in)"
     " AND fe0(entry-interface) "

     " IDBOUTPUT InboundACL1:ACE-line-10-g21711_applies" vector ","
               "InboundACL1:ACE-line-13-g21714_applies" vector
     " TUPLING")) 
    (mtext (string-append "SHOW POPULATED 0 InboundACL1:ACE-line-10-g21711_applies" vector ","
               "InboundACL1:ACE-line-13-g21714_applies" vector))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; change-impact
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nChange-impact:~n")
    
    ; vs change 1    
    (mtext "EXPLORE (InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL2:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

       OR
       (InboundACL2:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

     TUPLING")  
    (mtext "IS POSSIBLE? 0")
    
    
    ; Vs. change 2
    (mtext "EXPLORE (InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL3:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

       OR
       (InboundACL3:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

       AND NOT InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface) )

     TUPLING")  
    (mtext "IS POSSIBLE? 0")
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Rule relationships
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nRule relationships:~n")
    
    
    
    
    

    
    
    
    ; It's correct. The problem is that dest-addr-in need not be in 0.0.0.0/0.0.0.0 (which ought to be all IPs?)
    ; This is a problem with the parser-generated .v
    
    
  (stop-margrave-engine)))