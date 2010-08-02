#lang racket

(require (file "../../margrave.rkt")
         (file "../../margrave-ios.rkt"))

; IMPORTANT! --->
; Things that need bugfixing for the paper are marked with TODO.



(define (run-queries-for-example)
  
  ; Start Margrave's java engine
  (start-margrave-engine (build-path (current-directory) 'up 'up))
  
  (with-handlers ((exn:fail? (lambda (v) 
                               (printf "Racket produced a failure exception.~n Stopping the Margrave engine.~n")
                               (stop-margrave-engine)
                               (raise v))))
    
    ; Load all the policies in this directory.
    (load-ios-policies (current-directory) "" "1")
    
    
    
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

        AND ip-173-194-33-104(src-addr-in)
        AND fe0(entry-interface)

     TUPLING")  
   ; (mtext "IS POSSIBLE? 0")
    
    ;; TODO: this should be show possible: needs to be added to parser
    ;; TODO also pretty-printer needs to... pretty print an unsatisfiable!
    ;; TODO: add the fe0 restriction to the document! (needs to be there, or it's sat.)
    (mtext "GET ONE 0")
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; rule responsibility
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nRule-blaming:~n")
    
    
   #| (mtext "EXPLORE InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

        AND ip-173-194-33-104(src-addr-in)
        AND fe0(entry-interface)

     IDBOUTPUT xxx

     TUPLING")  
    (mtext "SHOW POPULATED ")
    |#
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; change-impact
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nChange-impact:~n")
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Rule relationships
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (printf "~n~nRule relationships:~n")
    
    
  (stop-margrave-engine)))