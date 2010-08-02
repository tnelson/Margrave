#lang racket

(require (file "../../margrave.rkt")
         (file "../../margrave-ios.rkt"))

(define (run-queries-for-example)
  
  ; Start Margrave's java engine
  (start-margrave-engine (build-path (current-directory) 'up 'up))
                         
  
  ; Load all the policies in this directory.
  (load-ios-policies (current-directory))
  
  (mtext "EXPLORE InboundACL:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)
     TUPLING")  
  ; The TUPLING keyword activates the tupling optimization, which is very useful for firewalls.
  
  ; !!! todo change to make 0 unnecessary
  (mtext "GET ONE 0")
  
  (stop-margrave-engine))