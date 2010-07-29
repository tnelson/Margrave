#lang racket

(require (file "../../margrave.rkt")
         (file "../../margrave-ios.rkt"))

(define (run-queries-for-example)
  
  ; Start Margrave's java engine
  (start-margrave-engine (build-path (current-directory) 'up 'up))
                         
  
  ; Load all the policies in this directory.
  (load-ios-policies (current-directory))
  
  (m "EXPLORE routed-packets(ahostname, entry-interface, 
        src-addr-in, src-addr_, src-addr-out, 
        dest-addr-in, dest-addr_, dest-addr-out, 
        protocol, message,
        src-port-in, src-port_, src-port-out, 
        dest-port-in, dest-port_, dest-port-out, 
        length, next-hop, exit-interface)
    AND
    GigabitEthernet0/0(entry-interface) AND
    ip-10-232-4-0/ip-255-255-252-0(src-addr-in) AND
    ip-0-0-0-0/ip-0-0-0-0(dest-addr-in) AND
    ip-10-232-0-15(next-hop) AND
    GigabitEthernet0/1(exit-interface)    
    TUPLING")  
  ; The TUPLING keyword activates the tupling optimization, which is very useful for firewalls.
  
  ; todo change to Varun's new pretty-printer
  (m "SHOW ONE 0")
  
  (stop-margrave-engine))