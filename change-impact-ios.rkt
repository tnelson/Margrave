#lang racket

(require "margrave.rkt"
         "margrave-ios.rkt")
         
(define reqpol "(hostname, entry-interface, src-addr-in, src-addr-out,
                 dest-addr-in, dest-addr-out, protocol, message, flags,
                 src-port-in, src-port-out, dest-port-in, dest-port-out,
                 length, next-hop, exit-interface)")

(define reqfull1 "(hostname, entry-interface, src-addr-in, src-addr_-1, src-addr-out-1,
                  dest-addr-in, dest-addr_-1, dest-addr-out-1, protocol, message, flags,
                  src-port-in, src-port_-1, src-port-out-1, dest-port-in, dest-port_-1,
                  dest-port-out-1, length, next-hop-1, exit-interface-1)")

(define reqfull2 "(hostname, entry-interface, src-addr-in, src-addr_-2, src-addr-out-2,
                  dest-addr-in, dest-addr_-2, dest-addr-out-2, protocol, message, flags,
                  src-port-in, src-port_-2, src-port-out-2, dest-port-in, dest-port_-2,
                  dest-port-out-2, length, next-hop-2, exit-interface-2)")



(define (run-timed-script pDirectoryName)
  
  ; Start the Java process
  (start-margrave-engine (current-directory) '("-Xss2048k" "-Xmx1g")  )  ; '( "-log")
  
  ; Print out baseline memory consumption (before policies loaded)
  (display-response (mtext "INFO"))
  
  ; Start the timer
  (time-since-last)

  ; load the policy. last parameter is verbosity (include timing)
  (load-ios-policies (build-path pDirectoryName "initial") "" "1" #t)  
  ; Changes has all 3 changes, but the 3 queries look at different parts
  (load-ios-policies (build-path pDirectoryName "changes") "" "2" #t)  

  (printf "~nLoading complete.~n~n")
   (time-since-last)
   
  ; ------------------------------------------------------------------
  ;   Change-impact #1: InboundACL vs. InboundACL
  ; ------------------------------------------------------------------
 
  ; Query does not take advantage of binary nature of the policy; could
  ; technically check only permit vs. !permit (half size of this)
  (printf "Change-impact #1: InboundACL vs. InboundACL~n")
  (display-response (mtext "EXPLORE (InboundACL1:permit" reqpol " AND NOT InboundACL2:permit" reqpol ")"
                           " OR (InboundACL2:permit" reqpol " AND NOT InboundACL1:permit" reqpol ")"
                           " OR (InboundACL2:deny" reqpol " AND NOT InboundACL1:deny" reqpol ")"
                           " OR (InboundACL2:deny" reqpol " AND NOT InboundACL1:deny" reqpol ")"                           
                           " TUPLING"))
  (if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
      (begin
        (printf "-------------------------------------------------~n")
        (printf "Found semantic differences. First scenario found:~n")
        (printf "-------------------------------------------------~n")
        (display-response (mtext "GET ONE")))
      (printf "No semantic differences found.~n~n"))
  
  (printf "Time: ~a~n " (time-since-last))
    
  ; ------------------------------------------------------------------
  ;   Change-impact #2: NetworkSwitching vs. NetworkSwitching
  ;   "Which (packet, next-hop)s will be forwarded differently?
  ; ------------------------------------------------------------------
  
  (printf "Change-impact #2: NetworkSwitching vs. NetworkSwitching~n")
  (display-response (mtext "EXPLORE (NetworkSwitching1:Forward" reqpol " AND NOT NetworkSwitching2:Forward" reqpol ")"
                            " OR (NetworkSwitching2:Forward" reqpol " AND NOT NetworkSwitching1:Forward" reqpol ")"
                            " TUPLING"))
                          
                           
  (if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
      (begin
        (printf "-------------------------------------------------~n")
        (printf "Found semantic differences. First scenario found:~n")
        (printf "-------------------------------------------------~n")
        (display-response (mtext "GET ONE")))
      (printf "No semantic differences found.~n~n"))
  
  (printf "Time: ~a~n " (time-since-last))
  
  ; ------------------------------------------------------------------
  ;   Change-impact #3: entire firewall config vs. entire new config
  ; ------------------------------------------------------------------

  (printf "Change-impact #3: Entire config comparison~n")
  (display-response (mtext "EXPLORE internal-result1" reqfull1 " AND internal-result2" reqfull2 " AND"
                           
                           ;; Did the routing change?
                           ;; Did the packet's fields change?
                           
                           "( NOT (exit-interface-1 = exit-interface-2) OR "
                           
                           
                           " NOT (next-hop-1 = next-hop-2) OR "
                           
                           
                           
                           ;; TN ==================
                           ;; Deal with this issue: NAT, scenarios. Use translate?
                           
                           
                           " NOT (src-addr-out-1 = src-addr-out-2) OR "
                           " NOT (dest-addr-out-1 = dest-addr-out-2) OR "
                           " NOT (src-port-out-1 = src-port-out-2)  OR "
                           " NOT (dest-port-out-1 = dest-port-out-2) OR "
                           
                           ;; Did the firewall's ACL act differently?
                           
                           " (InboundACL1:permit" reqpol " AND NOT InboundACL2:permit" reqpol ") OR "
                           " (InboundACL2:permit" reqpol " AND NOT InboundACL1:permit" reqpol ") OR "
                           " (InboundACL1:deny" reqpol " AND NOT InboundACL2:deny" reqpol ") OR "
                           " (InboundACL2:deny" reqpol " AND NOT InboundACL1:deny" reqpol ") )"
                           " TUPLING"))
  (if (xml-bool-response->bool (mtext "IS POSSIBLE?"))
      (begin
        (printf "-------------------------------------------------~n")
        (printf "Found semantic differences. First scenario found:~n")
        (printf "-------------------------------------------------~n")
        (display-response (mtext "GET ONE")))
      (printf "No semantic differences found.~n~n"))
  
    (printf "Time: ~a~n " (time-since-last))
  
  
  ;(stop-margrave-engine)
  )