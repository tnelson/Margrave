; Margrave Lite version of simple IOS firewall demo.

; Individual examples are separated by (pause-for-user) calls, 
; so you can step through this file and read the examples while
; running it.

(printf "Running example: Margrave Lite simple IOS:~n~n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Margrave's query language currently requires request vectors to be
; provided in full. That is, users must spell out
; R(x1, x2, x3, x4, ...) instead of 
; R(<x>).

; We are working on adding features that will make vectors easier to
; manage. For now, we do the following to make vectors less cumbersome:

(define polvector "(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The engine is already loaded when Margrave Lite starts up. 

; Load all the policies.
; These are three different versions of the same configuration. The policies are loaded 
; with the suffixes 1,2, and 3 respectively.

(parse-and-load-ios "demo.txt" (build-path margrave-home-path "examples" "policies" "ios-demo" "initial") "" "1")
(parse-and-load-ios "change1.txt" (build-path margrave-home-path "examples" "policies" "ios-demo" "change1") "" "2")
(parse-and-load-ios "change2.txt" (build-path margrave-home-path "examples" "policies" "ios-demo" "change2") "" "3")

; If you have already parsed, and do not want to re-parse, you can load the intermediate IOS policies
; directly by using:
;(load-ios-policies (build-path (current-directory) "initial") "" "1")
;(load-ios-policies (build-path (current-directory) "change1") "" "2")
;(load-ios-policies (build-path (current-directory) "change2") "" "3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example: which-packets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "~n~nWhich-packets:~n")

(display-response (mtext "EXPLORE InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)
     TUPLING")  )
; The TUPLING keyword activates the tupling optimization, which is very useful for firewalls.

(printf "~a~n" (mtext "SHOW ONE"))

(pause-for-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example: verification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "~n~nVerification:~n")

(display-response (mtext "EXPLORE InboundACL1:Permit(ahostname, entry-interface, 
        src-addr-in, src-addr-out, 
        dest-addr-in, dest-addr-out, 
        protocol, message, flags,
        src-port-in,  src-port-out, 
        dest-port-in, dest-port-out, 
        length, next-hop, exit-interface)

        AND src-addr-in = 10.1.1.2 
        AND fe0 = entry-interface

     TUPLING") )
(display-response (mtext "IS POSSIBLE?"))

(pause-for-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example: rule responsibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here, we start using the polvector variable that we defined above.

(printf "~n~nRule-blaming:~n")

(display-response (mtext "EXPLORE InboundACL1:Deny" polvector
                         
                         " AND 10.1.1.2 = src-addr-in"
                         " AND fe0 = entry-interface "
                         
                         " INCLUDE InboundACL1:Router-fe0-line9_applies" polvector ","
                         "InboundACL1:Router-fe0-line12_applies" polvector
                         " TUPLING"))
(display-response (mtext "SHOW REALIZED InboundACL1:Router-fe0-line9_applies" polvector ","
                         "InboundACL1:Router-fe0-line12_applies" polvector))

(pause-for-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example: change-impact
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "~n~nChange-impact:~n")

; vs change 1 
; (Differencing versions 1 and 2)

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
(display-response (mtext "IS POSSIBLE?"))

(pause-for-user)

; Vs. change 2
; (Differencing versions 1 and 3)

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
(display-response  (mtext "IS POSSIBLE?"))

(pause-for-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; example: Rule relationships
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(printf "~n~nRule relationships:~n")

; This involves rules in the first change (version 2)
; line 12 wants to apply: what prevents it from doing so?

(display-response (mtext "EXPLORE InboundACL2:Router-fe0-line12_matches" polvector                                        
                         " INCLUDE InboundACL2:Router-fe0-line9_applies" polvector ","
                         "InboundACL2:Router-fe0-line10_applies" polvector ","
                         "InboundACL2:Router-fe0-line11_applies" polvector
                         " TUPLING")) 
(display-response (mtext "SHOW REALIZED InboundACL2:Router-fe0-line9_applies" polvector ","
                         "InboundACL2:Router-fe0-line10_applies" polvector ","
                         "InboundACL2:Router-fe0-line11_applies" polvector))



; Expect to see that line9 applies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; To exit Margrave Lite, use the (exit) command.
; (exit)