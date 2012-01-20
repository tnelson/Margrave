#lang racket

;(require margrave)
(require "../../margrave/margrave.rkt"
         rackunit)

; Start the engine
(start-margrave-engine #:margrave-params '( "-log" )
                       #:margrave-path "../../margrave")

; Our policies:
(m-load-policy "aclfw1" "inboundacl_fw1.p")
(m-load-policy "aclfw2" "inboundacl_fw2.p")
(m-load-policy "natfw1" "inboundnat_fw1.p")
(m-load-policy "natfw2" "inboundnat_fw2.p")
(m-load-policy "aclfw1new" "inboundacl_fw1_new.p")
(m-load-policy "aclfw2new" "inboundacl_fw2_new.p")

; ************************************************************
; Example 1: "Is it possible for the firewall(s) to drop packets
; from the employee's PC to an external server on port 80?
; ************************************************************

; Query generated by the topology of the network: A request coming from the employee PC
; can be dropped if:
; (1) Internal FW rejects it, or
; (2) External FW rejects it (after it is modified by internal FW's NAT)

(m-let "Example1" '([interf Interface] 
                    [ipsrc IPAddress]
                    [ipdest IPAddress]
                    [portsrc Port]
                    [portdest Port]
                    [pro Protocol]
                    [tempnatsrc IPAddress]
                    [interminterface Interface])
       '(and 
         (= 'fw2int interf)
         (= 'fw1dmz interminterface)         
         (OtherPorts portsrc)
         (= 'managerpc ipsrc)
         (= 'port80 portdest)
         (= 'tcp pro)
         (OutsideIPs ipdest)
         
         (not (= ipdest ipsrc))
         (not (= portsrc portdest))
         
         ; Bind the result of NAT at FW2 to tempnatsrc
         ([natfw2 translate] interf ipsrc ipdest portsrc portdest pro tempnatsrc)
         
         ; Two places the packet could be denied. One pre-NAT, one post-NAT.
         (or
          ([aclfw2 deny] interf ipsrc ipdest portsrc portdest pro)                               
          ([aclfw1 deny] interminterface tempnatsrc ipdest portsrc portdest pro)))
       )


(printf "~n----------------------------------~nExample 1 results:~n----------------------------------~n~n")
(check-true (m-is-poss? "Example1"))
;(display (m-show "Example1"))





; ************************************************************
; Example 2: "How is it the case that this packet could be dropped?"
; ************************************************************

(printf "~n----------------------------------~nExample 2 results:~n----------------------------------~n~n")
(check-not-exn (lambda () (m-show "Example1" #:include '(([aclfw2 deny] interf ipsrc ipdest portsrc portdest pro) 
                                                         ([aclfw1 deny] interf tempnatsrc ipdest portsrc portdest pro)
                                                         ([aclfw1 rule1_applies] interf tempnatsrc ipdest portsrc portdest pro)
                                                         ([aclfw1 rule2_applies] interf tempnatsrc ipdest portsrc portdest pro)
                                                         ([aclfw1 rule3_applies] interf tempnatsrc ipdest portsrc portdest pro)
                                                         ([aclfw1 rule4_applies] interf tempnatsrc ipdest portsrc portdest pro)
                                                         ([aclfw1 rule5_applies] interf tempnatsrc ipdest portsrc portdest pro)
                                                         ([aclfw1 rule6_applies] interf tempnatsrc ipdest portsrc portdest pro)
                                                         ([aclfw1 rule7_applies] interf tempnatsrc ipdest portsrc portdest pro)))))





; *************************************************
; Example 3: Testing original property after first change
; *************************************************

(m-let "Example3" '([interf Interface] 
                    [ipsrc IPAddress]
                    [ipdest IPAddress]
                    [portsrc Port]
                    [portdest Port]
                    [pro Protocol]
                    [tempnatsrc IPAddress]
                    [interminterface Interface])
       '(and 
         (= 'fw2int interf)
         (= 'fw1dmz interminterface)         
         (OtherPorts portsrc)
         (= 'managerpc ipsrc)
         (= 'port80 portdest)
         (= 'tcp pro)
         (OutsideIPs ipdest)
         
         (not (= ipdest ipsrc))
         (not (= portsrc portdest))
         
         ; Bind the result of NAT at FW2 to tempnatsrc
         ([natfw2 translate] interf ipsrc ipdest portsrc portdest pro tempnatsrc)
         
         ; Two places the packet could be denied. One pre-NAT, one post-NAT.
         (or
          ([aclfw2 deny] interf ipsrc ipdest portsrc portdest pro)                               
          ([aclfw1new deny] interminterface tempnatsrc ipdest portsrc portdest pro))))

(printf "~n----------------------------------~nExample 3 results:~n----------------------------------~n~n")
(check-false (m-is-poss? "Example3"))
;(display (m-show "Example3"))



; ************************************************************
; Example 4: "Now that we fixed the bug, what changed?"
; ************************************************************

; NOT a full change-impact. Shorter due to what we changed and
; the topology. We only altered ONE policy and ONE interface.


(m-let "Example4" '([interf Interface] 
                    [ipsrc IPAddress]
                    [ipdest IPAddress]
                    [portsrc Port]
                    [portdest Port]
                    [pro Protocol]
                    [tempnatsrc IPAddress]
                    [interminterface Interface])
'(and
  ; Packet coming from fw1dmz via fw2int, passed and translated by fw2:
  (= 'fw2int interf)
  (= 'fw1dmz interminterface)    
  ([aclfw2 accept] interf ipsrc ipdest portsrc portdest pro)
  ([natfw2 translate] interf ipsrc ipdest portsrc portdest pro tempnatsrc)
  
  ; Ask for scenarios for everyone else, not just the manager
  (not (= 'managerpc ipsrc))
  
  ; Gain or loss of access vs. new policy
  (or
      (and
        (not ([aclfw1 accept] interminterface tempnatsrc ipdest portsrc portdest pro))        
        ([aclfw1new accept] interminterface tempnatsrc ipdest portsrc portdest pro))
      (and
        (not ([aclfw1new accept] interminterface tempnatsrc ipdest portsrc portdest pro))        
        ([aclfw1 accept] interminterface tempnatsrc ipdest portsrc portdest pro)))))


(printf "~n----------------------------------~nExample 4 results:~n----------------------------------~n~n")
(check-true (m-is-poss? "Example4"))
;(display (m-show "Example4"))



; **************************************************
; Example 5: verifying the new property
; after SECOND new policy
; **************************************************

; Two possible kinds of bad behavior to look for:
         
; (a) non-manager is allowed access
(m-let "Example5a" '([interf Interface] 
                     [ipsrc IPAddress]
                     [ipdest IPAddress]
                     [portsrc Port]
                     [portdest Port]
                     [pro Protocol]
                     [tempnatsrc IPAddress]
                     [interminterface Interface])
       '(and
         ; General conditions
         (= 'fw2int interf)
         (= 'fw1dmz interminterface)  
         (= 'port80 portdest)
         (= 'tcp pro)
         (OutsideIPs ipdest)         
                   
         ; Not the manager, and gets through
         (not (= 'managerpc ipsrc))
         ([aclfw2new accept] interf ipsrc ipdest portsrc portdest pro)
         ([natfw2 translate] interf ipsrc ipdest portsrc portdest pro tempnatsrc)
         ([aclfw1new accept] interminterface tempnatsrc ipdest portsrc portdest pro)))

; (b) The manager is being denied access to the web
(m-let "Example5b" '([interf Interface] 
                     [ipsrc IPAddress]
                     [ipdest IPAddress]
                     [portsrc Port]
                     [portdest Port]
                     [pro Protocol]
                     [tempnatsrc IPAddress]
                     [interminterface Interface])
       '(and      
         ; General conditions
         (= 'fw2int interf)
         (= 'fw1dmz interminterface)  
         (= 'port80 portdest)
         (= 'tcp pro)
         (OutsideIPs ipdest) 
         
         ; Manager, and denied.
         (= 'managerpc ipsrc)
         (or
          ; denied at FW2
          ([aclfw2new deny] interf ipsrc ipdest portsrc portdest pro)            
          ; or denied at FW1
          (and
           ([natfw2 translate] interf ipsrc ipdest portsrc portdest pro tempnatsrc)
           ([aclfw1new deny] interminterface tempnatsrc ipdest portsrc portdest pro)))))

  
  
(printf "~n----------------------------------~nExample 5 results:~n----------------------------------~n~n")
(check-false (m-is-poss? "Example5a"))
(check-false (m-is-poss? "Example5b"))

;(m-is-poss? "Example5a")
;(m-is-poss? "Example5b")

; ^^^ both return #f
 

;; ********************************************************
;; Example 6
;; Re-run query 5, but with the 3rd policy specs ("rbac" via nat)
;; ********************************************************

(m-load-policy "aclfw1ex6" "ex6/inboundacl_fw1_ex6.p")
(m-load-policy "aclfw2ex6" "ex6/inboundacl_fw2_ex6.p")
(m-load-policy "natfw2ex6" "ex6/inboundnat_fw2_ex6.p")

; (a) non-manager is allowed access
(m-let "Example6a" '([interf Interface] 
                     [ipsrc IPAddress]
                     [ipdest IPAddress]
                     [portsrc Port]
                     [portdest Port]
                     [pro Protocol]
                     [tempnatsrc IPAddress]
                     [interminterface Interface])
       '(and
         ; General conditions
         (= 'fw2int interf)
         (= 'fw1dmz interminterface)  
         (= 'port80 portdest)
         (= 'tcp pro)
         (OutsideIPs ipdest)         
                   
         ; Not the manager, and gets through
         (not (= 'managerpc ipsrc))
         ([aclfw2ex6 accept] interf ipsrc ipdest portsrc portdest pro)
         ([natfw2ex6 translate] interf ipsrc ipdest portsrc portdest pro tempnatsrc)
         ([aclfw1ex6 accept] interminterface tempnatsrc ipdest portsrc portdest pro)))

; (b) The manager is being denied access to the web
(m-let "Example6b" '([interf Interface] 
                     [ipsrc IPAddress]
                     [ipdest IPAddress]
                     [portsrc Port]
                     [portdest Port]
                     [pro Protocol]
                     [tempnatsrc IPAddress]
                     [interminterface Interface])
       '(and      
         ; General conditions
         (= 'fw2int interf)
         (= 'fw1dmz interminterface)  
         (= 'port80 portdest)
         (= 'tcp pro)
         (OutsideIPs ipdest) 
         
         ; Manager, and denied.
         (= 'managerpc ipsrc)
         (or
          ; denied at FW2
          ([aclfw2ex6 deny] interf ipsrc ipdest portsrc portdest pro)            
          ; or denied at FW1
          (and
           ([natfw2ex6 translate] interf ipsrc ipdest portsrc portdest pro tempnatsrc)
           ([aclfw1ex6 deny] interminterface tempnatsrc ipdest portsrc portdest pro)))))

(printf "~n----------------------------------~nExample 6 results:~n----------------------------------~n~n")
(check-false (m-is-poss? "Example6a"))
(check-false (m-is-poss? "Example6b"))


(m-let "Exampleisa1" '([ipdest Interface] 
                     [ipsrc IPAddress])
       '(isa ipdest Port true)
       #:under '("aclfw1ex6"))

;(m-is-poss? "Example6a")
;(m-is-poss? "Example6b")
; ^^^ both return #f
