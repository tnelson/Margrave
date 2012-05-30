(Policy uses min-firewall
        (Variables 
         ;(ipsrc IPAddress)
         (ipdest IPAddress)
         ;(portsrc Port)
         ;(portdest Port)
         (pro Protocol))
        (Rules 
;  	  (r0 = (permit ipsrc ipdest portsrc portdest pro) :- (IP0 ipdest) (PTCP pro))
;          (rrange1 = (permit ipsrc ipdest portsrc portdest pro) :- (IPRange0to4 ipdest) (PTCP pro))		
;  	  (r1 = (permit ipsrc ipdest portsrc portdest pro) :- (IP1 ipdest) (PTCP pro))
 ; 	  (r2 = (permit ipsrc ipdest portsrc portdest pro) :- (IP2 ipdest) (PTCP pro))
 ; 	  (r3 = (permit ipsrc ipdest portsrc portdest pro) :- (IP3 ipdest) (PTCP pro))
 ; 	  (r4 = (permit ipsrc ipdest portsrc portdest pro) :- (IP4 ipdest) (PTCP pro))
 ; 	  (r5 = (permit ipsrc ipdest portsrc portdest pro) :- (IP5 ipdest) (PTCP pro))
 ; 	  (r6 = (permit ipsrc ipdest portsrc portdest pro) :- (IP6 ipdest) (PTCP pro))
 ; 	  (r7 = (permit ipsrc ipdest portsrc portdest pro) :- (IP7 ipdest) (PTCP pro))
 ; 	  (r8 = (permit ipsrc ipdest portsrc portdest pro) :- (IP8 ipdest) (PTCP pro))
 ;         
         ;(r9 = (permit ipsrc ipdest portsrc portdest pro) :- (IP9 ipdest) (PTCP pro))
  	  (r0 = (permit ipdest pro) :- (IP0 ipdest) (PTCP pro))
          (rrange1 = (permit ipdest pro) :- (IPRange0to4 ipdest) (PTCP pro))		
  	  (r1 = (permit ipdest pro) :- (IP1 ipdest) (PTCP pro))
  	  (r2 = (permit ipdest pro) :- (IP2 ipdest) (PTCP pro))
  	  (r3 = (permit ipdest pro) :- (IP3 ipdest) (PTCP pro))
  	  (r4 = (permit ipdest pro) :- (IP4 ipdest) (PTCP pro))
  	  (r5 = (permit ipdest pro) :- (IP5 ipdest) (PTCP pro))
  	  (r6 = (permit ipdest pro) :- (IP6 ipdest) (PTCP pro))
  	  (r7 = (permit ipdest pro) :- (IP7 ipdest) (PTCP pro))
  	  (r8 = (permit ipdest pro) :- (IP8 ipdest) (PTCP pro))
  	  (r9 = (permit ipdest pro) :- (IP9 ipdest) (PTCP pro))


)
        (RComb (fa permit deny)))


