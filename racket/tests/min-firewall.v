
; To use predicates instead of sorts for addr ranges, really ought to have a unary-preds-all-disjoint constraint

(Theory min-firewall
        (Vocab min-firewall
               (Types
                (IPAddress > IPRange0to4 IPRange5to9)
                
                (IPRange0to4 > IP0 IP1 IP2 IP3 IP4)
                (IPRange5to9 > IP5 IP6 IP7 IP8 IP9)
                
		Port
		(Protocol > PTCP PUDP))
               (Predicates                                
                ))
        (Axioms 
	 (atmostone IP0)
	 (atmostone IP1)
	 (atmostone IP2)
	 (atmostone IP3)
	 (atmostone IP4)
	 (atmostone IP5)
	 (atmostone IP6)
	 (atmostone IP7)
	 (atmostone IP8)
	 (atmostone IP9)
         (abstract Protocol)))        	    	     

