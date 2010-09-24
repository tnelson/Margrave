(PolicyVocab examplefw1
             (Types

	     ; An IP address is one of the following. 
              (IPAddress : blacklistaddr webserver ftpserver )

	      ; A port is one of the following.
              (Port : port21 port80)

	      ; Protocol is either TCP or UDP
	      (Protocol : TCP UDP)
 	     )

	     ; Firewalls either Accept or Drop a packet.
             (Decisions Accept Drop)

             (Predicates
              )
               
    	     ; Describes the layout of a request. In this case, it's the network layer's header's contents.		
             ; A real packet would contain a payload, but we are not concerned with it.
             (ReqVariables (ipsrc : IPAddress)
                           (ipdest : IPAddress)
                           (portsrc : Port)
                           (portdest : Port)
			   (pro : Protocol)
                           )            
            
	     (OthVariables )

		; How are our types above constrained?
		; For instance, all ports are disjoint (they represent distinct numbers) 
             (Constraints
                      
	      ; Ports are disjoint
              (disjoint-all Port)

	      ; In this example, all our IP Address singletons declared above are disjoint from one another.
	      (disjoint-all IPAddress)
	      ; In a future example, however, we will deal with IP Address sets, which need not be disjoint.

	      (disjoint-all Protocol)
              
	      ; There is at least one Port and at least one IPAddress in the world.
	      ; (This prunes out bad solutions.)
	      (nonempty Port)
              (nonempty IPAddress)
	
	      ; 0-1 atoms per Port. 0-1 atoms per atomic ip address.
	      (atmostone port21)
              (atmostone port80)

	      ; (Again, this may not be the case in a more complex example. An IP range may certainly contain more than one IP Address!)
	      (atmostone blacklistaddr)
              (atmostone webserver)
              (atmostone ftpserver)
	      (atmostone-all Protocol)
              (abstract Protocol) 
              ))

