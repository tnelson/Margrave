(PolicyVocab examplefw3
             (Types

              (IPAddress : blacklistaddr 
                           (dmz mailserver ftpserver webserver) 
                           privateserver)

              (Port : port21 port80 port25)

	      (Protocol : TCP UDP)
 	     )

             (Decisions Accept Drop)

             ; Is there a connection established between the parties?
             (Predicates
              (Connection : IPAddress IPAddress Port Port Protocol)
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
                      
	      ; Ports are disjoint, private server isn't in the dmz, etc.
              (disjoint-all Port)
	      (disjoint-all IPAddress)
	      (disjoint-all dmz)
	      (disjoint-all Protocol)
              
	      ; There is at least one of all our types.
	      (nonempty Port)
              (nonempty IPAddress)
              (nonempty Protocol)
	      
              (abstract Protocol)
	
              (atmostone port21)
              (atmostone port80)
              (atmostone port25)

	      (atmostone webserver)
              (atmostone mailserver)
              (atmostone ftpserver)
              (atmostone privateserver)

	      (atmostone-all Protocol)
              
              ))

