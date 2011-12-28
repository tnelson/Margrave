(PolicyVocab examplefw2
             (Types

              ; Note that we now have a *set* of IPs defined explicitly: dmz.
              (IPAddress : blacklistaddr 
			   ; something in dmz is either the mailserver, the ftpserver, the webserver
			   ;   (or something else, since this sort isn't abstract)
                           (dmz mailserver ftpserver webserver) 
                           privateserver)

	      ; A port is one of the following.
              (Port : port21 port80 port25 )

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
                      
	      ; Ports are disjoint, private server isn't in the dmz, etc.
              (disjoint-all Port)
	      (disjoint-all IPAddress)
	      (disjoint-all dmz)
	      (disjoint-all Protocol)
              
	      ; There is at least one of all our types.
	      (nonempty Port)
              (nonempty IPAddress)
              (nonempty Protocol)
	
              ; port21, port80, and port25 are atomic. 
              (atmostone port21)
              (atmostone port80)
              (atmostone port25)

	      (atmostone webserver)
              (atmostone mailserver)
              (atmostone ftpserver)
              (atmostone privateserver)
	      ; Note *not* atmostone dmz. Obviously there could be two different things there.

	      (abstract Protocol)
	      (atmostone-all Protocol)
              
              ))

