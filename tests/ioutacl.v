(PolicyVocab ioutacl
             (Types
              (IPAddress : ftpserver ftpclient)
              (Port : port21 port20)
	      (Protocol : TCP UDP)
 	     )

             (Decisions Accept Drop)

             (Predicates
              )
               
             (ReqVariables (ipsrc : IPAddress)
                           (ipdest : IPAddress)
                           (portsrc : Port)
                           (portdest : Port)
			   (pro : Protocol)
                           )            
            
	     (OthVariables )

             (Constraints
                      
              (disjoint-all Port)
	      (disjoint-all IPAddress)
	      (disjoint-all Protocol)
              
	      (nonempty Port)
              (nonempty IPAddress)
	
              (atmostone port21)
              (atmostone port20)

	      (atmostone ftpserver)
              (atmostone ftpclient)
	     
              (atmostone-all Protocol)
              (abstract Protocol)              
              ))

