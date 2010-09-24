; Vocab and Policies using ACLs from Happy Router's ACL tutorial video
; http://happyrouter.com/free-video-harden-your-cisco-router-with-ios-acls


(PolicyVocab happyrouteracl
             (Types
              (IPAddress : 10network webserver pc )
              (Port : http https ftp ftpdata pop3 smtp )
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
	
	      (atmostone http)
              (atmostone https)
              (atmostone ftp)
              (atmostone ftpdata)
              (atmostone pop3)
              (atmostone smtp)

	      (atmostone webserver)
              (atmostone pc)
              ; 10network may contain >1 atoms
	     
              (atmostone-all Protocol)
              (abstract Protocol)     
              ))

