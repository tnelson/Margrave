(PolicyVocab FirewallPolicy
             (Types
              (IPAddress : Zoonet one two)
              (Port : nntp traceroute https ssh)
              )
             (Decisions 
              Accept
              Drop)
             (Predicates
              (Connection : IPAddress IPAddress Port Port)
              )
                         
             (ReqVariables (ipsrc : IPAddress)
                           (ipdest : IPAddress)
                           (portsrc : Port)
                           (portdest : Port)
                           )             
             (OthVariables (port1 : Port)
                           (port2 : Port)
                           )

             (Constraints
              (disjoint-all IPAddress)
              (disjoint-all Port)
              (atmostone one)
	      (atmostone two) 
              (atmostone-all Port)
              ))
