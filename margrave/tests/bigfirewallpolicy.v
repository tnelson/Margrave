(PolicyVocab BigFirewallPolicy
             (Types
              ; Test larger number of subdomains. Try 100 IP ranges and 50 ports
              (IPAddress : ip00 ip01 ip02 ip03 ip04 ip05 ip06 ip07 ip08 ip09
                           ip10 ip11 ip12 ip13 ip14 ip15 ip16 ip17 ip18 ip19
                           ip20 ip21 ip22 ip23 ip24 ip25 ip26 ip27 ip28 ip29
                           ip30 ip31 ip32 ip33 ip34 ip35 ip36 ip37 ip38 ip39
                           ip40 ip41 ip42 ip43 ip44 ip45 ip46 ip47 ip48 ip49
			   ip50 ip51 ip52 ip53 ip54 ip55 ip56 ip57 ip58 ip59
                           ip60 ip61 ip62 ip63 ip64 ip65 ip66 ip67 ip68 ip69
                           ip70 ip71 ip72 ip73 ip74 ip75 ip76 ip77 ip78 ip79
                           ip80 ip81 ip82 ip83 ip84 ip85 ip86 ip87 ip88 ip89
                           ip90 ip91 ip92 ip93 ip94 ip95 ip96 ip97 ip98 ip99
                         )
              (Port : port00 port01 port02 port03 port04 port05 port06 port07 port08 port09
                      port10 port11 port12 port13 port14 port15 port16 port17 port18 port19
		      port20 port21 port22 port23 port24 port25 port26 port27 port28 port29
                      port30 port31 port32 port33 port34 port35 port36 port37 port38 port39
                      port40 port41 port42 port43 port44 port45 port46 port47 port48 port49
                    )
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
              
              (disjoint-all Port)
              (nonempty Port)
              (nonempty IPAddress)
              
             ; (disjoint ip00 ip01)
             ; (disjoint ip00 ip02)
	      
	     ; Won't *always* have all ranges disjoint (for instance, overlapping...)
	     ; But making them ALL disjoint stresses Margrave, so test that way.
	      (disjoint-all IPAddress)
	      (disjoint-all Port)

              ; no disjoint-from-others constraint... 
              
              ))
