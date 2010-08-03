(PolicyVocab TalkFirewallPolicy
             (Types
              (IPAddress : employeepc managerpc contractorpc webserver mailserver fw2static outsideips blacklistedip)
              (Port : port21 port23 port25 port80 otherports)
              (Protocol : tcp udp )
              
              ; FW1 has interfaces facing the outside world and the DMZ.
              ; FW2 has interfaces facing the internal net and the DMZ.
              (Interface : fw1dmz fw2dmz fw2int fw1ext)
              )
             
             (Decisions 
              Accept
              Translate
              Deny)
             
             (Predicates )
             
             (ReqVariables 
             
	      ; Entry interface in the context of a particular firewall
	      (interf : Interface)
              
              ; Packet header (again IN THE CONTEXT of a particular firewall)
              (ipsrc : IPAddress)
              (ipdest : IPAddress)
              (portsrc : Port)
              (portdest : Port)
              (pro : Protocol)
             
		(tempnatip : IPAddress) ; used by NAT policies, not ACLs. Ignore in ACLs.
 
              )         
             
             (OthVariables )
             
             (Constraints
              
              (nonempty Port)
              (nonempty IPAddress)
              (nonempty Interface)
              
              (atmostone managerpc)
              (atmostone webserver)
              (atmostone mailserver)
              (atmostone fw2static)
              
              (atmostone fw2int)
              (atmostone fw1ext)
              (atmostone fw1dmz)
              (atmostone fw2dmz)
              
              (atmostone port21)
              (atmostone port23)
              (atmostone port25)
              (atmostone port80)
                            
              (disjoint-all IPAddress)
              (disjoint-all Port)
              (disjoint-all Interface)
              (disjoint-all Protocol)
              
              )
             )
