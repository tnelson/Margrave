(Theory talkfirewallpolicy
        (Vocab talkfirewallpolicy
               
               (Constants
                ('employeepc InternalIPs)
                ('managerpc InternalIPs)
                ('contractorpc InternalIPs)
                ('webserver InternalIPs)
                ('mailserver InternalIPs)
                ('fw2static InternalIPs)        
                
                ('port21 Port) 
                ('port23 Port)
                ('port25 Port) 
                ('port80 Port)
                
                ('tcp Protocol) 
                ('udp Protocol)
                
                ; FW1 has interfaces facing the outside world and the DMZ.
                ; FW2 has interfaces facing the internal net and the DMZ.        
                ('fw1dmz Interface)
                ('fw2dmz Interface)
                ('fw2int Interface)
                ('fw1ext Interface))
               
               (Types
                (IPAddress > InternalIPs OutsideIPs BlacklistedIPs)        
                (Port > OtherPorts)
                Protocol              
                Interface)
                              
               (Predicates ))       
        (Axioms 
         ; A port that isn't one of the constants must be in other ports.
         (abstract Port)    
         ; There are no other interfaces than those named.
         (abstract Interface)
         
         ; IPs must be internal, external, or blacklisted.
         (abstract IPAddress)
         ; the constants given are the only internal IP addresses used
         (abstract InternalIPs)
           
         ; Everything is either tcp or udp 
         (abstract Protocol)
         
         ; These constants are distinct:
         (constants-neq-all InternalIPs) ; <-- NOT IPAddress. Only looks for constants declared with this type.
         (constants-neq-all Port)
         (constants-neq-all Protocol)
         (constants-neq-all Interface)))
