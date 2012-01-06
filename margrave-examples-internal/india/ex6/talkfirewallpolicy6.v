
(Theory talkfirewallpolicy6
        (Vocab talkfirewallpolicy6
               
               (Constants
                ('employeepc IPAddress)
                ('managerpc IPAddress)
                ('contractorpc IPAddress)
                ('webserver IPAddress)
                ('mailserver IPAddress)
                ('fw2static_con IPAddress)        
                ('fw2static_emp IPAddress)
                ('fw2static_mgr IPAddress)
                
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
                (IPAddress > OutsideIPs BlacklistedIPs)        
                (Port > OtherPorts)
                Protocol              
                Interface)
                              
               (Predicates ))       
        (Axioms 
         ; A port that isn't one of the constants must be in other ports.
         (abstract Port)
         
         ; These constants are distinct:
         (constants-neq-all IPAddress)
         (constants-neq-all Port)
         (constants-neq-all Protocol)
         (constants-neq-all Interface)))


