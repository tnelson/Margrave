; A theory gives the vocabulary (what are the sorts? functions? etc.)
; and a set of axioms that hold.


Theory(filter,
       
       Vocab(filter, 
             Types(IPAddress, Port),
             Predicates(; whitelist(IPAddress), 
                        ; blacklist(IPAddress),

			 ; A couple of workstations 
                         ip10-1-1-1(IPAddress),
                         ip10-1-1-2(IPAddress),
			 
			 ; on a subnet
                         ip10-1-1-x(IPAddress),
			 
			 ; and a server.
                         ip10-1-20-20(IPAddress),

			 ; and some ports
			 port80(Port), port25(Port))
            ),

       ; subset(child, parent). 
       Axioms( subset(ip10-1-1-1, ip10-1-1-x),
               subset(ip10-1-1-2, ip10-1-1-x),
               atmostone(ip10-1-1-1),
               atmostone(ip10-1-1-2),
               atmostone(ip10-1-20-20),
               atmostone(port80),
               atmostone(port25)))