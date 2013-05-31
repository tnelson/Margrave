; A theory gives the vocabulary (what are the sorts? functions? etc.)
; and a set of axioms that hold.

Theory(filter,
       
       Vocab(filter, 
             Types(IPAddress, Port),
             Predicates(
			 ; A couple of workstations 
                         ip10-1-1-1(IPAddress),
                         ip10-1-1-2(IPAddress),
			 
			 ; on a subnet
                         ip10-1-1-x(IPAddress),
			 
			 ; and a server on a different subnet.
                         ip10-1-20-20(IPAddress)

			 ),
	     
  	     ; and some ports. constants in Margrave start with $. 
	     Constants($port80(Port), $port25(Port))
            ),

       ; subset constraints on predicates are: subset(child, parent). 
       Axioms( subset(ip10-1-1-1, ip10-1-1-x),
               subset(ip10-1-1-2, ip10-1-1-x),
       ; Never more than one object representing individual addresses
               atmostone(ip10-1-1-1),
               atmostone(ip10-1-1-2),
               atmostone(ip10-1-20-20)
             ))
