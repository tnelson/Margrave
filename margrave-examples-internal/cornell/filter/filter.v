; A theory gives the vocabulary (what are the sorts? functions? etc.)
; and a set of axioms that hold.

Theory(filter,       
       Vocab(filter, 
             Types(IPAddress, Port),
	     Constants($port80(Port), $port25(Port),
                       $10.1.1.1(IPAddress), $10.1.1.2(IPAddress),
		       $10.1.20.20(IPAddress))
            ),
       Axioms(constants-neq-all(IPAddress),
              constants-neq-all(Port)))
	      
