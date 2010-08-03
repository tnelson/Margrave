(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (default-NAT-g782
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)))
 (RComb FAC)
 (PComb FAC)
 (Children))
