(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-21-NAT-line-21-g3166g3167
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-intern hostname)
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out)
   (Port dest-port-in))
  (NAT-line-21-g3168
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-intern hostname)
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (Port dest-port-in))
  (hostname-intern-default-NAT-g2658
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)
   (hostname-intern hostname))
  (hostname-extern-default-NAT-g2658
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)
   (hostname-extern hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
