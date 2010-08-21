(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-17-NAT-hostname-int-line-17-g13167g13168
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out)
   (Port dest-port-in))
  (NAT-hostname-int-line-17-g13169
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (Port dest-port-in))
  (hostname-int-default-NAT-g13121
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)
   (hostname-int hostname))
  (hostname-ext-default-NAT-g13121
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)
   (hostname-ext hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
