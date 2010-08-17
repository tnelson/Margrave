(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-22-g9314
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (ip-209-172-108-16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (prot-TCP protocol)
   (port-80 src-port-in)
   (port-80 src-port-out)
   (= dest-port-in dest-port-out))
  (NAT-line-22-g9315
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-80 src-port-in))
  (NAT-line-23-g9316
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (ip-209-172-108-16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (prot-TCP protocol)
   (port-21 src-port-in)
   (port-21 src-port-out)
   (= dest-port-in dest-port-out))
  (NAT-line-23-g9317
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-21 src-port-in))
  (NAT-line-24-g9318
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (ip-209-172-108-16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (prot-TCP protocol)
   (port-3389 src-port-in)
   (port-3389 src-port-out)
   (= dest-port-in dest-port-out))
  (NAT-line-24-g9319
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-3389 src-port-in))
  (NAT-line-21-ACE-line-26-g9320g9321
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 src-addr-in)
   (ip-209-172-108-16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out))
  (NAT-line-21-g9322
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 src-addr-in))
  (default-NAT-g8890
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)))
 (RComb FAC)
 (PComb FAC)
 (Children))
