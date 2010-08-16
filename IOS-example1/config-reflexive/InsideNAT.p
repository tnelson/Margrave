(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-23-g11830
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
  (NAT-line-23-g11831
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-80 src-port-in))
  (NAT-line-24-g11832
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
  (NAT-line-24-g11833
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-21 src-port-in))
  (NAT-line-25-g11834
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
  (NAT-line-25-g11835
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-3389 src-port-in))
  (NAT-line-22-ACE-line-27-g11836g11837
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
  (NAT-line-22-g11838
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 src-addr-in))
  (default-NAT-g11795
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
