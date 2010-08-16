(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-22-g8220
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
  (NAT-line-22-g8221
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-80 src-port-in))
  (NAT-line-23-g8222
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
  (NAT-line-23-g8223
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-21 src-port-in))
  (NAT-line-24-g8224
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
  (NAT-line-24-g8225
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-6 src-addr-in)
   (port-3389 src-port-in))
  (NAT-line-21-ACE-line-26-g8226g8227
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
  (NAT-line-21-g8228
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 src-addr-in))
  (default-NAT-g7975
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
