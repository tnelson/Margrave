(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-22-g6084
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (ip-192-168-2-6 dest-addr-out)
   (prot-TCP protocol)
   (port-80 dest-port-in)
   (port-80 dest-port-out)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out))
  (NAT-line-22-g6085
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (port-80 dest-port-in))
  (NAT-line-23-g6086
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (ip-192-168-2-6 dest-addr-out)
   (prot-TCP protocol)
   (port-21 dest-port-in)
   (port-21 dest-port-out)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out))
  (NAT-line-23-g6087
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (port-21 dest-port-in))
  (NAT-line-24-g6088
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (ip-192-168-2-6 dest-addr-out)
   (prot-TCP protocol)
   (port-3389 dest-port-in)
   (port-3389 dest-port-out)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out))
  (NAT-line-24-g6089
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (port-3389 dest-port-in))
  (NAT-line-21-NAT-line-21-g6090g6091
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 dest-addr-out)
   (ip-209-172-108-16 dest-addr-in)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out))
  (NAT-line-21-g6092
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 dest-addr-out)
   (ip-209-172-108-16 dest-addr-in))
  (default-NAT-g6006
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
