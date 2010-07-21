(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-22-g1847
   =
   (Translate
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
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
  (NAT-line-22-g1848
   =
   (Drop
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (port-80 dest-port-in))
  (NAT-line-23-g1849
   =
   (Translate
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
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
  (NAT-line-23-g1850
   =
   (Drop
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (port-21 dest-port-in))
  (NAT-line-24-g1851
   =
   (Translate
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
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
  (NAT-line-24-g1852
   =
   (Drop
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-209-172-108-16 dest-addr-in)
   (port-3389 dest-port-in))
  (NAT-line-21-NAT-line-21-g1853g1854
   =
   (Translate
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 dest-addr-out)
   (ip-209-172-108-16 dest-addr-in)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out))
  (NAT-line-21-g1855
   =
   (Drop
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (ip-192-168-2-0/ip-255-255-255-0 dest-addr-out)
   (ip-209-172-108-16 dest-addr-in))
  (default-NAT-g1824
   =
   (Translate
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)))
 (RComb FAC)
 (PComb FAC)
 (Children))
