(Policy
 PolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (default-route-hostname-tas-g1873
   =
   (Pass
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.0.0/255.255.252.0 src-addr-in)
   true)
  (default-route-hostname-tas-g1874
   =
   (Pass
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.100.0/255.255.252.0 src-addr-in)
   true)
  (default-route-hostname-tas-g1877
   =
   (Pass
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.4.0/255.255.252.0 src-addr-in)
   true)
  (default-route-hostname-tas-g1878
   =
   (Pass
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   true)
  (hostname-tas-default-route-g1716
   =
   (Pass
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   true
   (hostname-tas hostname))
  (hostname-baz-default-route-g1716
   =
   (Pass
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   true
   (hostname-baz hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
