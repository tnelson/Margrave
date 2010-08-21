(Policy
 PolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (next-hop-hostname-tas-line-39-g1798
   =
   (Route
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
   (10.232.0.15 next-hop))
  (next-hop-hostname-tas-line-39-g1799
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
   (10.232.0.0/255.255.252.0 src-addr-in))
  (next-hop-hostname-tas-line-39-g1800
   =
   (Route
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
   (10.232.0.15 next-hop))
  (next-hop-hostname-tas-line-39-g1801
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
   (10.232.100.0/255.255.252.0 src-addr-in))
  (next-hop-hostname-tas-line-43-g1804
   =
   (Route
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
   (10.232.4.10 next-hop))
  (next-hop-hostname-tas-line-43-g1805
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
   (10.232.4.0/255.255.252.0 src-addr-in))
  (next-hop-hostname-tas-line-43-g1806
   =
   (Route
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
   (10.232.4.10 next-hop))
  (next-hop-hostname-tas-line-43-g1807
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
   (10.232.104.0/255.255.252.0 src-addr-in))
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
