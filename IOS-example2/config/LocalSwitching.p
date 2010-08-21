(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-hostname-tas-Serial0/3/0:0-g1763
   =
   (Forward
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
   (10.254.1.128/255.255.255.252 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (local-switch-primary-drop-hostname-tas-Serial0/3/0:0-g1764
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
   (10.254.1.128/255.255.255.252 dest-addr-in))
  (local-switch-primary-hostname-tas-GigabitEthernet0/0-g1765
   =
   (Forward
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
   (10.232.0.0/255.255.252.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-primary-drop-hostname-tas-GigabitEthernet0/0-g1766
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
   (10.232.0.0/255.255.252.0 dest-addr-in))
  (local-switch-primary-hostname-tas-GigabitEthernet0/1-g1767
   =
   (Forward
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
   (10.232.8.0/255.255.252.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/1 exit-interface))
  (local-switch-primary-drop-hostname-tas-GigabitEthernet0/1-g1768
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
   (10.232.8.0/255.255.252.0 dest-addr-in))
  (local-switch-secondary-hostname-tas-GigabitEthernet0/0-g1769
   =
   (Forward
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
   (= next-hop dest-addr-out)
   (10.232.4.0/255.255.252.0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-secondary-drop-hostname-tas-GigabitEthernet0/0-g1770
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
   (10.232.4.0/255.255.252.0 dest-addr-in))
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
  (local-switch-primary-hostname-baz-Serial0/3/0:0-g1771
   =
   (Forward
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
   (hostname-baz hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (local-switch-primary-drop-hostname-baz-Serial0/3/0:0-g1772
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
   (hostname-baz hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in))
  (local-switch-primary-hostname-baz-GigabitEthernet0/0-g1773
   =
   (Forward
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
   (hostname-baz hostname)
   (10.232.100.0/255.255.252.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-primary-drop-hostname-baz-GigabitEthernet0/0-g1774
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
   (hostname-baz hostname)
   (10.232.100.0/255.255.252.0 dest-addr-in))
  (local-switch-secondary-hostname-baz-GigabitEthernet0/0-g1775
   =
   (Forward
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
   (hostname-baz hostname)
   (= next-hop dest-addr-out)
   (10.232.104.0/255.255.252.0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-secondary-drop-hostname-baz-GigabitEthernet0/0-g1776
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
   (hostname-baz hostname)
   (10.232.104.0/255.255.252.0 dest-addr-in))
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
