(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-hostname-tas-Serial0/3/0:0-g1838
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
  (local-switch-primary-drop-hostname-tas-Serial0/3/0:0-g1839
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
  (local-switch-primary-hostname-tas-GigabitEthernet0/0-g1840
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
  (local-switch-primary-drop-hostname-tas-GigabitEthernet0/0-g1841
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
  (local-switch-primary-hostname-tas-GigabitEthernet0/1-g1842
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
  (local-switch-primary-drop-hostname-tas-GigabitEthernet0/1-g1843
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
  (local-switch-secondary-hostname-tas-GigabitEthernet0/0-g1844
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
  (local-switch-secondary-drop-hostname-tas-GigabitEthernet0/0-g1845
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
  (local-switch-primary-hostname-baz-Serial0/3/0:0-g1846
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
  (local-switch-primary-drop-hostname-baz-Serial0/3/0:0-g1847
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
  (local-switch-primary-hostname-baz-GigabitEthernet0/0-g1848
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
  (local-switch-primary-drop-hostname-baz-GigabitEthernet0/0-g1849
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
  (local-switch-secondary-hostname-baz-GigabitEthernet0/0-g1850
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
  (local-switch-secondary-drop-hostname-baz-GigabitEthernet0/0-g1851
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
