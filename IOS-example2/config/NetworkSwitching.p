(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (network-switch-Serial0/3/0:0-g692
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
   (ip-10-254-1-128/ip-255-255-255-252 next-hop)
   (Serial0/3/0:0 exit-interface))
  (network-switch-GigabitEthernet0/0-g693
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
   (ip-10-232-0-0/ip-255-255-252-0 next-hop)
   (GigabitEthernet0/0 exit-interface))
  (network-switch-GigabitEthernet0/1-g694
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
   (ip-10-232-8-0/ip-255-255-252-0 next-hop)
   (GigabitEthernet0/1 exit-interface))
  (network-switch-GigabitEthernet0/0-g695
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
   (ip-10-232-4-0/ip-255-255-252-0 next-hop)
   (GigabitEthernet0/0 exit-interface))
  (network-switch-Serial0/3/0:0-g696
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
   (ip-10-254-1-128/ip-255-255-255-252 next-hop)
   (Serial0/3/0:0 exit-interface))
  (network-switch-GigabitEthernet0/0-g697
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
   (ip-10-232-100-0/ip-255-255-252-0 next-hop)
   (GigabitEthernet0/0 exit-interface))
  (network-switch-GigabitEthernet0/0-g698
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
   (ip-10-232-104-0/ip-255-255-252-0 next-hop)
   (GigabitEthernet0/0 exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
