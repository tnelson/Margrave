(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-Serial0/3/0:0-g926
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
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-254-1-128/ip-255-255-255-252 dest-addr-in)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (local-switch-primary-drop-Serial0/3/0:0-g927
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
   (ip-10-254-1-128/ip-255-255-255-252 dest-addr-in))
  (local-switch-primary-GigabitEthernet0/0-g928
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
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/0-g929
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
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in))
  (local-switch-primary-GigabitEthernet0/1-g930
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
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-232-8-0/ip-255-255-252-0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/1 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/1-g931
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
   (ip-10-232-8-0/ip-255-255-252-0 dest-addr-in))
  (local-switch-secondary-GigabitEthernet0/0-g932
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
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-secondary-drop-GigabitEthernet0/0-g933
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
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (default-route-g911
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
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
