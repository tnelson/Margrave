(Policy
 DefaultPolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (default-route-g3084
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
   (hostname-Router hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-0-0/ip-255-255-252-0 src-addr-in)
   true)
  (default-route-g3085
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
   (hostname-Router hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-100-0/ip-255-255-252-0 src-addr-in)
   true)
  (default-route-g3088
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
   (hostname-Router hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-4-0/ip-255-255-252-0 src-addr-in)
   true)
  (default-route-g3089
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
   (hostname-Router hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in)
   true)
  (default-route-g1890
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
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
