(Policy
 PolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (next-hop-line-38-g944
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
   (ip-10-232-0-15 next-hop))
  (next-hop-line-38-g945
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
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-0-0/ip-255-255-252-0 src-addr-in))
  (next-hop-line-38-g946
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
   (ip-10-232-0-15 next-hop))
  (next-hop-line-38-g947
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
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-100-0/ip-255-255-252-0 src-addr-in))
  (next-hop-line-42-g950
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
   (ip-10-232-4-10 next-hop))
  (next-hop-line-42-g951
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
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-4-0/ip-255-255-252-0 src-addr-in))
  (next-hop-line-42-g952
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
   (ip-10-232-4-10 next-hop))
  (next-hop-line-42-g953
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
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in))
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
