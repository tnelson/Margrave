(Policy
 StaticRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (route-line-21-g984
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
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in)
   (ip-10-254-1-130 next-hop))
  (route-line-21-g985
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
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (route-line-22-g986
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
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in)
   (ip-10-254-1-130 next-hop))
  (route-line-22-g987
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
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
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
