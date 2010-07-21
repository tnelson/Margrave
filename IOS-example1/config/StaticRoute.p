(Policy
 StaticRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (route-line-16-g1862
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
   (ip-0-0-0-0/ip-0-0-0-0 dest-addr-in)
   (ip-209-172-108-1 next-hop))
  (route-line-16-g1863
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
   (ip-0-0-0-0/ip-0-0-0-0 dest-addr-in))
  (default-route-g1825
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
