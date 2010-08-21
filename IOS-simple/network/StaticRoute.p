(Policy
 StaticRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (route-line-19-g3387
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
   (hostname-intern hostname)
   (IPAddress dest-addr-in)
   (= next-hop dest-addr-out)
   (in_dmz exit-interface)
   (IPAddress next-hop))
  (route-line-19-g3388
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
   (hostname-intern hostname)
   (IPAddress dest-addr-in))
  (hostname-intern-default-route-g3290
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
   (hostname-intern hostname))
  (hostname-extern-default-route-g3290
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
   (hostname-extern hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
