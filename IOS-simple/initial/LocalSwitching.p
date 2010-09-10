(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (Router-fe0-primary
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
   (hostname-Router hostname)
   (10.1.1.0/255.255.255.254 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (fe0 exit-interface))
  (Router-fe0-drop
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
   (hostname-Router hostname)
   (10.1.1.0/255.255.255.254 dest-addr-in))
  (Router-vlan1-primary
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
   (hostname-Router hostname)
   (192.128.5.0/255.255.255.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (vlan1 exit-interface))
  (Router-vlan1-drop
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
   (hostname-Router hostname)
   (192.128.5.0/255.255.255.0 dest-addr-in))
  (Router-default-route
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
   (hostname-Router hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
