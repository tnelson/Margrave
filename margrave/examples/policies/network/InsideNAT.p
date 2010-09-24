(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (int-in_lan-line21-trans
   =
   (Translate
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
   (hostname-int hostname)
   (in_lan entry-interface)
   (192.168.0.0/255.255.0.0 src-addr-in)
   (10.1.1.1 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (Port src-port-out)
   (= dest-port-in dest-port-out))
  (int-line-17-drop
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
   (hostname-int hostname)
   (in_lan entry-interface)
   (192.168.0.0/255.255.0.0 src-addr-in))
  (int-default-NAT
   =
   (Translate
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
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)
   (hostname-int hostname))
  (ext-default-NAT
   =
   (Translate
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
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)
   (hostname-ext hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
