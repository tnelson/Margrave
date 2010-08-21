(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-17-ACE-line-21-g3369g3370
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
   (hostname-intern hostname)
   (in_lan entry-interface)
   (192.168.0.0/255.255.0.0 src-addr-in)
   (10.1.1.1 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (Port src-port-out)
   (= dest-port-in dest-port-out))
  (NAT-line-17-g3371
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
   (in_lan entry-interface)
   (192.168.0.0/255.255.0.0 src-addr-in))
  (hostname-intern-default-NAT-g3289
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
   (hostname-intern hostname))
  (hostname-extern-default-NAT-g3289
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
   (hostname-extern hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
