(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (NAT-line-17-NAT-line-17-g3372g3373
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
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out)
   (Port dest-port-in))
  (NAT-line-17-g3374
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
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (Port dest-port-in))
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
