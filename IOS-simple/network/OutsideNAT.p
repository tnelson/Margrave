(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (int-in_dmz-line21-trans
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
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out)
   (Port dest-port-in))
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
   (in_dmz entry-interface)
   (192.168.0.0/255.255.0.0 dest-addr-out)
   (10.1.1.1 dest-addr-in)
   (Port dest-port-in))
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
