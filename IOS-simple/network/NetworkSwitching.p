(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (network-switch-in_dmz-g3383
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
   (10.1.1.0/255.255.255.0 next-hop)
   (in_dmz exit-interface))
  (network-switch-in_lan-g3384
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
   (192.168.0.0/255.255.0.0 next-hop)
   (in_lan exit-interface))
  (network-switch-out_dmz-g3385
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
   (hostname-extern hostname)
   (10.1.1.0/255.255.255.0 next-hop)
   (out_dmz exit-interface))
  (network-switch-out_inet-g3386
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
   (hostname-extern hostname)
   (10.200.0.0/255.255.0.0 next-hop)
   (out_inet exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
