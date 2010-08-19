(Policy
 DefaultPolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (hostname-intern-default-route-g801
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
   (hostname-intern hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
