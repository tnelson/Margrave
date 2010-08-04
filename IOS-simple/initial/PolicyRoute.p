<<<<<<< HEAD:IOS-simple/initial/PolicyRoute.p
(Policy
 PolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (default-route-g783
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
=======
(Policy
 PolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (default-route-g21677
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
>>>>>>> aa325bc7a18c2ab96ff68487de545708fcd6d203:IOS-simple/initial/PolicyRoute.p
