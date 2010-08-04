<<<<<<< HEAD:IOS-simple/initial/StaticRoute.p
(Policy
 StaticRoute
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
 StaticRoute
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
>>>>>>> aa325bc7a18c2ab96ff68487de545708fcd6d203:IOS-simple/initial/StaticRoute.p
