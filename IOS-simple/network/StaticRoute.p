(Policy
 StaticRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (ext-default-route
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
