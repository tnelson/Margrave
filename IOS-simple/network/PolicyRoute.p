(Policy
 PolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (hostname-int-default-route-g13122
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true
   (hostname-int hostname))
  (hostname-ext-default-route-g13122
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true
   (hostname-ext hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
