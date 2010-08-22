(Policy
 StaticRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (route-hostname-int-line-19-g13182
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (IPAddress dest-addr-in)
   (= next-hop dest-addr-out)
   (in_dmz exit-interface)
   (IPAddress next-hop))
  (route-hostname-int-line-19-g13183
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (IPAddress dest-addr-in))
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