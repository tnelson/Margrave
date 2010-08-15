(Policy
 StaticRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (route-line-16-g6064
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (IPAddress dest-addr-in)
   (ip-209-172-108-1 next-hop))
  (route-line-16-g6065
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (IPAddress dest-addr-in))
  (default-route-g6007 = (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface) :- true))
 (RComb FAC)
 (PComb FAC)
 (Children))
