(Policy
 StaticRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (route-line-23-g9034
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in)
   (ip-10-254-1-130 next-hop))
  (route-line-23-g9035
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (route-line-24-g9036
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in)
   (ip-10-254-1-130 next-hop))
  (route-line-24-g9037
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (default-route-g8891
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true)
  (route-line-26-g9038
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (IPAddress dest-addr-in)
   (ip-10-254-1-129 next-hop))
  (route-line-26-g9039
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (IPAddress dest-addr-in))
  (route-line-27-g9040
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in)
   (ip-10-254-1-129 next-hop))
  (route-line-27-g9041
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in))
  (route-line-28-g9042
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in)
   (ip-10-254-1-129 next-hop))
  (route-line-28-g9043
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (route-line-29-g9044
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-192-168-1-0/ip-255-255-255-0 dest-addr-in)
   (ip-10-254-1-129 next-hop))
  (route-line-29-g9045
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (ip-192-168-1-0/ip-255-255-255-0 dest-addr-in))
  (default-route-g8891
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
