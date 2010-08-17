(Policy
 PolicyRoute
 uses
 IOS-vocab
 (Target)
 (Rules
  (next-hop-line-40-g11684
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.0.0/255.255.252.0 src-addr-in)
   (10.232.0.15 next-hop))
  (next-hop-line-40-g11685
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.0.0/255.255.252.0 src-addr-in))
  (next-hop-line-40-g11686
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.100.0/255.255.252.0 src-addr-in)
   (10.232.0.15 next-hop))
  (next-hop-line-40-g11687
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.100.0/255.255.252.0 src-addr-in))
  (next-hop-line-44-g11690
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.4.0/255.255.252.0 src-addr-in)
   (10.232.4.10 next-hop))
  (next-hop-line-44-g11691
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.4.0/255.255.252.0 src-addr-in))
  (next-hop-line-44-g11692
   =
   (Route hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   (10.232.4.10 next-hop))
  (next-hop-line-44-g11693
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.104.0/255.255.252.0 src-addr-in))
  (default-route-g11602
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true)
  (default-route-g11602
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
