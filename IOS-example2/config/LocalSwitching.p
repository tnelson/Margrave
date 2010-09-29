(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (baz-Serial0/3/0:0-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (baz-Serial0/3/0:0-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in))
  (baz-GigabitEthernet0/0-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.100.0/255.255.252.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (baz-GigabitEthernet0/0-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.100.0/255.255.252.0 dest-addr-in))
  (baz-GigabitEthernet0/0-secondary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (= next-hop dest-addr-out)
   (10.232.104.0/255.255.252.0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (baz-GigabitEthernet0/0-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.104.0/255.255.252.0 dest-addr-in))
  (baz-default-route
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
