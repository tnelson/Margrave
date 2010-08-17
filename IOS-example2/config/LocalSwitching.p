(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-GigabitEthernet0/1-g11649
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.8.0/255.255.252.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/1 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/1-g11650
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.8.0/255.255.252.0 dest-addr-in))
  (local-switch-primary-Serial0/3/0:0-g11651
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (local-switch-primary-drop-Serial0/3/0:0-g11652
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in))
  (local-switch-primary-GigabitEthernet0/0-g11653
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.0.0/255.255.252.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/0-g11654
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.0.0/255.255.252.0 dest-addr-in))
  (local-switch-secondary-GigabitEthernet0/0-g11655
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (= next-hop dest-addr-out)
   (10.232.4.0/255.255.252.0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-secondary-drop-GigabitEthernet0/0-g11656
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.4.0/255.255.252.0 dest-addr-in))
  (default-route-g11602
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true)
  (local-switch-primary-Serial0/3/0:0-g11657
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Serial0/3/0:0 exit-interface))
  (local-switch-primary-drop-Serial0/3/0:0-g11658
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.254.1.128/255.255.255.252 dest-addr-in))
  (local-switch-primary-GigabitEthernet0/0-g11659
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.100.0/255.255.252.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-primary-drop-GigabitEthernet0/0-g11660
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.100.0/255.255.252.0 dest-addr-in))
  (local-switch-secondary-GigabitEthernet0/0-g11661
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (= next-hop dest-addr-out)
   (10.232.104.0/255.255.252.0 dest-addr-in)
   (IPAddress next-hop)
   (GigabitEthernet0/0 exit-interface))
  (local-switch-secondary-drop-GigabitEthernet0/0-g11662
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.104.0/255.255.252.0 dest-addr-in))
  (default-route-g11602
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
