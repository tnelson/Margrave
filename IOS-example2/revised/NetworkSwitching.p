(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (network-switch-Serial0/3/0:0-g12366
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.254.1.128/255.255.255.252 next-hop)
   (Serial0/3/0:0 exit-interface))
  (network-switch-GigabitEthernet0/1-g12367
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.8.0/255.255.252.0 next-hop)
   (GigabitEthernet0/1 exit-interface))
  (network-switch-GigabitEthernet0/0-g12368
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.0.0/255.255.252.0 next-hop)
   (GigabitEthernet0/0 exit-interface))
  (network-switch-GigabitEthernet0/0-g12369
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (10.232.4.0/255.255.252.0 next-hop)
   (GigabitEthernet0/0 exit-interface))
  (network-switch-Serial0/3/0:0-g12370
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.254.1.128/255.255.255.252 next-hop)
   (Serial0/3/0:0 exit-interface))
  (network-switch-GigabitEthernet0/0-g12371
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.100.0/255.255.252.0 next-hop)
   (GigabitEthernet0/0 exit-interface))
  (network-switch-GigabitEthernet0/0-g12372
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (10.232.104.0/255.255.252.0 next-hop)
   (GigabitEthernet0/0 exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
