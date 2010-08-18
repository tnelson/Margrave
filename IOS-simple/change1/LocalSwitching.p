(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-fe0-g5488
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (10.1.1.0/255.255.255.254 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (fe0 exit-interface))
  (local-switch-primary-drop-fe0-g5489
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (10.1.1.0/255.255.255.254 dest-addr-in))
  (local-switch-primary-vlan1-g5490
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (192.128.5.0/255.255.255.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (vlan1 exit-interface))
  (local-switch-primary-drop-vlan1-g5491
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (192.128.5.0/255.255.255.0 dest-addr-in))
  (hostname-Router-default-route-g5442
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true
   (hostname-Router hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
