(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-in_dmz-g3169
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-intern hostname)
   (10.1.1.0/255.255.255.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (in_dmz exit-interface))
  (local-switch-primary-drop-in_dmz-g3170
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-intern hostname)
   (10.1.1.0/255.255.255.0 dest-addr-in))
  (local-switch-primary-in_lan-g3171
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-intern hostname)
   (192.168.0.0/255.255.0.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (in_lan exit-interface))
  (local-switch-primary-drop-in_lan-g3172
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-intern hostname)
   (192.168.0.0/255.255.0.0 dest-addr-in))
  (hostname-intern-default-route-g2659
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true
   (hostname-intern hostname))
  (hostname-extern-default-route-g2659
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true
   (hostname-extern hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
