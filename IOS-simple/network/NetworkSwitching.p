(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (network-switch-hostname-int-in_dmz-g13178
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (10.1.1.0/255.255.255.0 next-hop)
   (in_dmz exit-interface))
  (network-switch-hostname-int-in_lan-g13179
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (192.168.0.0/255.255.0.0 next-hop)
   (in_lan exit-interface))
  (network-switch-hostname-ext-out_inet-g13180
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.200.0.0/255.255.0.0 next-hop)
   (out_inet exit-interface))
  (network-switch-hostname-ext-out_dmz-g13181
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.1.1.0/255.255.255.0 next-hop)
   (out_dmz exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
