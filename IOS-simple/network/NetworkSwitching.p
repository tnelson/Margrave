(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (ext-out_dmz-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.1.1.0/255.255.255.0 next-hop)
   (out_dmz exit-interface))
  (ext-out_inet-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (10.200.0.0/255.255.0.0 next-hop)
   (out_inet exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
