(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (network-switch-fe0-g799
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-1-1-0/ip-255-255-255-254 next-hop)
   (fe0 exit-interface))
  (network-switch-vlan1-g800
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-128-5-0/ip-255-255-255-0 next-hop)
   (vlan1 exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
