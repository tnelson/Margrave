(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-fe0-g23727
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-1-1-0/ip-255-255-255-254 dest-addr-in)
   (IPAddress next-hop)
   (fe0 exit-interface))
  (local-switch-primary-drop-fe0-g23728
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-1-1-0/ip-255-255-255-254 dest-addr-in))
  (local-switch-primary-vlan1-g23729
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-128-5-0/ip-255-255-255-0 dest-addr-in)
   (IPAddress next-hop)
   (vlan1 exit-interface))
  (local-switch-primary-drop-vlan1-g23730
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-128-5-0/ip-255-255-255-0 dest-addr-in))
  (default-route-g23714 = (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface) :- true))
 (RComb FAC)
 (PComb FAC)
 (Children))
