(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-FastEthernet0-g20063
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-209-172-108-0/ip-255-255-255-224 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (FastEthernet0 exit-interface))
  (local-switch-primary-drop-FastEthernet0-g20064
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-209-172-108-0/ip-255-255-255-224 dest-addr-in))
  (local-switch-primary-Vlan1-g20065
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-168-2-0/ip-255-255-255-0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Vlan1 exit-interface))
  (local-switch-primary-drop-Vlan1-g20066
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-168-2-0/ip-255-255-255-0 dest-addr-in))
  (default-route-g19976 = (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface) :- true))
 (RComb FAC)
 (PComb FAC)
 (Children))
