(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (Router-FastEthernet0-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (209.172.108.0/255.255.255.224 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (FastEthernet0 exit-interface))
  (Router-FastEthernet0-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (209.172.108.0/255.255.255.224 dest-addr-in))
  (Router-Vlan1-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (192.168.2.0/255.255.255.0 dest-addr-in)
   (= next-hop dest-addr-out)
   (IPAddress next-hop)
   (Vlan1 exit-interface))
  (Router-Vlan1-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (192.168.2.0/255.255.255.0 dest-addr-in))
  (Router-default-route
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
