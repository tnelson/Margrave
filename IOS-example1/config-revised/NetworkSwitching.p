(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (Router-FastEthernet0-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (209.172.108.0/255.255.255.224 next-hop)
   (FastEthernet0 exit-interface))
  (Router-Vlan1-primary
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (192.168.2.0/255.255.255.0 next-hop)
   (Vlan1 exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
