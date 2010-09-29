(Policy
 OutboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (Router-FastEthernet0-line0
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 exit-interface)
   (IPAddress src-addr-in))
  (Router-Vlan1-line0
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 exit-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
