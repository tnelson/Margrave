(Policy
 OutboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g793
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (vlan1 exit-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in))
  (ACE-line-0-g794
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (fe0 exit-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
