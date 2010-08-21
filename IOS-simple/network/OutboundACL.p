(Policy
 OutboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-hostname-int-line-0-g13160
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_lan exit-interface)
   (IPAddress src-addr-in))
  (ACE-hostname-int-line-0-g13161
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_dmz exit-interface)
   (IPAddress src-addr-in))
  (ACE-hostname-ext-line-0-g13162
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_dmz exit-interface)
   (IPAddress src-addr-in))
  (ACE-hostname-ext-line-0-g13163
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_inet exit-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
