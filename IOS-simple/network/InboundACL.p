(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ext-out_inet-line12
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_inet entry-interface)
   (10.200.200.200 src-addr-in))
  (ext-out_inet-line13
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_inet entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (10.1.1.3 dest-addr-in)
   (port-25 dest-port-in))
  (ext-out_inet-line14
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_inet entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (10.1.1.4 dest-addr-in)
   (port-80 dest-port-in))
  (ext-out_inet-line15
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_inet entry-interface)
   (IPAddress src-addr-in))
  (ext-out_dmz-line17
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in)
   (10.200.200.200 dest-addr-in))
  (ext-out_dmz-line18
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (IPAddress dest-addr-in)
   (port-23 dest-port-in))
  (ext-out_dmz-line19
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_dmz entry-interface)
   (192.168.1.2 src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (IPAddress dest-addr-in)
   (port-80 dest-port-in))
  (ext-out_dmz-line20
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
