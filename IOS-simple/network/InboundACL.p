(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-hostname-int-line-12-g13147
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_lan entry-interface)
   (192.168.4.0/255.255.255.0 src-addr-in)
   (10.1.1.3 dest-addr-in))
  (ACE-hostname-int-line-13-g13148
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_lan entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (10.1.1.3 dest-addr-in)
   (port-25 dest-port-in))
  (ACE-hostname-int-line-14-g13149
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_lan entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (IPAddress dest-addr-in)
   (port-80 dest-port-in))
  (ACE-hostname-int-line-15-g13150
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_lan entry-interface)
   (IPAddress src-addr-in))
  (ACE-hostname-int-line-0-g13151
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-int hostname)
   (in_dmz entry-interface)
   (IPAddress src-addr-in))
  (ACE-hostname-ext-line-17-g13152
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in)
   (10.200.200.200 dest-addr-in))
  (ACE-hostname-ext-line-18-g13153
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
  (ACE-hostname-ext-line-19-g13154
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
  (ACE-hostname-ext-line-20-g13155
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in))
  (ACE-hostname-ext-line-12-g13156
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_inet entry-interface)
   (10.200.200.200 src-addr-in))
  (ACE-hostname-ext-line-13-g13157
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
  (ACE-hostname-ext-line-14-g13158
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
  (ACE-hostname-ext-line-15-g13159
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-ext hostname)
   (out_inet entry-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
