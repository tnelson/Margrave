(Policy
 OutboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (baz-GigabitEthernet0/0-line0
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 exit-interface)
   (IPAddress src-addr-in))
  (baz-Serial0/3/0:0-line31
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.100.0/255.255.252.0 src-addr-in)
   (10.232.4.0/255.255.252.0 dest-addr-in))
  (baz-Serial0/3/0:0-line32
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   (10.232.0.0/255.255.252.0 dest-addr-in))
  (baz-Serial0/3/0:0-line33
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   (10.232.4.0/255.255.252.0 dest-addr-in))
  (baz-Serial0/3/0:0-line34
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
