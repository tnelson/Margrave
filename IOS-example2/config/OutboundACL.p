(Policy
 OutboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g12267
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 exit-interface)
   (IPAddress src-addr-in))
  (ACE-line-0-g12268
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/1 exit-interface)
   (IPAddress src-addr-in))
  (ACE-line-26-g12269
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.0.0/255.255.252.0 src-addr-in)
   (10.232.104.0/255.255.252.0 dest-addr-in))
  (ACE-line-27-g12270
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.4.0/255.255.252.0 src-addr-in)
   (10.232.100.0/255.255.252.0 dest-addr-in))
  (ACE-line-28-g12271
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 exit-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g12272
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 exit-interface)
   (IPAddress src-addr-in))
  (ACE-line-32-g12273
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.100.0/255.255.252.0 src-addr-in)
   (10.232.4.0/255.255.252.0 dest-addr-in))
  (ACE-line-33-g12274
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   (10.232.0.0/255.255.252.0 dest-addr-in))
  (ACE-line-34-g12275
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   (10.232.4.0/255.255.252.0 dest-addr-in))
  (ACE-line-35-g12276
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
