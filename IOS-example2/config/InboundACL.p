(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-29-g12255
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.0.0/255.255.252.0 src-addr-in)
   (10.232.4.0/255.255.252.0 dest-addr-in))
  (ACE-line-30-g12256
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.4.0/255.255.252.0 src-addr-in)
   (10.232.0.0/255.255.252.0 dest-addr-in))
  (ACE-line-31-g12257
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g12258
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/1 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-0-g12259
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-14-g12260
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   (10.232.100.0/255.255.252.0 dest-addr-in))
  (ACE-line-15-g12261
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.100.0/255.255.252.0 src-addr-in)
   (10.232.104.0/255.255.252.0 dest-addr-in))
  (ACE-line-16-g12262
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-39-g12263
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.104.0/255.255.252.0 src-addr-in)
   (10.232.100.0/255.255.252.0 dest-addr-in))
  (ACE-line-40-g12264
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (10.232.100.0/255.255.252.0 src-addr-in)
   (10.232.104.0/255.255.252.0 dest-addr-in))
  (ACE-line-41-g12265
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g12266
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 entry-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
