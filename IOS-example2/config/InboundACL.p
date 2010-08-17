(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g8991
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-0-g8992
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/1 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-29-g8993
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-0-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-30-g8994
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-4-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-31-g8995
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g8996
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-14-g8997
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-15-g8998
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-100-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-16-g8999
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-39-g9000
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-40-g9001
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-100-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-41-g9002
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
