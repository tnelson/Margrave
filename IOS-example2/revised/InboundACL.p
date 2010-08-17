(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g8916
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-0-g8917
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/1 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-28-g8918
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-0-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-29-g8919
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-4-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-30-g8920
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g8921
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-14-g8922
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-15-g8923
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-100-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-16-g8924
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-39-g8925
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-40-g8926
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-100-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-41-g8927
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
