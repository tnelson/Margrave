(Policy
 OutboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-26-g9003
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 exit-interface)
   (ip-10-232-0-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-27-g9004
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 exit-interface)
   (ip-10-232-4-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-28-g9005
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (Serial0/3/0:0 exit-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g9006
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/1 exit-interface)
   (IPAddress src-addr-in))
  (ACE-line-0-g9007
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-tas hostname)
   (GigabitEthernet0/0 exit-interface)
   (IPAddress src-addr-in))
  (ACE-line-32-g9008
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (ip-10-232-100-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-33-g9009
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-34-g9010
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (ip-10-232-104-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-35-g9011
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (Serial0/3/0:0 exit-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g9012
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-baz hostname)
   (GigabitEthernet0/0 exit-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
