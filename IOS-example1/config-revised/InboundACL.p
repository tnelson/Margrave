(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g9340
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-27-g9341
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-80 dest-port-in))
  (ACE-line-28-g9342
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-21 dest-port-in))
  (ACE-line-29-g9343
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-20 dest-port-in))
  (ACE-line-30-g9344
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-23 dest-port-in))
  (ACE-line-31-g9345
   =
   (Permit hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (port-80 src-port-in)
   (IPAddress dest-addr-in)
   (Port dest-port-in))
  (ACE-line-32-g9346
   =
   (Deny hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (Port dest-port-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
