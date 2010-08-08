(Policy
 OutboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g967
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (GigabitEthernet0/1 exit-interface)
   (IPAddress src-addr-in))
  (ACE-line-0-g968
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (GigabitEthernet0/0 exit-interface)
   (IPAddress src-addr-in))
  (ACE-line-24-g969
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (Serial0/3/0:0 exit-interface)
   (ip-10-232-0-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-104-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-25-g970
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (Serial0/3/0:0 exit-interface)
   (ip-10-232-4-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-100-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-26-g971
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (Serial0/3/0:0 exit-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
