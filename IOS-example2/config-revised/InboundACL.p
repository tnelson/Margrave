(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g962
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
   (GigabitEthernet0/1 entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-27-g963
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
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-0-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-4-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-28-g964
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
   (GigabitEthernet0/0 entry-interface)
   (ip-10-232-4-0/ip-255-255-252-0 src-addr-in)
   (ip-10-232-0-0/ip-255-255-252-0 dest-addr-in))
  (ACE-line-29-g965
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
   (GigabitEthernet0/0 entry-interface)
   (IPAddress src-addr-in)
   (IPAddress dest-addr-in))
  (ACE-line-0-g966
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
   (Serial0/3/0:0 entry-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
