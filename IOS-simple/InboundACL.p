(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-19-g914
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
   (vlan1 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in))
  (ACE-line-14-g915
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
   (fe0 entry-interface)
   (ip-173-194-33-104 src-addr-in)
   (ip-0-0-0-0/ip-0-0-0-0 dest-addr-in))
  (ACE-line-15-g916
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
   (fe0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (ports-0-65535 src-port-in)
   (ip-192-168-5-10 dest-addr-in)
   (port-80 dest-port-in))
  (ACE-line-16-g917
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
   (fe0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (ports-0-65535 src-port-in)
   (ip-192-168-5-11 dest-addr-in)
   (port-25 dest-port-in))
  (ACE-line-17-g918
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
   (fe0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
