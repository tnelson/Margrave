(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-0-g813
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
   (Vlan1 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in))
  (ACE-line-27-g814
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
   (FastEthernet0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (ports-0-65535 src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-80 dest-port-in))
  (ACE-line-28-g815
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
   (FastEthernet0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (ports-0-65535 src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-21 dest-port-in))
  (ACE-line-29-g816
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
   (FastEthernet0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (ports-0-65535 src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-20 dest-port-in))
  (ACE-line-30-g817
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
   (FastEthernet0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (ports-0-65535 src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (port-23 dest-port-in))
  (ACE-line-31-g818
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
   (FastEthernet0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (port-80 src-port-in)
   (ip-0-0-0-0/ip-0-0-0-0 dest-addr-in)
   (ports-0-65535 dest-port-in))
  (ACE-line-32-g819
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
   (FastEthernet0 entry-interface)
   (ip-0-0-0-0/ip-0-0-0-0 src-addr-in)
   (prot-tcp protocol)
   (ports-0-65535 src-port-in)
   (ip-209-172-108-16 dest-addr-in)
   (ports-0-65535 dest-port-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
