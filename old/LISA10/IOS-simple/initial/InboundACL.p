(Policy
 InboundACL
 uses
 IOS-vocab
 (Variables
  (hostname Hostname)
  (entry-interface Interf-real)
  (src-addr-in IPAddress)
  (src-addr-out IPAddress)
  (dest-addr-in IPAddress)
  (dest-addr-out IPAddress)
  (protocol Protocol-any)
  (message ICMPMessage)
  (flags TCPFlags)
  (src-port-in Port)
  (src-port-out Port)
  (dest-port-in Port)
  (dest-port-out Port)
  (length Length)
  (next-hop IPAddress)
  (exit-interface Interface))
 (Rules
  (Router-vlan1-line14
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
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (Hostname-Router hostname)
   (vlan1 entry-interface)
   (IPAddress src-addr-in))
  (Router-fe0-line9
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
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (Hostname-Router hostname)
   (fe0 entry-interface)
   (10.1.1.2 src-addr-in)
   (IPAddress dest-addr-in))
  (Router-fe0-line10
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
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (Hostname-Router hostname)
   (fe0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (192.168.5.10 dest-addr-in)
   (port-80 dest-port-in))
  (Router-fe0-line11
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
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (Hostname-Router hostname)
   (fe0 entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (192.168.5.11 dest-addr-in)
   (port-25 dest-port-in))
  (Router-fe0-line12
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
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (Hostname-Router hostname)
   (fe0 entry-interface)
   (IPAddress src-addr-in)))
 (RComb (fa permit deny translate route forward drop pass advertise encrypt)))
