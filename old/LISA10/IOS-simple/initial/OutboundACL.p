(Policy
 OutboundACL
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
  (Router-vlan1-line0
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
   (vlan1 exit-interface)
   (IPAddress src-addr-in))
  (Router-fe0-line0
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
   (fe0 exit-interface)
   (IPAddress src-addr-in)))
 (RComb (fa permit deny translate route forward drop pass advertise encrypt)))
