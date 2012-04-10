(Policy
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
  (Router-Vlan1-line0
   =
   (permit
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
   (Vlan1 exit-interface)
   (IPAddress src-addr-in))
  (Router-Fe0-line0
   =
   (permit
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
   (Fe0 exit-interface)
   (IPAddress src-addr-in))
  (ruleNeverdeny
   =
   (deny
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
   false)
  (ruleNeverdrop
   =
   (drop
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
   false))
 (RComb (fa permit deny translate route forward drop pass advertise encrypt)))
