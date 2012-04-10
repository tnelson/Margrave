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
  (Router-Vlan1-line15
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
   (Vlan1 entry-interface)
   (IPAddress src-addr-in))
  (Router-Fe0-line9
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
   (Hostname-Router hostname)
   (Fe0 entry-interface)
   (IP-10.1.1.2 src-addr-in)
   (IPAddress dest-addr-in))
  (Router-Fe0-line10
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
   (Fe0 entry-interface)
   (IPAddress src-addr-in)
   (Prot-TCP protocol)
   (Port src-port-in)
   (IP-192.168.5.10 dest-addr-in)
   (Port-80 dest-port-in))
  (Router-Fe0-line11
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
   (Fe0 entry-interface)
   (IPAddress src-addr-in)
   (Prot-TCP protocol)
   (Port src-port-in)
   (IP-192.168.5.11 dest-addr-in)
   (Port-25 dest-port-in))
  (Router-Fe0-line12
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
   (Fe0 entry-interface)
   (IP-10.1.1.3 src-addr-in)
   (Prot-TCP protocol)
   (Port src-port-in)
   (IP-192.168.5.10 dest-addr-in)
   (Port-443 dest-port-in))
  (Router-Fe0-line13
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
   (Hostname-Router hostname)
   (Fe0 entry-interface)
   (IPAddress src-addr-in))
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
