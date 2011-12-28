(PolicyVocab
 IOS-vocab
 (Types
  (Hostname : hostname-Router)
  (Interface : interf-drop (interf-real fe0 vlan1))
  (IPAddress 10.1.1.0/255.255.255.254 192.128.5.0/255.255.255.0 192.168.5.11 192.168.5.10 10.1.1.2)
  (Protocol-any : prot-ICMP prot-TCP prot-UDP)
  (Port port-25 port-80)
  (ICMPMessage : icmp-echo icmp-echo-reply icmp-time-exceeded icmp-unreachable)
  (TCPFlags : SYN ACK FIN PSH URG RST)
  (Length :))
 (Decisions Permit Deny Translate Route Forward Drop Pass Advertise Encrypt)
 (Predicates)
 (ReqVariables
  (hostname : Hostname)
  (entry-interface : interf-real)
  (src-addr-in : IPAddress)
  (src-addr-out : IPAddress)
  (dest-addr-in : IPAddress)
  (dest-addr-out : IPAddress)
  (protocol : Protocol-any)
  (message : ICMPMessage)
  (flags : TCPFlags)
  (src-port-in : Port)
  (src-port-out : Port)
  (dest-port-in : Port)
  (dest-port-out : Port)
  (length : Length)
  (next-hop : IPAddress)
  (exit-interface : Interface))
 (OthVariables)
 (Constraints
  (abstract Hostname)
  (abstract Interface)
  (abstract interf-real)
  (abstract ICMPMessage)
  (abstract Protocol-any)
  (atmostone-all Hostname)
  (disjoint-all Hostname)
  (disjoint-all Interface)
  (disjoint-all interf-real)
  (disjoint-all IPAddress)
  (disjoint-all Protocol-any)
  (disjoint-all Port)
  (disjoint-all ICMPMessage)
  (disjoint-all Length)
  (atmostone-all interf-real)
  (atmostone interf-drop)
  (atmostone 10.1.1.2)
  (atmostone 192.168.5.10)
  (atmostone 192.168.5.11)
  (disjoint-all IPAddress)
  (atmostone-all Protocol-any)
  (atmostone port-80)
  (atmostone port-25)
  (disjoint-all Port)
  (atmostone icmp-echo)
  (atmostone icmp-echo-reply)
  (atmostone-all Length)
  (nonempty Hostname)
  (nonempty Interface)
  (nonempty IPAddress)
  (nonempty Protocol-any)
  (nonempty Port)
  (nonempty ICMPMessage)
  (nonempty TCPFlags)
  (nonempty Length)))
