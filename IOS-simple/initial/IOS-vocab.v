(PolicyVocab
 IOS-vocab
 (Types
  (Hostname : hostname-Router)
  (Interface : vlan1 fe0)
  (IPAddress ip-192-128-5-0/ip-255-255-255-0 ip-10-1-1-0/ip-255-255-255-254 ip-192-168-5-11 ip-192-168-5-10 ip-10-1-1-2)
  (Protocol : prot-ICMP prot-TCP prot-UDP)
  (Port port-25 port-80)
  (ICMPMessage : icmp-echo icmp-echo-reply icmp-time-exceeded icmp-unreachable)
  (Length :))
 (Decisions Permit Deny Translate Route Forward Drop Pass Advertise Encrypt)
 (Predicates)
 (ReqVariables
  (hostname : Hostname)
  (entry-interface : Interface)
  (src-addr-in : IPAddress)
  (src-addr-out : IPAddress)
  (dest-addr-in : IPAddress)
  (dest-addr-out : IPAddress)
  (protocol : Protocol)
  (message : ICMPMessage)
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
  (abstract ICMPMessage)
  (abstract Protocol)
  (disjoint-all Hostname)
  (disjoint-all Interface)
  (disjoint-all IPAddress)
  (disjoint-all Protocol)
  (disjoint-all Port)
  (disjoint-all ICMPMessage)
  (disjoint-all Length)
  (atmostone-all Interface)
  (atmostone ip-10-1-1-2)
  (atmostone ip-192-168-5-10)
  (atmostone ip-192-168-5-11)
  (disjoint-all IPAddress)
  (atmostone-all Protocol)
  (atmostone port-80)
  (atmostone port-25)
  (disjoint-all Port)
  (atmostone icmp-echo)
  (atmostone icmp-echo-reply)
  (atmostone-all Length)
  (nonempty Hostname)
  (nonempty Interface)
  (nonempty IPAddress)
  (nonempty Protocol)
  (nonempty Port)
  (nonempty ICMPMessage)
  (nonempty Length)))
