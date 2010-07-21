(PolicyVocab
 IOS-vocab
 (Types
  (Hostname : hostname-Router)
  (Interface : Vlan1 FastEthernet0)
  (IPAddress
   :
   ip-N/A
   (ip-0-0-0-0/ip-0-0-0-0
    (ip-209-172-108-0/ip-255-255-255-224 ip-209-172-108-1 ip-209-172-108-16)
    (ip-192-168-2-0/ip-255-255-255-0 ip-192-168-2-6)))
  (Protocol : prot-ICMP prot-TCP prot-UDP)
  (Port : port-N/A (ports-0-65535 port-3389 port-23 port-20 port-21 port-80))
  (ICMPMessage
   :
   icmp-N/A
   (icmp-any icmp-echo icmp-echo-reply icmp-time-exceeded icmp-unreachable))
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
  (atmostone ip-N/A)
  (atmostone ip-192-168-2-6)
  (disjoint-all ip-192-168-2-0/ip-255-255-255-0)
  (atmostone ip-209-172-108-16)
  (atmostone ip-209-172-108-1)
  (disjoint-all ip-209-172-108-0/ip-255-255-255-224)
  (disjoint-all ip-0-0-0-0/ip-0-0-0-0)
  (atmostone-all Protocol)
  (atmostone port-80)
  (atmostone port-21)
  (atmostone port-20)
  (atmostone port-23)
  (atmostone port-3389)
  (disjoint-all ports-0-65535)
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
