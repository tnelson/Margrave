(Vocab
 IOS-vocab
 (Types
  (Hostname > Hostname-Router)
  (Interface > Interf-drop Interf-real)
  (Interf-real > vlan1 fe0)
  (IPAddress
   >
   192.128.5.0/255.255.255.0
   10.1.1.0/255.255.255.254
   192.168.5.11
   192.168.5.10
   10.1.1.2)
  192.128.5.0/255.255.255.0
  10.1.1.0/255.255.255.254
  192.168.5.11
  192.168.5.10
  10.1.1.2
  (Protocol-any : prot-ICMP prot-TCP prot-UDP)
  (Port > port-25 port-80)
  port-25
  port-80
  (ICMPMessage > ICMP-echo ICMP-echo-reply ICMP-time-exceeded ICMP-unreachable)
  (TCPFlags > SYN ACK FIN PSH URG RST)
  Length)
 (Predicates)
 (Constraints
  (abstract Hostname)
  (abstract Interface)
  (abstract interf-real)
  (abstract ICMPMessage)
  (abstract Protocol-any)
  (atmostone-all Hostname)
  (atmostone-all interf-real)
  (atmostone interf-drop)
  (atmostone 10.1.1.2)
  (atmostone 192.168.5.10)
  (atmostone 192.168.5.11)
  (atmostone-all Protocol-any)
  (atmostone port-80)
  (atmostone port-25)
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
