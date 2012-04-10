(Theory
 IOS-vocab
 (Vocab
  IOS-vocab
  (Types
   (Hostname > Hostname-Router)
   (Interface > Interf-drop Interf-real)
   (Interf-real > Vlan1 Fe0)
   (IPAddress
    >
    IP-192.128.5.0/255.255.255.0
    IP-10.1.1.0/255.255.255.254
    IP-10.1.1.3
    IP-192.168.5.11
    IP-192.168.5.10
    IP-10.1.1.2)
   IP-192.128.5.0/255.255.255.0
   IP-10.1.1.0/255.255.255.254
   IP-10.1.1.3
   IP-192.168.5.11
   IP-192.168.5.10
   IP-10.1.1.2
   (Protocol-any > Prot-ICMP Prot-TCP Prot-UDP)
   (Port > Port-443 Port-25 Port-80)
   Port-443
   Port-25
   Port-80
   (ICMPMessage
    >
    ICMP-echo
    ICMP-echo-reply
    ICMP-time-exceeded
    ICMP-unreachable)
   (TCPFlags > SYN ACK FIN PSH URG RST)
   Length)
  (Predicates))
 (Axioms
  (abstract Hostname)
  (abstract Interface)
  (abstract Interf-real)
  (abstract ICMPMessage)
  (abstract Protocol-any)
  (atmostone-all Hostname)
  (atmostone-all Interf-real)
  (atmostone Interf-drop)
  (atmostone IP-10.1.1.2)
  (atmostone IP-192.168.5.10)
  (atmostone IP-192.168.5.11)
  (atmostone IP-10.1.1.3)
  (atmostone-all Protocol-any)
  (atmostone Port-80)
  (atmostone Port-25)
  (atmostone Port-443)
  (atmostone ICMP-echo)
  (atmostone ICMP-echo-reply)
  (atmostone-all Length)
  (nonempty Hostname)
  (nonempty Interface)
  (nonempty IPAddress)
  (nonempty Protocol-any)
  (nonempty Port)
  (nonempty ICMPMessage)
  (nonempty TCPFlags)
  (nonempty Length)))
