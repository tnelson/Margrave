(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (Router-line-21-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.6 src-addr-in)
   (209.172.108.16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (prot-TCP protocol)
   (port-80 src-port-in)
   (port-80 src-port-out)
   (= dest-port-in dest-port-out))
  (Router-line-21-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.6 src-addr-in)
   (port-80 src-port-in))
  (Router-line-22-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.6 src-addr-in)
   (209.172.108.16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (prot-TCP protocol)
   (port-21 src-port-in)
   (port-21 src-port-out)
   (= dest-port-in dest-port-out))
  (Router-line-22-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.6 src-addr-in)
   (port-21 src-port-in))
  (Router-line-23-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.6 src-addr-in)
   (209.172.108.16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (prot-TCP protocol)
   (port-3389 src-port-in)
   (port-3389 src-port-out)
   (= dest-port-in dest-port-out))
  (Router-line-23-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.6 src-addr-in)
   (port-3389 src-port-in))
  (Router-Vlan1-line25-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.0/255.255.255.0 src-addr-in)
   (209.172.108.16 src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out))
  (Router-line-20-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (Vlan1 entry-interface)
   (192.168.2.0/255.255.255.0 src-addr-in))
  (Router-default-NAT
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)
   (hostname-Router hostname)))
 (RComb FAC)
 (PComb FAC)
 (Children))
