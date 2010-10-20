(Policy
 OutsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (Router-line-22-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (209.172.108.16 dest-addr-in)
   (192.168.2.6 dest-addr-out)
   (prot-TCP protocol)
   (port-80 dest-port-in)
   (port-80 dest-port-out)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out))
  (Router-line-22-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (209.172.108.16 dest-addr-in)
   (port-80 dest-port-in))
  (Router-line-23-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (209.172.108.16 dest-addr-in)
   (192.168.2.6 dest-addr-out)
   (prot-TCP protocol)
   (port-21 dest-port-in)
   (port-21 dest-port-out)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out))
  (Router-line-23-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (209.172.108.16 dest-addr-in)
   (port-21 dest-port-in))
  (Router-line-24-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (209.172.108.16 dest-addr-in)
   (192.168.2.6 dest-addr-out)
   (prot-TCP protocol)
   (port-3389 dest-port-in)
   (port-3389 dest-port-out)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out))
  (Router-line-24-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (209.172.108.16 dest-addr-in)
   (port-3389 dest-port-in))
  (Router-FastEthernet0-line26-trans
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (192.168.2.0/255.255.255.0 dest-addr-out)
   (209.172.108.16 dest-addr-in)
   (= src-addr-in src-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out))
  (Router-line-21-drop
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message flags src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (FastEthernet0 entry-interface)
   (192.168.2.0/255.255.255.0 dest-addr-out)
   (209.172.108.16 dest-addr-in)
   (= dest-port-in dest-port-out))
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
