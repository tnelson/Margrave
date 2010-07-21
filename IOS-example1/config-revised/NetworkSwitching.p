(Policy
 NetworkSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (network-switch-FastEthernet0-g844
   =
   (Forward
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (ip-209-172-108-0/ip-255-255-255-224 next-hop)
   (FastEthernet0 exit-interface))
  (network-switch-Vlan1-g845
   =
   (Forward
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-168-2-0/ip-255-255-255-0 next-hop)
   (Vlan1 exit-interface)))
 (RComb FAC)
 (PComb FAC)
 (Children))
