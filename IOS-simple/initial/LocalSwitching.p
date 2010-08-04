<<<<<<< HEAD:IOS-simple/initial/LocalSwitching.p
(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-fe0-g795
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-1-1-0/ip-255-255-255-254 dest-addr-in)
   (ip-N/A next-hop)
   (fe0 exit-interface))
  (local-switch-primary-drop-fe0-g796
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-10-1-1-0/ip-255-255-255-254 dest-addr-in))
  (local-switch-primary-vlan1-g797
   =
   (Forward hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-128-5-0/ip-255-255-255-0 dest-addr-in)
   (ip-N/A next-hop)
   (vlan1 exit-interface))
  (local-switch-primary-drop-vlan1-g798
   =
   (Drop hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (hostname-Router hostname)
   (ip-192-128-5-0/ip-255-255-255-0 dest-addr-in))
  (default-route-g783
   =
   (Pass hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
=======
(Policy
 LocalSwitching
 uses
 IOS-vocab
 (Target)
 (Rules
  (local-switch-primary-fe0-g21717
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
   (ip-10-1-1-0/ip-255-255-255-254 dest-addr-in)
   (IPAddress next-hop)
   (fe0 exit-interface))
  (local-switch-primary-drop-fe0-g21718
   =
   (Drop
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
   (ip-10-1-1-0/ip-255-255-255-254 dest-addr-in))
  (local-switch-primary-vlan1-g21719
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
   (ip-192-128-5-0/ip-255-255-255-0 dest-addr-in)
   (IPAddress next-hop)
   (vlan1 exit-interface))
  (local-switch-primary-drop-vlan1-g21720
   =
   (Drop
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
   (ip-192-128-5-0/ip-255-255-255-0 dest-addr-in))
  (default-route-g21677
   =
   (Pass
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
   true))
 (RComb FAC)
 (PComb FAC)
 (Children))
>>>>>>> aa325bc7a18c2ab96ff68487de545708fcd6d203:IOS-simple/initial/LocalSwitching.p
