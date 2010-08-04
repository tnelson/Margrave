<<<<<<< HEAD:IOS-simple/initial/InsideNAT.p
(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (default-NAT-g782
   =
   (Translate hostname entry-interface src-addr-in src-addr-out dest-addr-in dest-addr-out protocol message src-port-in src-port-out dest-port-in dest-port-out length next-hop exit-interface)
   :-
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)))
 (RComb FAC)
 (PComb FAC)
 (Children))
=======
(Policy
 InsideNAT
 uses
 IOS-vocab
 (Target)
 (Rules
  (default-NAT-g21676
   =
   (Translate
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
   (= src-addr-in src-addr-out)
   (= dest-addr-in dest-addr-out)
   (= src-port-in src-port-out)
   (= dest-port-in dest-port-out)))
 (RComb FAC)
 (PComb FAC)
 (Children))
>>>>>>> aa325bc7a18c2ab96ff68487de545708fcd6d203:IOS-simple/initial/InsideNAT.p
