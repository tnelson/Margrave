(Policy
 InboundACL
 uses
 IOS-vocab
 (Target)
 (Rules
  (ACE-line-12-g3352
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-intern hostname)
   (in_lan entry-interface)
   (192.168.4.0/255.255.255.0 src-addr-in)
   (10.1.1.3 dest-addr-in))
  (ACE-line-13-g3353
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-intern hostname)
   (in_lan entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (10.1.1.3 dest-addr-in)
   (port-25 dest-port-in))
  (ACE-line-14-g3354
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-intern hostname)
   (in_lan entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (IPAddress dest-addr-in)
   (port-80 dest-port-in))
  (ACE-line-15-g3355
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-intern hostname)
   (in_lan entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-0-g3356
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-intern hostname)
   (in_dmz entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-12-g3357
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_inet entry-interface)
   (10.200.200.200 src-addr-in))
  (ACE-line-13-g3358
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_inet entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (10.1.1.3 dest-addr-in)
   (port-25 dest-port-in))
  (ACE-line-14-g3359
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_inet entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (10.1.1.4 dest-addr-in)
   (port-80 dest-port-in))
  (ACE-line-15-g3360
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_inet entry-interface)
   (IPAddress src-addr-in))
  (ACE-line-17-g3361
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in)
   (10.200.200.200 dest-addr-in))
  (ACE-line-18-g3362
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (IPAddress dest-addr-in)
   (port-23 dest-port-in))
  (ACE-line-19-g3363
   =
   (Permit
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_dmz entry-interface)
   (192.168.1.2 src-addr-in)
   (prot-tcp protocol)
   (Port src-port-in)
   (IPAddress dest-addr-in)
   (port-80 dest-port-in))
  (ACE-line-20-g3364
   =
   (Deny
    hostname
    entry-interface
    src-addr-in
    src-addr-out
    dest-addr-in
    dest-addr-out
    protocol
    message
    flags
    src-port-in
    src-port-out
    dest-port-in
    dest-port-out
    length
    next-hop
    exit-interface)
   :-
   (hostname-extern hostname)
   (out_dmz entry-interface)
   (IPAddress src-addr-in)))
 (RComb FAC)
 (PComb FAC)
 (Children))
