; $Id: ./filters\sample_speedway.p $
; $Date: 2013/06/22 $
(Policy (uses sample_speedway) 
  (Variables 
    (da IPAddress)
    (sa IPAddress)
    (sp Port)
    (dp Port)
    (pro Protocol)
    (opt Options)
  )
  (Rules 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Sample policy for Speedway Iptables.
; Speedway generates iptables output suitable for loading
; using the iptables-restore command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(base-allow-est-in = (accept sa sp da dp pro opt) :-
    (established opt)
)

(base-allow-icmp-in = (accept sa sp da dp pro opt) :-
     (icmp pro)
)

(base-traceroute-in = (accept sa sp da dp pro opt) :-
     (udp pro)
 (p33434-33534 sp)
 (p1024-65535 dp)
)

(base-allow-ssh-in = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
  (tcp pro)
 (p22 dp)
)

; Sample output filter policy for Speedway Iptables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Allow all loopback communications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(base-allow-est-out = (accept sa sp da dp pro opt) :-
    (established opt)
)

(base-allow-dns-query-out = (accept sa sp da dp pro opt) :-
     (udp pro)
 (p53 dp)
)

(base-allow-icmp-out = (accept sa sp da dp pro opt) :-
     (icmp pro)
)

(base-traceroute-out = (accept sa sp da dp pro opt) :-
     (udp pro)
 (p1024-65535 sp)
 (p33434-33534 dp)
)

(base-allow-ssh-out = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (tcp pro)
 (p22 dp)
)

; Sample forwarding filter policy for Speedway Iptables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(base-forwarding-deny = (reject sa sp da dp pro opt) :-
     true )

)
  (RComb (fa accept deny))
)