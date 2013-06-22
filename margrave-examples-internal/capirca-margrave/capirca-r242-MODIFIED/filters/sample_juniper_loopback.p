; $Id: ./filters\sample_juniper_loopback.p $
; $Date: 2013/06/22 $
(Policy (uses sample_juniper_loopback) 
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
; Sample Juniper lookback filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-icmp = (accept sa sp da dp pro opt) :-
     (icmp pro)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Allow inbound traceroute from any source.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-traceroute = (accept sa sp da dp pro opt) :-
     (udp pro)
 (p33434-33534 dp)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Allow BGP requests from peers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-bgp-requests = (accept sa sp da dp pro opt) :-
     (tcp pro)
 (p179 dp)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Allow inbound replies to BGP requests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-bgp-replies = (accept sa sp da dp pro opt) :-
     (tcp pro)
 (p179 sp)
 (tcp-established opt)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Allow outbound OSPF traffic from other RFC1918 routers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-ospf = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
  (ospf pro)
)

(allow-vrrp = (accept sa sp da dp pro opt) :-
     (vrrp pro)
)

(accept-ike = (accept sa sp da dp pro opt) :-
     (udp pro)
 (p500 sp)
 (p500 dp)
)

(accept-ipsec = (accept sa sp da dp pro opt) :-
     (esp pro)
)

(accept-pim = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
  (pim pro)
)

(accept-igmp = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
  (igmp pro)
)

(accept-ssh-requests = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
  (tcp pro)
 (p22 dp)
)

(accept-ssh-replies = (accept sa sp da dp pro opt) :-
     (tcp pro)
 (p22 sp)
 (tcp-established opt)
)

(accept-snmp-requests = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
 (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (udp pro)
 (p161 dp)
)

(accept-dns-replies = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
 (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (udp pro)
 (p53 sp)
 (established opt)
)

(allow-ntp-request = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.1/32 sa) (ip10.0.0.2/32 sa)) 
 (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (udp pro)
 (p123 dp)
)

(allow-ntp-replies = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
 (or (ip10.0.0.1/32 da)  (ip10.0.0.2/32 da) ) 
  (udp pro)
 (p123 sp)
 (established opt)
)

(allow-radius-replies = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
 (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (udp pro)
 (p1812 sp)
)

(allow-tacacs-requests = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa)) 
 (or (ip10.1.0.1/32 da)  (ip10.1.0.2/32 da) ) 
  (tcp pro)
 (p49 dp)
)

(allow-tacacs-replies = (accept sa sp da dp pro opt) :-
    (or (ip10.1.0.1/32 sa) (ip10.1.0.2/32 sa)) 
 (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (tcp pro)
 (p49 sp)
 (tcp-established opt)
)

(discard-default = (deny sa sp da dp pro opt) :-
     true )

)
  (RComb (fa accept deny))
)