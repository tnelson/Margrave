; $Id: ./filters\sample_packetfilter.p $
; $Date: 2013/06/22 $
(Policy (uses sample_packetfilter) 
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
; Denies all traffic to internal IPs except established tcp replies.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Optional - allow forwarding of DHCP requests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-dhcp = (accept sa sp da dp pro opt) :-
     (udp pro)
 (p67-68 dp)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Allow name resolution using honestdns.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-to-honestdns = (accept sa sp da dp pro opt) :-
    (or (ip8.8.4.4/32 da)  (ip8.8.8.8/32 da)  (ip2001.4860.4860..8844/128 da)  (ip2001.4860.4860..8888/128 da) ) 
  (udp pro)
 (p53 dp)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Deny access to rfc1918/internal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deny-to-internal = (reject sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
)

(deny-to-internal-log = (log sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
)

(test-icmp = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (icmp pro)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Deny access to specified public.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deny-to-specific_hosts = (deny sa sp da dp pro opt) :-
    (or (ip200.1.1.1/32 da)  (ip200.1.1.2/32 da)  (ip200.1.1.4/31 da) ) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Allow what's left.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(default-permit = (accept sa sp da dp pro opt) :-
     true )

)
  (RComb (fa accept deny))
)