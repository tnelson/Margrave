; $Id: ./filters\sample_cisco_lab.p $
; $Date: 2013/06/22 $
(Policy (uses sample_cisco_lab) 
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
; Also denies access to certain public allocations.
; Ideal for some internal lab/testing types of subnets that are
; not well trusted, but allowing internal users to access.
; Apply to ingress interface (to filter traffic coming from lab)
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
; Allow tcp replies to internal hosts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(accept-tcp-replies = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (tcp pro)
 (tcp-established opt)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Deny access to rfc1918/internal.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deny-to-internal = (deny sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
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