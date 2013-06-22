; $Id: ./filters\sample_multitarget.p $
; $Date: 2013/06/22 $
(Policy (uses sample_multitarget) 
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
; this is a sample edge input filter that generates
; multiple output formats.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; this is a sample edge input filter with a very very very long and
; multi-line comment that
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; also has multiple entries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deny-from-bogons = (deny sa sp da dp pro opt) :-
    (or (ip0.0.0.0/8 sa) (ip192.0.0.0/24 sa) (ip192.0.2.0/24 sa) (ip192.18.0.0/15 sa) (ip198.51.100.0/24 sa) (ip203.0.113.0/24 sa) (ip224.0.0.0/3 sa) (ip2001.db8../32 sa) (ip3ffe../16 sa) (ip5f00../8 sa) (ipff00../8 sa)) 
)

(deny-from-reserved = (deny sa sp da dp pro opt) :-
    (or (ip0.0.0.0/8 sa) (ip10.0.0.0/8 sa) (ip127.0.0.0/8 sa) (ip169.254.0.0/16 sa) (ip172.16.0.0/12 sa) (ip192.168.0.0/16 sa) (ip224.0.0.0/3 sa) (ip../3 sa) (ip4000../2 sa) (ip8000../1 sa)) 
)

(deny-to-rfc1918 = (deny sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
)

(permit-mail-services = (accept sa sp da dp pro opt) :-
     (ip200.1.1.4/31 da) 
  (tcp pro)
 (or(p25 dp)(p465 dp)(p587 dp)(p995 dp)) 
)

(permit-web-services = (accept sa sp da dp pro opt) :-
    (or (ip200.1.1.1/32 da)  (ip200.1.1.2/32 da) ) 
  (tcp pro)
 (or(p80 dp)(p443 dp)) 
)

(permit-tcp-established = (accept sa sp da dp pro opt) :-
    (or (ip200.1.1.1/32 da)  (ip200.1.1.2/31 da)  (ip200.1.1.4/31 da) ) 
  (tcp pro)
 (tcp-established opt)
)

(permit-udp-established = (accept sa sp da dp pro opt) :-
    (or (ip200.1.1.1/32 da)  (ip200.1.1.2/31 da)  (ip200.1.1.4/31 da) ) 
  (udp pro)
 (p1024-65535 sp)
)

(default-deny = (deny sa sp da dp pro opt) :-
     true )

; this is a sample output filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deny-to-bad-destinations = (deny sa sp da dp pro opt) :-
    (or (ip0.0.0.0/8 da)  (ip10.0.0.0/8 da)  (ip127.0.0.0/8 da)  (ip169.254.0.0/16 da)  (ip172.16.0.0/12 da)  (ip192.0.0.0/24 da)  (ip192.0.2.0/24 da)  (ip192.18.0.0/15 da)  (ip192.168.0.0/16 da)  (ip198.51.100.0/24 da)  (ip203.0.113.0/24 da)  (ip224.0.0.0/3 da)  (ip../3 da)  (ip2001.db8../32 da)  (ip3ffe../16 da)  (ip4000../2 da)  (ip8000../1 da) ) 
)

(default-accept = (accept sa sp da dp pro opt) :-
     true )

)
  (RComb (fa accept deny))
)