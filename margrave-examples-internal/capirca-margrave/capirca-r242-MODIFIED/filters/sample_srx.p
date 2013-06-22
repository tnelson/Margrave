; $Id: ./filters\sample_srx.p $
; $Date: 2013/06/22 $
(Policy (uses sample_srx) 
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
; this is a sample policy to generate Juniper SRX filter
; from zone Untrust to zone DMZ.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test-tcp = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
 (or (tcp pro) (udp pro)) 
)

(test-tcp-log = (log sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
 (or (tcp pro) (udp pro)) 
)

(test-icmp = (accept sa sp da dp pro opt) :-
    (or (ip10.0.0.0/8 da)  (ip172.16.0.0/12 da)  (ip192.168.0.0/16 da) ) 
  (icmp pro)
)

(default-deny = (deny sa sp da dp pro opt) :-
     true )

)
  (RComb (fa accept deny))
)