#lang racket

; Assume same dir for now (later, module path!)
(require "margrave.rkt")

(provide load-ios-policies)

; Routed Packets query for IOS parser
; tn april 2010
; updated tn july 2010

; Remember to start the engine before calling this.
(define (load-ios-policies dirpath)
  (printf "Loading IOS policies in path: ~a ~n" dirpath)
 
  (printf ".")
  (load-policy (build-path dirpath "InboundACL.p"))  
  (printf ".")
  (load-policy (build-path dirpath "InsideNAT.p"))
  (printf ".")
  (load-policy (build-path dirpath "StaticRoute.p"))
  (printf ".")
  (load-policy (build-path dirpath "LocalSwitching.p"))
  (printf ".")
  (load-policy (build-path dirpath "PolicyRoute.p"))
  (printf ".")
  (load-policy (build-path dirpath "DefaultPolicyRoute.p"))
  (printf ".")
  (load-policy (build-path dirpath "NetworkSwitching.p"))
  (printf ".")
  (load-policy (build-path dirpath "OutsideNAT.p"))
  (printf ".")
  (load-policy (build-path dirpath "OutboundACL.p"))  
  (printf ".")
  (load-policy (build-path dirpath "Encryption.p"))
  (printf ".")
  (load-policy (build-path dirpath "OutsideNAT.p"))
  (printf ".~n")
  
  
  
  (m "
EXPLORE 
  InboundACL:permit(ahostname, entry-interface, src-addr-in, src-addr-in,
  dest-addr-in, dest-addr-in, protocol, message, src-port-in, src-port-in,
  dest-port-in, dest-port-in, length, next-hop, exit-interface) 
AND
  OutsideNAT:translate(ahostname, entry-interface, src-addr-in, src-addr_,
  dest-addr-in, dest-addr_, protocol, message, src-port-in, src-port_,
  dest-port-in, dest-port_, length, next-hop, exit-interface)
AND

(
  LocalSwitching:Forward(ahostname, entry-interface, src-addr_, src-addr_,
  dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
  dest-port_, dest-port_, length, next-hop, exit-interface)
  OR

  (
    LocalSwitching:Pass(ahostname, entry-interface, src-addr_, src-addr_,
    dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
    dest-port_, dest-port_, length, next-hop, exit-interface)
    AND
    (
      PolicyRoute:Forward(ahostname, entry-interface, src-addr_, src-addr_,
      dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
      dest-port_, dest-port_, length, next-hop, exit-interface)
      OR
      (
        PolicyRoute:Route(ahostname, entry-interface, src-addr_, src-addr_,
        dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
        dest-port_, dest-port_, length, next-hop, exit-interface)
        AND
        NetworkSwitching:Forward(ahostname, entry-interface, src-addr_,
        src-addr_, dest-addr_, dest-addr_, protocol, message,
        src-port_, src-port_, dest-port_, dest-port_, length,
        next-hop, exit-interface)
      )
      OR
      (
        PolicyRoute:Pass(ahostname, entry-interface, src-addr_, src-addr_,
        dest-addr_, dest-addr_, protocol, message, src-port_,
        src-port_, dest-port_, dest-port_, length, next-hop,
        exit-interface)
        AND
        (
          StaticRoute:Forward(ahostname, entry-interface, src-addr_,
          src-addr_, dest-addr_, dest-addr_, protocol, message,
          src-port_, src-port_, dest-port_, dest-port_, length,
          next-hop, exit-interface)
          OR
          (
            StaticRoute:Route(ahostname, entry-interface, src-addr_,
            src-addr_, dest-addr_, dest-addr_, protocol, message,
            src-port_, src-port_, dest-port_, dest-port_, length,
            next-hop, exit-interface)
            AND
            NetworkSwitching:Forward(ahostname, entry-interface, src-addr_,
            src-addr_, dest-addr_, dest-addr_, protocol, message,
            src-port_, src-port_, dest-port_, dest-port_, length,
            next-hop, exit-interface)
          )
          OR
          (
            StaticRoute:Pass(ahostname, entry-interface, src-addr_, src-addr_,
            dest-addr_, dest-addr_, protocol, message, src-port_,
            src-port_, dest-port_, dest-port_, length, next-hop,
            exit-interface)
            AND
            (
              DefaultPolicyRoute:Forward(ahostname, entry-interface,
              src-addr_, src-addr_, dest-addr_, dest-addr_,
              protocol, message, src-port_, src-port_, dest-port_,
              dest-port_, length, next-hop, exit-interface)
              OR
              (
                DefaultPolicyRoute:Route(ahostname, entry-interface,
                src-addr_, src-addr_, dest-addr_, dest-addr_,
                protocol, message, src-port_, src-port_, dest-port_,
                dest-port_, length, next-hop, exit-interface)
                AND
                NetworkSwitching:Forward(ahostname, entry-interface,
                src-addr_, src-addr_, dest-addr_, dest-addr_,
                protocol, message, src-port_, src-port_, dest-port_,
                dest-port_, length, next-hop, exit-interface)
              )
            )
          )
        )
      )
    )
  )
)

AND
  InsideNAT:Translate(ahostname, entry-interface, src-addr_, src-addr-out,
  dest-addr_, dest-addr-out, protocol, message, src-port_,
  src-port-out, dest-port_, dest-port-out, length, next-hop,
  exit-interface)
AND
  OutboundACL:Permit(ahostname, entry-interface, src-addr-out, src-addr-out,
  dest-addr-out, dest-addr-out, protocol, message, src-port-out,
  src-port-out, dest-port-out, dest-port-out, length, next-hop,
  exit-interface)

PUBLISH ahostname, entry-interface, 
        src-addr-in, src-addr_, src-addr-out, 
        dest-addr-in, dest-addr_, dest-addr-out, 
        protocol, message,
        src-port-in, src-port_, src-port-out, 
        dest-port-in, dest-port_, dest-port-out, 
        length, next-hop, exit-interface
IS POSSIBLE?
TUPLING
")
  ; IS POSSIBLE? ---> don't print any results, just load the query
  
  ; Let us use the query under the name routed-packets
  (m "RENAME LAST routed-packets"))



