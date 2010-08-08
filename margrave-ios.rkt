#lang racket

; Assume same dir for now (later, module path!)
(require "margrave.rkt")

(provide load-ios-policies)

; Routed Packets query for IOS parser
; tn april 2010
; updated tn july 2010

(define (load-ios-helper filename dirpath prefix suffix) 
  (let ([ polname (load-policy (build-path dirpath (string-append filename ".p")))])
    
    (printf ".")
    
    (when (or (> (string-length prefix) 0)
              (> (string-length suffix) 0))
      (mtext (string-append "RENAME " polname " " prefix polname suffix)))))
  
; Remember to start the engine before calling this. Also wrap in an exception check!
; dirpath says where to find the policies
; prefix (and suffix) are prepended (and appended) to the 
;   policy's name to avoid naming conflicts.
(define (load-ios-policies dirpath prefix suffix)
  (printf "Loading IOS policies in path: ~a with prefix: ~a and suffix: ~a ~n" dirpath prefix suffix)
    
    (load-ios-helper "InboundACL" dirpath prefix suffix)  
    (load-ios-helper "InsideNAT" dirpath prefix suffix)  
    (load-ios-helper "StaticRoute" dirpath prefix suffix)  
    (load-ios-helper "LocalSwitching" dirpath prefix suffix)  
    (load-ios-helper "PolicyRoute" dirpath prefix suffix)  
    (load-ios-helper "DefaultPolicyRoute" dirpath prefix suffix)  
    (load-ios-helper "NetworkSwitching" dirpath prefix suffix)  
    (load-ios-helper "OutsideNAT" dirpath prefix suffix)  
    (load-ios-helper "OutboundACL" dirpath prefix suffix)  
    (load-ios-helper "Encryption" dirpath prefix suffix)  
    (load-ios-helper "OutsideNAT" dirpath prefix suffix)  
    (printf "~n")
   
    
    (mtext (string-append "EXPLORE "
  (string-append prefix "InboundACL" suffix)
  ":permit(ahostname, entry-interface, src-addr-in, src-addr-in,
  dest-addr-in, dest-addr-in, protocol, message, src-port-in, src-port-in,
  dest-port-in, dest-port-in, length, next-hop, exit-interface) 
AND "
  (string-append prefix "OutsideNAT" suffix)
  ":translate(ahostname, entry-interface, src-addr-in, src-addr_,
  dest-addr-in, dest-addr_, protocol, message, src-port-in, src-port_,
  dest-port-in, dest-port_, length, next-hop, exit-interface)
AND

( "
  (string-append prefix "LocalSwitching" suffix)
  ":Forward(ahostname, entry-interface, src-addr_, src-addr_,
  dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
  dest-port_, dest-port_, length, next-hop, exit-interface)
  OR

  ( "
  (string-append prefix "LocalSwitching" suffix)
    ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
    dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
    dest-port_, dest-port_, length, next-hop, exit-interface)
    AND
    ( "
      (string-append prefix "PolicyRoute" suffix)
      ":Forward(ahostname, entry-interface, src-addr_, src-addr_,
      dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
      dest-port_, dest-port_, length, next-hop, exit-interface)
      OR
      ( "
        (string-append prefix "PolicyRoute" suffix)
        ":Route(ahostname, entry-interface, src-addr_, src-addr_,
        dest-addr_, dest-addr_, protocol, message, src-port_, src-port_,
        dest-port_, dest-port_, length, next-hop, exit-interface)
        AND "
          (string-append prefix "NetworkSwitching" suffix)
        ":Forward(ahostname, entry-interface, src-addr_,
        src-addr_, dest-addr_, dest-addr_, protocol, message,
        src-port_, src-port_, dest-port_, dest-port_, length,
        next-hop, exit-interface)
      )
      OR
      ( "
          (string-append prefix "PolicyRoute" suffix)
        ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
        dest-addr_, dest-addr_, protocol, message, src-port_,
        src-port_, dest-port_, dest-port_, length, next-hop,
        exit-interface)
        AND
        ( "
          (string-append prefix "StaticRoute" suffix)
          ":Forward(ahostname, entry-interface, src-addr_,
          src-addr_, dest-addr_, dest-addr_, protocol, message,
          src-port_, src-port_, dest-port_, dest-port_, length,
          next-hop, exit-interface)
          OR
          ( "
            (string-append prefix "StaticRoute" suffix)
            ":Route(ahostname, entry-interface, src-addr_,
            src-addr_, dest-addr_, dest-addr_, protocol, message,
            src-port_, src-port_, dest-port_, dest-port_, length,
            next-hop, exit-interface)
            AND "
            (string-append prefix "NetworkSwitching" suffix)
            ":Forward(ahostname, entry-interface, src-addr_,
            src-addr_, dest-addr_, dest-addr_, protocol, message,
            src-port_, src-port_, dest-port_, dest-port_, length,
            next-hop, exit-interface)
          )
          OR
          ( "
            (string-append prefix "StaticRoute" suffix)
            ":Pass(ahostname, entry-interface, src-addr_, src-addr_,
            dest-addr_, dest-addr_, protocol, message, src-port_,
            src-port_, dest-port_, dest-port_, length, next-hop,
            exit-interface)
            AND
            ( "
              (string-append prefix "DefaultPolicyRoute" suffix)
              ":Forward(ahostname, entry-interface,
              src-addr_, src-addr_, dest-addr_, dest-addr_,
              protocol, message, src-port_, src-port_, dest-port_,
              dest-port_, length, next-hop, exit-interface)
              OR
              ( "
              (string-append prefix "DefaultPolicyRoute" suffix)
              ":Route(ahostname, entry-interface,
                src-addr_, src-addr_, dest-addr_, dest-addr_,
                protocol, message, src-port_, src-port_, dest-port_,
                dest-port_, length, next-hop, exit-interface)
                AND "
              (string-append prefix "NetworkSwitching" suffix)
              ":Forward(ahostname, entry-interface,
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

AND "
              (string-append prefix "InsideNAT" suffix)
              ":Translate(ahostname, entry-interface, src-addr_, src-addr-out,
  dest-addr_, dest-addr-out, protocol, message, src-port_,
  src-port-out, dest-port_, dest-port-out, length, next-hop,
  exit-interface)
AND "
              (string-append prefix "OutboundACL" suffix)
  ":Permit(ahostname, entry-interface, src-addr-out, src-addr-out,
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

TUPLING
"))
  
  ; Let us use the query under the name routed-packets (w/ same prefix and suffix applied)
  (mtext (string-append "RENAME LAST " prefix "routed-packets" suffix)))



