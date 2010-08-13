#lang racket

(require "load-netgraph.rkt" "visualize.rkt" "../../margrave.rkt")

; User is responsible for loading margrave and the relevent policy files.
(start-margrave-engine (build-path 'up 'up))
(load-policy (build-path "tests" "inboundacl_fw1.p"))
(load-policy (build-path "tests" "inboundacl_fw2.p"))
(load-policy (build-path "tests" "inboundacl_fw1_new.p"))
(load-policy (build-path "tests" "inboundacl_fw2_new.p"))
(load-policy (build-path "tests" "inboundnat_fw1.p"))
(load-policy (build-path "tests" "inboundnat_fw2.p"))

; Load a netgraph from a file
(define myng (load-topology "topotest.rkt"))

; Define the keywords from the policy vocabulary.
; This way we know the variable names associated with the ip source and destination
(define mykws (make-hash))
(hash-set! mykws 'ipsrc "ipsrc")
(hash-set! mykws 'ipdest "ipdest")

; This is the query which will be run

; Query should be a TUPLING query and it should have an IDBOUTPUT inclusion for each policy file
; TUPLING is required because non-tupling queries look different and the application does not parse them.
; IDBOUPUT is required because that's the only way to know the decisions made by individual policies.
(define myquery
  "EXPLORE (inboundacl_fw2:Deny(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) OR
(inboundnat_fw2:Translate(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) AND
inboundacl_fw1_new:Deny(interminterface, tempnatsrc, ipdest, portsrc, portdest, pro, tempnatsrc)))
AND fw2int(interf) AND fw1dmz(interminterface) AND managerpc(ipsrc) AND otherports(portsrc) AND port80(portdest) AND tcp(pro) AND outsideips(ipdest) TUPLING
IDBOUTPUT inboundacl_fw2:Deny(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip),
inboundnat_fw2:Translate(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip),
inboundacl_fw1_new:Deny(interminterface, tempnatsrc, ipdest, portsrc, portdest, pro, tempnatsrc)")

(visualize myng myquery mykws)
