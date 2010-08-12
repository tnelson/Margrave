#lang racket

(require "load-netgraph.rkt" "visualize.rkt" "../../margrave.rkt")

(start-margrave-engine (build-path 'up 'up))
(load-policy (build-path "tests" "inboundacl_fw1.p"))
(load-policy (build-path "tests" "inboundacl_fw2.p"))
(load-policy (build-path "tests" "inboundacl_fw1_new.p"))
(load-policy (build-path "tests" "inboundacl_fw2_new.p"))
(load-policy (build-path "tests" "inboundnat_fw1.p"))
(load-policy (build-path "tests" "inboundnat_fw2.p"))

(define myng (load-topology "topotest.rkt"))

(define myquery
  "EXPLORE (inboundacl_fw2:Deny(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) OR
(inboundnat_fw2:Translate(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) AND
inboundacl_fw1_new:Deny(interminterface, tempnatsrc, ipdest, portsrc, portdest, pro, tempnatsrc)))
AND fw2int(interf) AND fw1dmz(interminterface) AND managerpc(ipsrc) AND otherports(portsrc) AND port80(portdest) AND tcp(pro) AND outsideips(ipdest) TUPLING
IDBOUTPUT inboundacl_fw2:Deny(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip),
inboundnat_fw2:Translate(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip),
inboundacl_fw1_new:Deny(interminterface, tempnatsrc, ipdest, portsrc, portdest, pro, tempnatsrc)")

(visualize myng myquery)