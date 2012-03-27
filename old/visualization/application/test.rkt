#lang racket/base

(require "../../margrave.rkt" xml "../../margrave-xml.rkt")

(start-margrave-engine (build-path 'up 'up))
(load-policy (build-path "tests" "inboundacl_fw1.p"))
(load-policy (build-path "tests" "inboundacl_fw2.p"))
(load-policy (build-path "tests" "inboundacl_fw1_new.p"))
(load-policy (build-path "tests" "inboundacl_fw2_new.p"))
(load-policy (build-path "tests" "inboundnat_fw1.p"))
(load-policy (build-path "tests" "inboundnat_fw2.p"))

;This works, but tupling would be nice

(mtext "EXPLORE (inboundacl_fw2:Deny(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) OR
(inboundnat_fw2:Translate(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) AND
inboundacl_fw1_new:Deny(interminterface, tempnatsrc, ipdest, portsrc, portdest, pro, tempnatsrc)))
AND fw2int(interf) AND fw1dmz(interminterface) AND managerpc(ipsrc) AND otherports(portsrc) AND port80(portdest) AND tcp(pro) AND outsideips(ipdest) CEILING 11")

(mtext "GET ONE 0")

;And this doesn't give me anything

(mtext "EXPLORE (inboundacl_fw2:Deny(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) OR
(inboundnat_fw2:Translate(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) AND
inboundacl_fw1_new:Deny(interminterface, tempnatsrc, ipdest, portsrc, portdest, pro, tempnatsrc)))
AND fw2int(interf) AND fw1dmz(interminterface) AND managerpc(ipsrc) AND otherports(portsrc) AND port80(portdest) AND tcp(pro) AND outsideips(ipdest) TUPLING")

(mtext "GET ONE 0")