#lang racket/gui

(require "visualize.rkt" "netgraph.rkt" "controls.rkt" "modelgraph.rkt" "apply-model.rkt")
(require "../../margrave.rkt" xml "../../margrave-xml.rkt")

(define icon-accept (make-object bitmap% "../images/icon_accept.png"))
(define icon-modify (make-object bitmap% "../images/icon_modify.png"))
(define icon-deny (make-object bitmap% "../images/icon_deny.png"))

; Standard gui toolkit setup stuff
(define window (new frame%
                    [label "FWP App"]
                    [width 1000]
                    [height 800]))

(define canvas (new editor-canvas% [parent window]))

; Using my custom subclass of pasteboard
(define pb (new fwpboard%))

(send canvas set-editor pb)

(send pb insert (make-object image-snip% (make-object bitmap% "../images/key.png")) 550 10)

(start-margrave-engine (build-path 'up 'up))
(load-policy (build-path "tests" "inboundacl_fw1.p"))
(load-policy (build-path "tests" "inboundacl_fw2.p"))
(load-policy (build-path "tests" "inboundacl_fw1_new.p"))
(load-policy (build-path "tests" "inboundacl_fw2_new.p"))
(load-policy (build-path "tests" "inboundnat_fw1.p"))
(load-policy (build-path "tests" "inboundnat_fw2.p"))

(mtext "EXPLORE inboundacl_fw2:Deny(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) OR
inboundnat_fw2:Translate(interf, ipsrc, ipdest, portsrc, portdest, pro, tempnatip) AND
inboundacl_fw1_new:Deny(interminterface, tempnatsrc, ipdest, portsrc, portdest, pro, tempnatsrc)
AND managerpc(ipsrc)")
; AND fw2int(interface) AND fw1dmz(interminterface) AND otherports(portsrc) AND port80(portdest)
;AND tcp(pro) AND outsideips(ipdest)
;(mtext "")
(mtext "GET ONE 0")

(define mr (document-element (mtext "GET NEXT 0")))
(stop-margrave-engine)

(define mod (if (equal? (get-attribute-value mr 'type) "model") mr #f)) 

; My stuff
; Make a netgraph
(define myng (new netgraph%))
; Create some positional nodes
(define s1 (new pos-netgraph-node% [name "Web Server"] [type 'server] [x 440] [y 120]))
(define s2 (new pos-netgraph-node% [name "Mail Server"] [type 'server] [x 440] [y 500]))
(define f1 (new pos-netgraph-node% [name "FW lan-dmz"] [type 'firewall] [x 320] [y 320]))
(define f2 (new pos-netgraph-node% [name "FW dmz-inet"] [type 'firewall] [x 620] [y 320]))
(define m1 (new pos-netgraph-node% [name "Internet"] [type 'group] [x 800] [y 320]))
(define h1 (new pos-netgraph-node% [name "Empl 1"] [type 'host] [x 20] [y 20]))
(define h2 (new pos-netgraph-node% [name "Empl 2"] [type 'host] [x 20] [y 140]))
(define h3 (new pos-netgraph-node% [name "Empl 3"] [type 'host] [x 20] [y 260]))
(define h4 (new pos-netgraph-node% [name "Contr 1"] [type 'host] [x 20] [y 380]))
(define h5 (new pos-netgraph-node% [name "Contr 2"] [type 'host] [x 20] [y 500]))
(define h6 (new pos-netgraph-node% [name "Manager"] [type 'host] [x 20] [y 620]))


;(define subng (new netgraph%))
;(define sn1 (new pos-netgraph-node% [name "Sub1"] [x 50] [y 150]))
;(define sn2 (new pos-netgraph-node% [name "Sub2"] [x 250] [y 50]))
;(send subng add-node! sn1)
;(send subng add-node! sn2)
;(send subng add-edge! sn1 sn2)
;(send n1 set-subgraph! subng)

(send myng add-node! s1)
(send myng add-node! s2)
(send myng add-node! f1)
(send myng add-node! f2)
(send myng add-node! m1)
(send myng add-node! h1)
(send myng add-node! h2)
(send myng add-node! h3)
(send myng add-node! h4)
(send myng add-node! h5)
(send myng add-node! h6)

(send myng add-edge! f1 h1) (send myng add-edge! h1 f1)
(send myng add-edge! f1 h2) (send myng add-edge! h2 f1)
(send myng add-edge! f1 h3) (send myng add-edge! h3 f1)
(send myng add-edge! f1 h4) (send myng add-edge! h4 f1)
(send myng add-edge! f1 h5) (send myng add-edge! h5 f1)
(send myng add-edge! f1 h6) (send myng add-edge! h6 f1)

(send myng add-edge! f1 s1) (send myng add-edge! s1 f1)
(send myng add-edge! f1 s2) (send myng add-edge! s2 f1)

(send myng add-edge! f2 s1) (send myng add-edge! s1 f2)
(send myng add-edge! f2 s2) (send myng add-edge! s2 f2)

(send myng add-edge! f2 m1) (send myng add-edge! m1 f2)

(visualize (apply-model/pos myng mod) pb)

(send window show #t)
