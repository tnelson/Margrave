#lang racket/gui

(require "visualize.rkt" "netgraph.rkt" "controls.rkt" "modelgraph.rkt" "../../margrave.rkt" "../../margrave-xml.rkt" xml)

(define icon-accept (make-object bitmap% "../images/icon_accept.png"))
(define icon-modify (make-object bitmap% "../images/icon_modify.png"))
(define icon-deny (make-object bitmap% "../images/icon_deny.png"))

; Standard gui toolkit setup stuff
(define window (new frame%
                    [label "FWP App"]
                    [width 800]
                    [height 600]))

(define canvas (new editor-canvas% [parent window]))

; Using my custom subclass of pasteboard
(define pb (new fwpboard%))

(send canvas set-editor pb)

(send pb insert (make-object image-snip% (make-object bitmap% "../images/key.png")) 550 10)

; My stuff
; Make a netgraph
(define myng (new netgraph%))
; Create some positional nodes
(define n1 (new pos-netgraph-node% [name "Some Host"] [x 20] [y 30]))
(define n2 (new pos-netgraph-node% [name "Web Server"] [x 300] [y 30]))
(define n3 (new pos-netgraph-node% [name "Something"] [x 120] [y 240]))


(define subng (new netgraph%))
(define sn1 (new pos-netgraph-node% [name "Sub1"] [x 50] [y 150]))
(define sn2 (new pos-netgraph-node% [name "Sub2"] [x 250] [y 50]))
(send subng add-node! sn1)
(send subng add-node! sn2)
(send subng add-edge! sn1 sn2)
(send n1 set-subgraph! subng)

(send myng add-node! n1)
(send myng add-node! n2)
(send myng add-node! n3)
(send myng add-edge! n1 n2)n
(send myng add-edge! n2 n3)

(visualize (apply-model/pos myng #f) pb)

(send window show #t)

(start-margrave-engine (build-path 'up 'up))

(load-policy (build-path 'up 'up "tests" "fwex1.p"))
(mtext "info")
(mtext "rename fwex1 firewall1")
(mtext "EXPLORE firewall1:accept(ipsrc, ipdest, portsrc, portdest, pro) IDBOUTPUT firewall1:accept(ipsrc, ipdest, portsrc, portdest, pro) TUPLING")
(mtext "GET ONE 0")

(stop-margrave-engine)
