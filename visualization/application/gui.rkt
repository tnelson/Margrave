#lang racket/gui

(require "visualize.rkt" "netgraph.rkt" "controls.rkt" "modelgraph.rkt")

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

; Add them to the netgraph (using mutation)
(send myng add-node! n1)
(send myng add-node! n2)
(send myng add-node! n3)
(send myng add-edge! n1 n2)
(send myng add-edge! n2 n3)

(visualize (apply-model myng #f) pb)

(send window show #t)