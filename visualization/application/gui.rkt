#lang racket/gui

(require "visualize.rkt" "netgraph.rkt" "controls.rkt")

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
(define n1 (new pos-node% [name "Some Host"] [x 20] [y 30]))
(define n2 (new pos-node% [name "Web Server"] [x 300] [y 30] [result (list icon-accept icon-modify)]))
(define n3 (new pos-node% [name "Something"] [x 120] [y 240]))

; Add them to the netgraph (using mutation)
(send myng add-node! n1)
(send myng add-node! n2)
(send myng add-node! n3)
(send myng add-edge! n1 n2 #t)
(send myng add-edge! n2 n3 #t)

; DO IT
(visualize pb myng)

; When this function is written, it will look more like this
;(visualize pb (model-graph myng))

; Without mutation
;(visualize pb (send (send (send myng add-node n1) add-node n2) add-edge n1 n2))

(send window show #t)
