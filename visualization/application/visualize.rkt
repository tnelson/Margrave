#lang racket/gui

(provide visualize)

(require "netgraph.rkt" "modelgraph.rkt" "controls.rkt")

; Adds a node to the pasteboard.
; It creates a snip and positions it based on the node's location
(define (place-node pb node)
  (let
      ((newnode (new entity-snip%
                     [updatef (lambda (x y) 
                                (send node set-x! x)
                                (send node set-y! y))]
                     [name (send node get-name)]
                     [icons (map (lambda (n) (send n get-bitmap) )(send node get-results))]
                     [bitmap (make-object bitmap% "../images/ent_computer.png")])))
    (send pb insert newnode (send node get-x) (send node get-y))))

; For now, just places the nodes and edges.
(define (visualize ng pb)
  (map (lambda (node) (place-node pb node)) (send ng get-nodes))
  (send pb set-edges! (send ng get-edges)))