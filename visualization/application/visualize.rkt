#lang racket/gui

(provide visualize)

(require "netgraph.rkt" "modelgraph.rkt" "controls.rkt")

(define type-bitmap-hash (make-hash))
(hash-set! type-bitmap-hash 'host "../images/ent_computer.png")
(hash-set! type-bitmap-hash 'server "../images/ent_server.png")
(hash-set! type-bitmap-hash 'group "../images/ent_group.png")
(hash-set! type-bitmap-hash 'firewall "../images/ent_fw.png")

; Adds a node to the pasteboard.
; It creates a snip and positions it based on the node's location
(define (place-node pb node)
  
  (let
      ((newnode (new entity-snip%
                     [updatef (lambda (x y) 
                                (send node set-x! x)
                                (send node set-y! y))]
                     [name (send node get-name)]
                     [icons (map (lambda (n) (send n get-bitmap)) (send node get-results))]
                     [bitmap (make-object bitmap% (hash-ref type-bitmap-hash (send node get-type)))])))
    (begin
    (send pb insert newnode (send node get-x) (send node get-y))
  (if (not (null? (send node get-subgraph)))
      (let (
            [edsnip (new editor-snip%)]
            [newpb (new fwpboard%)])
        (begin
          (send edsnip set-editor newpb)
          (send pb set-before edsnip #f)
          (send newnode set-subed! edsnip)
          (send newnode set-flags (list 'handles-events))
          (visualize (send node get-subgraph) newpb)))
      #f
      ))
  ))

; For now, just places the nodes and edges.
(define (visualize ng pb)
  (map (lambda (node) (place-node pb node)) (send ng get-nodes))
  (send pb set-edges! (send ng get-edges)))