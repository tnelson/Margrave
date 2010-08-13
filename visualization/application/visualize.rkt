#lang racket/gui

(provide visualize)
(require "netgraph.rkt" "controls.rkt" "modelgraph.rkt" "apply-model.rkt" "visxml.rkt")
(require "../../margrave.rkt" xml "../../margrave-xml.rkt")

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
                     [kind 'png/mask]
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

; Places the nodes and edges
(define (_visualize ng pb)
  (map (lambda (node) (place-node pb node)) (send ng get-nodes))
  (send pb set-edges! (send ng get-edges)))

; Sets up the window and canvas, calls _visualize
(define (visualize ng query kws)
  (begin
    (mtext query)
    (letrec ([window (new frame%
                          [label "FWP App"]
                          [width 1000]
                          [height 800])]
             [canvas (new editor-canvas% [parent window])]
             [mod (new mg-model% [xml (get-child-element (document-element (mtext "GET ONE 0")) 'model)] [keyword-map kws])]
             [pb (new fwpboard% [next_model_fun
                                 (lambda () 
                                   (begin
                                     ;(display-xml (mtext "GET NEXT 0"))
                                     (let ([mod (new mg-model% [xml (get-child-element (document-element (mtext "GET NEXT 0")) 'model)] [keyword-map kws]) ])
                                     (_visualize (apply-model/pos ng mod) pb))))])])
      (begin
        (send canvas set-editor pb)
        (send pb insert (make-object image-snip% (make-object bitmap% "../images/key.png")) 760 10)
        (_visualize (apply-model/pos ng 
                                     mod) pb)
        (send window show #t)))))
