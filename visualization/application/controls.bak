#lang racket/gui

(provide fwpboard% entity-snip%)

(define arrow-dark (make-object color% 30 30 30))
(define arrow-light (make-object color% 170 170 170))

(define (vn x y)
  (let ([d (sqrt (+ (* x x) (* y y)))])
    (vector (/ x d) (/ y d))))

; Draws the edge by looking at the location of the nodes
(define (draw-edge dc edge)
  (letrec (
           [n1 (send edge get-from)]
           [n2 (send edge get-to)]
           [cx1 (+ (send n1 get-x) 43)]
           [cy1 (+ (send n1 get-y) 43)]
           [cx2 (+ (send n2 get-x) 43)]
           [cy2 (+ (send n2 get-y) 43)]
           [v1 (vn (- cx2 cx1) (- cy2 cy1))]
           [arrow-color (if (send edge active?) arrow-dark arrow-light)]
           )
    ; Line pen
    (send dc set-pen arrow-color 3 'solid)
    (send dc set-smoothing 'aligned)
    ; Draw the line
    (send dc draw-line
          (+ cx1 (* 70 (vector-ref v1 0)))
          (+ cy1 (* 70 (vector-ref v1 1)))
          (- cx2 (* 70 (vector-ref v1 0)))
          (- cy2 (* 70 (vector-ref v1 1))))
    ; Set the pen width back
    (send dc set-pen arrow-color 1 'solid)
    ; Set brush
    (send dc set-brush arrow-color 'solid)
    ; Draw the arrowhead
    (send dc draw-polygon
          (list 
           (make-object point%
             (- cx2 (* 60 (vector-ref v1 0)))
             (- cy2 (* 60 (vector-ref v1 1))))
           (make-object point%
             (+ (- cx2 (* 75 (vector-ref v1 0))) (- 0 (* 7 (vector-ref v1 1))))
             (+ (- cy2 (* 75 (vector-ref v1 1))) (* 7 (vector-ref v1 0)))
             )
           (make-object point%
             (- (- cx2 (* 75 (vector-ref v1 0))) (- 0 (* 7 (vector-ref v1 1))))
             (- (- cy2 (* 75 (vector-ref v1 1))) (* 7 (vector-ref v1 0)))
             ))          
          )
    ; Fix things
    (send dc set-pen "black" 1 'solid)
    (send dc set-brush "black" 'transparent)
    (send dc set-smoothing 'unsmoothed)
    ))

; Subclass of pasteboard, so we can draw the arrows.
(define fwpboard% 
  (class pasteboard%
    (define edges empty)
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (if before?
          (begin
            (map (lambda (edge) (draw-edge dc edge)) edges)
            (send this invalidate-bitmap-cache left top (- right left) (- bottom top))
            ) #f)
      )
    
    (define/public (set-edges! e) (set! edges e))
    
    (super-new)))

; Subclass of image-snip, so we can have text below the images.
(define entity-snip%
  (class image-snip%
    (init-field updatef [bitmap null] [icons empty] [name ""])
    
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (if (not (empty? icons)) (begin
                                 (send dc set-smoothing 'aligned)
                                 (send dc draw-rounded-rectangle (- x 20) (- y 10) 120 120 9)
                                 (send dc set-smoothing 'unsmoothed)) #f)
      (super draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text name (+ x 15) (+ y 90))
      (draw-icons icons dc x y)
      (updatef x y)      
      )
    
    (define/private (draw-icons icons dc x y)
      (cond [(empty? icons) #f]
            [else (begin
                    (send dc draw-bitmap (first icons) (- x 16) (- y 23))
                    (draw-icons (rest icons) dc (+ 34 x) y))]))
    
    (define/override (on-event dc x y editorx editory event)
      (super on-event dc x y editorx editory event)
      )
    
    (super-make-object bitmap)))

