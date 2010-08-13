#lang racket/gui

(provide fwpboard% entity-snip% next-button-snip%)

(define arrow-dark (make-object color% 30 30 30))
(define arrow-light (make-object color% 170 170 170))
(define x-red (make-object color% 255 0 0))

; Returns a normalized vector
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
           [arrow-color (if (send edge is-active?) arrow-dark arrow-light)]
           )
    ; Line pen
    (send dc set-pen arrow-color 3 (if (send edge is-blocked?) 'short-dash 'solid))
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
    (if (send edge is-active?)
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
          ) #f)
    
    ; Draw the X
    (if (send edge is-blocked?)
        (begin
          (send dc set-pen x-red 3 'solid)
          (send dc draw-line
                (- (/ (+ cx1 cx2) 2) 12)
                (- (/ (+ cy1 cy2) 2) 12)
                (+ (/ (+ cx1 cx2) 2) 12)
                (+ (/ (+ cy1 cy2) 2) 12))
          (send dc draw-line
                (- (/ (+ cx1 cx2) 2) 12)
                (+ (/ (+ cy1 cy2) 2) 12)
                (+ (/ (+ cx1 cx2) 2) 12)
                (- (/ (+ cy1 cy2) 2) 12))) #f)
              
    ; Fix things
    (send dc set-pen "black" 1 'solid)
    (send dc set-brush "black" 'transparent)
    (send dc set-smoothing 'unsmoothed)
    ))

; Subclass of pasteboard, so we can draw the arrows.
(define fwpboard% 
  (class pasteboard%
    (init-field [next_model_fun null])
    (define edges empty)
    
    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (if before?
          (begin
            (map (lambda (edge) (draw-edge dc edge))
                 ; Sort them so that the black ones are drawn on top of the gray ones
                 (sort edges (lambda (a b) (send b is-active?))))
            (send this invalidate-bitmap-cache left top (- right left) (- bottom top))
            ) #f)
      )
    
    ; If you press space
    (define/override (on-default-char event)
      (if (eq? (send event get-key-code) #\space)
          (begin
          (send this select-all)
          (send this clear)
          (set! edges empty)
          (next_model_fun)
          (send this refresh 0 0 1000 800 'no-caret #f)
          ) #f))
    
    ; Double clicking brings up the subcomponents
    (define/override (on-double-click snip event)
      (if (and (is-a? snip entity-snip%) (not (null? (send snip get-subed))))
          (if (send this get-snip-location (send snip get-subed))
              (send this remove (send snip get-subed))
              (begin
                (send this insert (send snip get-subed) (- (send event get-x) 150) (- (send event get-y) 150))
                (send (send snip get-subed) set-flags (list))
                (send this set-before (send snip get-subed) #f))
              ) #f))
    
    (define/public (set-edges! e) (set! edges e))
    
    (super-new)))

; Subclass of image-snip, so we can have text below the images.
(define entity-snip%
  (class image-snip%
    (init-field updatef [bitmap null] [kind null] [icons empty] [name ""] [subed null])
    
    ; Draws the box around entities with policy decisions, and draws the icons for those decisions.
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (let ([w (send bitmap get-width)]
            [h (send bitmap get-height)])
        
      (if (not (empty? icons)) (begin
                                 (if (not (null? subed))
                                     (send dc set-pen arrow-dark 2 'solid)
                                     (send dc set-pen arrow-light 1 'solid))
                                 (send dc set-smoothing 'aligned)
                                 (send dc draw-rounded-rectangle (- x 20) (- y 10) 130 130 9)
                                 (send dc set-smoothing 'unsmoothed)
                                 (send dc set-pen "black" 1 'solid)) #f)
      (super draw dc x y left top right bottom dx dy draw-caret)
      (send dc draw-text name (- (+ x (/ w 2)) (/ (let-values ([(a b c d) (send dc get-text-extent name)]) a) 2)) (+ y h))
      (draw-icons icons dc x y)
        ; Updates the modelgraph node positions
      (updatef x y)      
      ))
    
    (define/private (draw-icons icons dc x y)
      (cond [(empty? icons) #f]
            [else (begin
                    (send dc draw-bitmap (first icons) (- x 16) (- y 23))
                    (draw-icons (rest icons) dc (+ 34 x) y))]))
    
    (define/public (set-subed! ed) (set! subed ed))
    (define/public (get-subed) subed)
    
    (super-make-object bitmap)))