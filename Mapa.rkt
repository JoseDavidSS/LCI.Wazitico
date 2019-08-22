#lang racket/gui

(require racket/gui map-widget)
(define toplevel (new frame% [label "Map Demo"] [width 800] [height 800]))
(define panelMapa (new panel% [parent toplevel]
                             [border 0]
                             [vert-margin 0]
                             [spacing 0]
                             [alignment '(center center)]))
(define map (new map-widget% [parent panelMapa]))

(define (go)
  (send map move-to ( vector(string->number(send search-box get-value)) (string->number (send search-box2 get-value)                                                                                                      
  ))))
(define (irCasa)
   (send map set-track-current-location #t)
  )
(define (casa)
  (define color (make-color 0 135 36))
  (send map add-marker( vector(string->number(send search-box get-value)) (string->number (send search-box2 get-value))) "Casa "  1 color)
  (send map set-current-location  ( vector(string->number(send search-box get-value)) (string->number (send search-box2 get-value))))
  (send map set-track-current-location #t)
  )
(define (zoom)
(send map set-zoom-level (string->number(send search-box3 get-value))                                                                                         )
 )
(new button% [parent toplevel]
             [label "Ir a las cordenadas"]
             [callback (lambda (button event)
                         (go))])
(define search-box (new text-field% [parent toplevel]
                                    [label "Latitud     "]))
(define search-box2 (new text-field% [parent toplevel]
                               [label "Longitud     "]))
(new button% [parent toplevel]
             [label "Escoger como casa"]
             [callback (lambda (button event)
                         (casa))])
(new button% [parent toplevel]
             [label "Ir a casa"]
             [callback (lambda (button event)
                         (irCasa))]) 
(define search-box3 (new text-field% [parent toplevel]
                                    [label "Zoom (los valor van del 1 al 16) "]))
(new button% [parent toplevel]
             [label "Cambiar Zoom"]
             [callback (lambda (button event)
                         (zoom))])
(send toplevel show #t)