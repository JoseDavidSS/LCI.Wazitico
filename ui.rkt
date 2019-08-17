#lang racket/gui
(require 2htdp/image)

;Blank bitmap for resize
(define bitmap-blank
  (lambda [[w 0] [h #false] #:backing-scale [backing-scale 2.0]]
    (define width  (max 1 (exact-ceiling w)))
    (define height (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale backing-scale)))

;Resize bitmap
(define bitmap-scale
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (let ([w (max 1 (exact-ceiling (* (send bmp get-width) scale-x)))]
                       [h (max 1 (exact-ceiling (* (send bmp get-height) scale-y)))])
                   (define dc (make-object bitmap-dc% (bitmap-blank w h)))
                   (send dc set-smoothing 'aligned)
                   (send dc set-scale scale-x scale-y)
                   (send dc draw-bitmap bmp 0 0)
                   (or (send dc get-bitmap) (bitmap-blank)))])]))

;-------------------------Main Menu;

; Main menu Screen
(define menuScreen (new frame% [label "Wazitico"]
                   [width 800]
                   [height 600]
                   [style '(no-resize-border)]))

; Load images
(define arcadiabayButtonIcon (make-object bitmap% "assets/arcadiabay_button.png"))
(define arcadiabay_map (make-object bitmap% "assets/maps/arcadiabay_map.png"))
(define background (make-object bitmap% "/home/jose/Desktop/Racket/Wazecheme/assets/start_button.png"))
(define hyruleButtonIcon (make-object bitmap% "assets/hyrule_button.png"))
(define hyrule_map (make-object bitmap% "assets/maps/hyrule_map.png"))
(define logo_namePic (make-object bitmap% "assets/logo_name.png"))
(define logoPic (make-object bitmap% "assets/logo.png"))
(define namePic (make-object bitmap% "assets/name.png"))
(define selectBackground (make-object bitmap% "assets/background_citySelect.png"))
(define startButtonIcon (make-object bitmap% "/home/jose/Desktop/Racket/Wazecheme/assets/start_button.png"))

";_______________________________________________________________________Menu Screen;"

; Build the Main Menu frame
(define menuPanel (new panel% [parent menuScreen]
                             [border 0]
                             [vert-margin 0]
                             [spacing 0]
                             [alignment '(center center)]))

; Control menu frame show with boolean value
(define (showMenu bool)
  (cond ((equal? bool #t)
         (send menuScreen show #t))
        (else
         (send menuScreen show #f))))

; Changes Frame to MapScreen from ConfigurationScreen
(define (toMapFromMenuScreen)
  (send mapScreen show #t)
  (showMenu #f))

; Changes from MenuScreen to MapScreen
(new button% [parent menuPanel]
             [label (bitmap-scale startButtonIcon 0.6)]
             [callback (lambda (button event)
                         (toMapFromMenuScreen))])

";_______________________________________________________________________Map Screen;"

; Build the Map frame
(define mapScreen (new frame% [label "Wazitico"]
                                     [width 800]
                                     [height 600]))

; Map panel
(define mapPanel (new pane% [parent mapScreen]
                                     [border 0]
                                     [spacing 0]
                                     [vert-margin 0]
                                     [alignment '(center center)]))
; Map canvas
(define canvas (new canvas% [parent mapPanel]))


;(define msg (new message% [parent mapScreen]
;                          [label "No events so far..."]))

; Changes from ConfigurationScreen to MapScreen 
(define (toMenuFromMapScreen)
  (send mapScreen show #f)
  (showMenu #t))

; Make a button in the Map frame to return to the Menu
(new button% [parent mapScreen]
             [label "Return to Menu"]
             [callback (lambda (button event)
                         (toMenuFromMapScreen))])

; Changes frame from MapScreen to ConfigurationScreen
(define (toConfigFromMapScreen)
  (send mapScreen show #f)
  (send configScreen show #t))

; Changes frame from ConfigurationScreen to MapScreen button
(new button% [parent mapScreen]
             [label "Add Items"]
             [callback (lambda (button event)
                         (toConfigFromMapScreen))])

";_______________________________________________________________________Configuration Screen;"

; Config Screen
(define configScreen (new frame% [label "Wazitico"]
                                     [width 800]
                                     [height 600]))

; Changes Frame to MapScreen from ConfigurationScreen
(define (toMapFromConfigScreen)
  (send mapScreen show #t)
  (send configScreen show #f))

(define search-box (new text-field% [parent configScreen]
                                    [label #f]))

(define search-box2 (new text-field% [parent configScreen]
                                     [label #f]))

; Returns from ConfigScreen to MapScreen
(new button% [parent configScreen]
             [label (bitmap-scale startButtonIcon 0.6)]
             [callback (lambda (button event)
                         (toMapFromConfigScreen))])

";_______________________________________________________________________Run;"

(showMenu #t)