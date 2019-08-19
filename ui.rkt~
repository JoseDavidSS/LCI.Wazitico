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

";_________________________________________________________________________________________________________________________Main Menu;"

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

";_______________________________________________________________________________________________________________________Menu Screen;"

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

";________________________________________________________________________________________________________________________Map Screen;"
(define listaChoice null)

(define (prueba)
  (begin
    (set! listaChoice (append (list "Ruta Principal") (list "Ruta Alterna")))
    (prueba2 listaChoice)))

(define (prueba2 lista)
  (cond ((null? lista)
         0)
        (else
         1)))
  

; Build the Map frame
(define mapScreen (new frame% [label "Wazitico"]
                                     [width 1000]
                                     [height 900]))

; Map panel
(define mapPanel (new pane% [parent mapScreen]
                                     [border 0]
                                     [spacing 0]
                                     [vert-margin 0]
                                     [alignment '(center center)]))
; Map canvas
(define canvas (new canvas% [parent mapPanel]))

; Changes from ConfigurationScreen to MapScreen 
(define (toMenuFromMapScreen)
  (send mapScreen show #f)
  (showMenu #t))

; Make a button in the Map frame to return to the Menu
(new button% [parent mapScreen]
             [label "Regresar al Menu"]
             [callback (lambda (button event)
                         (toMenuFromMapScreen))])

; Changes frame from MapScreen to ConfigurationScreen
(define (toConfigFromMapScreen)
  (send mapScreen show #f)
  (send configScreen show #t))

(define msgPesos (new message% [parent configScreen]
                          [label (string-append "Peso de la ruta: " "(pesoRuta-string)")]))

; Changes frame from ConfigurationScreen to MapScreen button
(new button% [parent mapScreen]
             [label "Agregar Elementos"]
             [callback (lambda (button event)
                         (toConfigFromMapScreen))])

; Draw lines in MapScreen canvas
(define (dibujar-linea x y x2 y2 canvas  color grosor)
  (define lapiz (instantiate pen% (color grosor 'solid)))
  (define dc (send canvas get-dc))
  (send dc set-pen lapiz)
  (send dc draw-line x y x2 y2)
)

";______________________________________________________________________________________________________________Configuration Screen;"

; Config Screen
(define configScreen (new frame% [label "Wazitico"]
                                     [width 600]
                                     [height 625]))

; Changes Frame to MapScreen from ConfigurationScreen
(define (toMapFromConfigScreen)
  (send mapScreen show #t)
  (send configScreen show #f))

(define msg (new message% [parent configScreen]
                          [label "Configuración de Ruta y Nodos"]))

(define search-box (new text-field% [parent configScreen]
                                    [label " Lugar de Inicio:     "]))

(define search-box2 (new text-field% [parent configScreen]
                                     [label " Lugar de Destino: "]))

; Returns from ConfigScreen to MapScreen
(new button% [parent configScreen]
             [label "Crear ruta"]
             [callback (lambda (button event)
                         (send msg set-label "Holaaaa perros"))])

(define espacio (new message% [parent configScreen]
                          [label "_____________________________________________________________________________________"]))


(define search-box3 (new text-field% [parent configScreen]
                                     [label " Nombre del lugar por crear: "]))

(define espacio2 (new message% [parent configScreen]
                          [label " "]))

(define txtConfig (new message% [parent configScreen]
                          [label " Configuración del nodo por crear:"]))

;Node 1 ready to connect
(define search-box4 (new text-field% [parent configScreen]
                                     [label " 1. Nodo con el que estará conectado: "]))

(define search-box4.1 (new text-field% [parent configScreen]
                                     [label "                                                Su peso: "]))

(define check-box (new check-box% [parent configScreen]
                [label "<- Si este nodo es bidireccional seleccione la casilla "]))

(define espacio3 (new message% [parent configScreen]
                          [label " "]))

;Node 2 ready to connect
(define search-box5 (new text-field% [parent configScreen]
                                     [label " 2. Nodo con el que estará conectado: "]))

(define search-box5.1 (new text-field% [parent configScreen]
                                     [label "                                                Su peso: "]))

(define check-box2 (new check-box% [parent configScreen]
                [label "<- Si este nodo es bidireccional seleccione la casilla "]))

(define espacio4 (new message% [parent configScreen]
                          [label " "]))

;Node 3 ready to connect
(define search-box6 (new text-field% [parent configScreen]
                                     [label " 3. Nodo con el que estará conectado: "]))

(define search-box6.1 (new text-field% [parent configScreen]
                                     [label "                                                Su peso: "]))

(define check-box3 (new check-box% [parent configScreen]
                [label "<- Si este nodo es bidireccional seleccione la casilla "]))

(define (set-texts)
  (send search-box4 set-value "")
  (send search-box4.1 set-value "")
  (send search-box5 set-value "")
  (send search-box5.1 set-value "")
  (send search-box6 set-value "")
  (send search-box6.1 set-value ""))                   

; Returns from ConfigScreen to MapScreen
(new button% [parent configScreen]
             [label "Crear Lugar"]
             [callback (lambda (button event)
                         (dibujar-linea 100 200 20 30 canvas "BLUE" 13))])

(define espacio6 (new message% [parent configScreen]
                          [label "_____________________________________________________________________________________"]))

(define search-box7 (new text-field% [parent configScreen]
                                     [label "Número de ruta que quiere seguir:       "]))

(define msgRutas (new message% [parent configScreen]
                          [label (string-append "Número de rutas: " "(numeroRutas-string)")]))

; Returns from ConfigScreen to MapScreen
(new button% [parent configScreen]
             [label "Actualizar mapa"]
             [callback (lambda (button event)
                         (toMapFromConfigScreen))])

; Returns from ConfigScreen to MapScreen
(new button% [parent configScreen]
             [label "Regresar al Mapa"]
             [callback (lambda (button event)
                         (toMapFromConfigScreen))])

";___________________________________________________________________________________________________Conexion de Logica con interfaz;"

; Validacion para que haya informacion en los text-field y no crear nodos sin informacion
(define (validacionNodos)
  (cond ((equal? (send search-box4 get-value) "")
         "Nada que hacer")
        ((equal? (send search-box5 get-value) "")
         (crearNodo1conexion))
        ((equal? (send search-box6 get-value) "")
         (crearNodo2conexiones))
        (else
         (crearNodo3conexiones))))

; Convertir el valor booleano del check-box en 1 si esta seleccionado o 0 si no lo esta
(define (boolCheck-box1)
  (cond ((equal? (send check-box get-value) #t)
         1)
        (else
         0)))

; Convertir el valor booleano del check-box2 en 1 si esta seleccionado o 0 si no lo esta
(define (boolCheck-box2)
  (cond ((equal? (send check-box2 get-value) #t)
         1)
        (else
         0)))

; Convertir el valor booleano del check-box3 en 1 si esta seleccionado o 0 si no lo esta
(define (boolCheck-box3)
  (cond ((equal? (send check-box3 get-value) #t)
         1)
        (else
         0)))

; Crear conexion entre el nodo nuevo y uno existente
(define (crearNodo1conexion)
  (cond ((equal? (send search-box4.1 get-value) "")
         "Nada que hacer")
        (else
         (anadirAlGrafo (metodo-Sahid (send search-box3 get-value)); int el que estoy creando
                        (list (metodo-Sahid (send search-box4 get-value))); list al que me quiero conectar
                        (list (metodo-Sahid (send search-box4.1 get-value))); list peso
                        (list (boolCheck-box1)))))); list direccion

; Crear conexion entre el nodo nuevo y dos existentes
(define (crearNodo2conexiones)
  (cond ((equal? (send search-box5.1 get-value) "")
         "Nada que hacer")
        (else
         (anadirAlGrafo (metodo-Sahid (send search-box3 get-value)); int el que estoy creando
                        
                        (list (metodo-Sahid (send search-box4 get-value))
                              (metodo-Sahid (send search-box5 get-value))); list al que me quiero conectar
                        
                        (list (metodo-Sahid (send search-box4.1 get-value))
                              (metodo-Sahid (send search-box5.1 get-value))); list peso
                        
                        (list (boolCheck-box1) (boolCheck-box2)))))); list direccion

; Crear conexion entre el nodo nuevo y tres existentes
(define (crearNodo3conexiones)
  (cond ((equal? (send search-box6.1 get-value) "")
         "Nada que hacer")
        (else
         (anadirAlGrafo (metodo-Sahid (send search-box3 get-value)); int el que estoy creando
                        
                        (list (metodo-Sahid (send search-box4 get-value))
                              (metodo-Sahid (send search-box5 get-value))
                              (metodo-Sahid (send search-box6 get-value))); list al que me quiero conectar
                        
                        (list (metodo-Sahid (send search-box4.1 get-value))
                              (metodo-Sahid (send search-box5.1 get-value))
                              (metodo-Sahid (send search-box6.1 get-value))); list peso
                        
                        (list (boolCheck-box1) (boolCheck-box2) (boolCheck-box3)))))); list direccion

;______________________________________________________________________________________________________________________Run;

(showMenu #t)