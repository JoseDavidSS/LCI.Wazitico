#lang racket/gui
(require racket/draw/arrow)
;;(require 2htdp/image)
(require racket/include)
(require "Logic.rkt")
;_____________________________________________________________Funciones Para Graficar el Mapa



(define red-brush
  (new brush%
[color (make-object color% 0 0 0)]))

;_____________________________________________________________Funcion que  pasa de un nombre a su valor numerico

(define (lugar-numero lugar contador)
  (define elemento (first(hash-ref nodos (number->string contador))))
  (cond
    [(eq? contador cantidad-nodos)
     ]
    
    [(equal?  lugar  elemento )
      contador]
    [else
     (lugar-numero  lugar (+ contador 1))]
    ))

;; ESTAS SON LAS VARIABLES PARA SABER DONDE SE ENCUENTRAN LOS NODOS
;;(define nodos (hash "1" '("Cartago" 100 100) "2" '("SanJose" 400 400)))
(define cantidad-nodos 1 )
(define nodos (make-hash))
;;Esta es la funcion que me actualiza el  hast table de nodos con sus nombres
(define (agregar-nodo nombre   ids pesos bi )
  (define valor1 ids)
  (define valor2 pesos)
  (define valor bi)
  (cond
    [(eq? cantidad-nodos 1)
     (anadirAlGrafo cantidad-nodos   '(0) '(0) '(0))
     (hash-set! nodos (number->string cantidad-nodos) (list nombre  (random 700)  (random 700)))
     (set! cantidad-nodos (+ cantidad-nodos 1))
     ]
    [else
     (quote ids)
     (anadirAlGrafo cantidad-nodos    ids pesos  bi)
     (hash-set! nodos (number->string cantidad-nodos) (list nombre  (random 700)  (random 700)))
     (set! cantidad-nodos (+ cantidad-nodos 1))] )
  )
(define (dibujar-rutas cantidad-nodos )
  (cond
    [(zero? cantidad-nodos )
     '()
     ]
    [else
     (dibujar-rutas-aux2 cantidad-nodos  (car(infoNodo cantidad-nodos)) (cadr(infoNodo cantidad-nodos)) (cadr(cdr (infoNodo cantidad-nodos) )) (-(largo (car (infoNodo cantidad-nodos) ))1))
     (dibujar-rutas (- cantidad-nodos 1))
     ] 
 )
)
;_____________________________________________________________Funcion aux que ayuda a  dibujar todas las rutas del grafo

(define (dibujar-rutas-aux2  nodo  ids pesos bi size )
  (cond
    [(and(not(equal? size -1)) )
     ;;(hash-ref nodos (number->string cantidad-de-nodos-restantes))
     (define id (list-ref ids  size))
     (define peso (list-ref  pesos size))
     (define direccion (list-ref  bi size) )
     
     (define xInicial (+(cadr(hash-ref nodos (number->string nodo))) 50))
     (define yInicial (+ (cadr(cdr(hash-ref nodos (number->string nodo))))50))
     (define xFinal (+(cadr(hash-ref nodos (number->string id)))50))
     (define yFinal (+ (cadr(cdr(hash-ref nodos (number->string id))))50))
     (define xTexto ( + xInicial (/(-  xFinal xInicial)2) 10) )
     (define yTexto (+ yInicial (/(-  yFinal yInicial)2)10))
     (define xCruz ( + xInicial (/(-  xFinal xInicial)6) ) )
     (define yCruz (+ yInicial (/(-  yFinal yInicial)6)))
     (define dc (send canvas get-dc))
     (send dc set-pen no-pen)
     (send dc set-brush red-brush)
     (cond
       [(zero? direccion)
         (draw-arrow dc xInicial yInicial xFinal yFinal 0 0 )
        ;(dibujar-linea xInicial yInicial xFinal yFinal canvas "VIOLET" 14)
        ;(dibujar-linea (- xCruz 10 )(- yCruz 10)(+ xCruz 10) (+ yCruz 10) canvas "VIOLET" 10)
        ;(dibujar-linea (- xCruz 10 )(+ yCruz 10)(+ xCruz 10) (- yCruz 10) canvas "VIOLET" 10)
        (dibujar-texto xTexto yTexto 1 canvas (number->string peso))
        (dibujar-rutas-aux2  nodo  ids pesos bi (- size 1) )
        ]
       [else
        
        (draw-arrow dc xInicial yInicial xFinal yFinal 0 0 )
        ;;(dibujar-linea xInicial yInicial xFinal yFinal canvas "PURPLE" 14)
        ;;(dibujar-linea (- xCruz 10 )(- yInicial 10)(+ xCruz 10) (+ yInicial 10) canvas "PURPLE" 10)
        ;;(dibujar-linea (- xCruz 10 )(+ yCruz 10)(+ xCruz 10) (- yCruz 10) canvas "PURPLE" 10)
        (dibujar-texto xTexto yTexto 1 canvas (number->string peso))
        (dibujar-rutas-aux2  nodo  ids pesos bi (- size 1) )]
       )
     ]
    
    
  )
  )

;;Esta es la funcion que dibujara los nodos despues haber agregado uno o varios
(define (dibujar-nodos cantidad-de-nodos-restantes)
  (cond
   [(not (zero?  cantidad-de-nodos-restantes ))
    (define valores (hash-ref nodos (number->string cantidad-de-nodos-restantes)))
    (define nombre-del-nodo(first valores))
    (define posicion-x (first(rest valores) ))
    (define posicion-y   (first(rest(rest valores) )))
    (dibujar-nodo posicion-x posicion-y 100 canvas nombre-del-nodo)
    (dibujar-nodos ( - cantidad-de-nodos-restantes 1))
  ]
   [else  '()])
  )
;;Funcion necesarias para obtener x y de un nodo
(define (dame-posiciones nodo-buscar)
   (rest(hash-ref nodos (number->string nodo-buscar)))
  )
;;Esta funcion es la dibujara la mejor ruta
(define (dibujar-mejor-ruta  lista-mejores)
  (cond
    [(not(null?  (rest lista-mejores)))
     (define nodo-inicial (first lista-mejores))
     (define nodo-final (first(rest lista-mejores)))
     (define pos-inicial (dame-posiciones nodo-inicial))
     (define pos-final (dame-posiciones nodo-final))
     (define dc (send canvas get-dc))
     
     (send dc set-brush red-brush)
     (define lapiz
  (new pen%
       [color (make-object color% 200 64 52)]
[width 8 ]))
     (send dc set-pen lapiz)
     (draw-arrow dc (+(first pos-inicial) 50) (+(first(rest pos-inicial)) 50)(+(first pos-final) 50) (+(first(rest pos-final)) 50) 0 0 )
     ;;(dibujar-linea (+(first pos-inicial) 50) (+(first(rest pos-inicial)) 50) (+(first pos-final) 50) (+(first(rest pos-final)) 50) canvas "RED" 25)
     (dibujar-mejor-ruta (rest lista-mejores))
     ]
    [else '()]))
; ESTOS SON LOS COLORES QUE MANTENDRAN DISPONIUBLES PARA DIBUJAR 
;
(define white (make-object color% "white"))
(define black (make-object color% "black"))
(define red   (make-object color% "red"))
(define blue (make-object color% "blue"))

  ; algunos lapices y brochas
(define no-pen (make-object pen% "BLACK" 1 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))

;;ESTE ES EL METODO QUE DIBUJA UN TEXTO EN EL CANVAS
(define (dibujar-texto  x y  size canvas texto )
  (define dc (send canvas get-dc))
  (send dc set-scale size size)
  (send dc set-text-foreground "blue")
  (send dc draw-text texto x y)
)
;;ESTE ES EL METODO  QUE REALIZA LOS NODOS CON CIERTO TAMAÑO Y CIERTO  TEXTO EN ELLOS 
(define (dibujar-nodo x y radio canvas texto)
  (define dc (send canvas get-dc))
  (send dc set-pen no-pen)
  (send dc set-brush yellow-brush)
  (send dc draw-ellipse x y radio radio)
  (dibujar-texto x (+ y (/ radio 2)) 1 canvas texto)
  )

;; ESTE ES EL METODO PARA DIBUJAR LA LINEA DE LAS RUTAS 
(define (dibujar-linea x y x2 y2 canvas  color grosor)
  (define lapiz (instantiate pen% (color grosor 'solid)))
  (define dc (send canvas get-dc))
  (send dc set-pen lapiz)
  (send dc draw-line x y x2 y2)
)

;------------------------------------------------------------------------------------------------------
;;(require "Rutas Panel.rkt")
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

(define fondo (make-object bitmap% "Imagenes/fondo.png"))
(define botonInicio (make-object bitmap% "Imagenes/iniciar.png"))
(define botonExtras (make-object bitmap% "Imagenes/extras.png"))

";_______________________________________________________________________________________________________________________Menu Screen;"

; Build the Main Menu frame
(define menuPanel (new panel% [parent menuScreen]
                             [border 0]
                             [vert-margin 0]
                             [spacing 0]
                             [alignment '(center center)]))

(define menuPanel2 (new panel% [parent menuScreen]
                             [border 0]
                             [vert-margin 0]
                             [spacing 0]
                             [alignment '(center center)]))

(define menuPanel23 (new panel% [parent menuScreen]
                             [border 0]
                             [vert-margin 0]
                             [spacing 0]
                             [alignment '(center center)]))

(define fondos (new message% [parent menuPanel]
                          [label (bitmap-scale fondo 0.6)]))

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
(new button% [parent menuPanel2]
             [label (bitmap-scale botonInicio 0.6)]
             [callback (lambda (button event)
                         (toMapFromMenuScreen))])

; Changes from MenuScreen to MapScreen
(new button% [parent menuPanel23]
             [label (bitmap-scale botonExtras 0.3)]
             [callback (lambda (button event)
                         (toExtraFromMenuScreen))])

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
  (send configScreen show #t)
  )

(define msgPesos (new message% [parent mapScreen]
                          [label (string-append "Peso de la ruta: " "(pesoRuta-string)")]))

; Changes frame from ConfigurationScreen to MapScreen button
(new button% [parent mapScreen]
             [label "Agregar Elementos"]
             [callback (lambda (button event)
                         (toConfigFromMapScreen))])

;;_________________________________________________________________________________________________________________
(define (better-route )
  (define  inicio (send search-box get-value ))
  (define  fin (send search-box2 get-value ))
  (set! inicio (lugar-numero inicio 1))
  (set! fin (lugar-numero fin 1))
  (cond
    [(equal? (send search-box7 get-value) "")
        (dibujar-mejor-ruta (cadr(dijkstra inicio fin)))
    ]
    [(equal? (string->number(send search-box7 get-value))  (largo (dameTodasRutas inicio fin)  ))
    (dibujar-mejor-ruta  (list-ref (dameTodasRutas inicio fin) (-(string->number(send search-box7 get-value) ) 1) ) )
    ]
    [else
      (dibujar-mejor-ruta  (list-ref (dameTodasRutas inicio fin) (string->number(send search-box7 get-value) ) ) )
      ]
   )
)



";______________________________________________________________________________________________________________Configuration Screen;"

; Config Screen
(define configScreen (new frame% [label "Wazitico"]
                                     [width 600]
                                     [height 625]))

; Changes Frame to MapScreen from ConfigurationScreen
(define (toMapFromConfigScreen bandera)
  (cond
    [(eq? bandera #t)
     (define dc (send canvas get-dc))
     (send dc clear)
     (better-route)
     (define  inicio (send search-box get-value ))
     (define  fin (send search-box2 get-value ))
     (set! inicio (lugar-numero inicio 1))
     (set! fin (lugar-numero fin 1))
     (send msgRutas set-label (string-append "Numero de Rutas totales:" (number->string(-(largo (dameTodasRutas inicio fin ))1)) ))
     (send msgPesos set-label (string-append "Peso de la mejor ruta" (number->string(caar (dameTodasRutas inicio fin )))) )
     (dibujar-nodos (- cantidad-nodos 1))
     (dibujar-rutas(- cantidad-nodos 1))
     (send mapScreen show #t)
     (send configScreen show #f)
    ]
    [else
     (define dc (send canvas get-dc))
     (send dc clear)
     (dibujar-nodos (- cantidad-nodos 1))
     (dibujar-rutas(- cantidad-nodos 1))
     (send mapScreen show #t)
     (send configScreen show #f)
     ]
  ))
  

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
                         ((toMapFromConfigScreen #t)))])

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
                         (validacionNodos))])

(define espacio6 (new message% [parent configScreen]
                          [label "_____________________________________________________________________________________"]))

(define search-box7 (new text-field% [parent configScreen]
                                     [label "Número de ruta que quiere seguir:       "]))

(define msgRutas (new message% [parent configScreen]
                          [label (string-append "Número de rutas: " "(numeroRutas-string)")]))

; Returns from ConfigScreen to MapScreen
(new button% [parent configScreen]
             [label "Regresar al Mapa"]
             [callback (lambda (button event)
                         (toMapFromConfigScreen #f))])

";___________________________________________________________________________________________________Conexion de Logica con interfaz;"


; Validacion para que haya informacion en los text-field y no crear nodos sin informacion
(define (validacionNodos)
  (define valor (send search-box3 get-value) )
  (cond 
        ((equal? (send search-box4 get-value) "" )
         (agregar-nodo valor 1 2 3))
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
         (define pos (list(lugar-numero (send search-box4 get-value) 1)) )
         (define pesos (list (string->number(send search-box4.1 get-value))))
         (define  binario  (list (boolCheck-box1)))
          (agregar-nodo (send search-box3 get-value); String
                        (list(lugar-numero (send search-box4 get-value) 1)); list al que me quiero conectar
                        (list (string->number(send search-box4.1 get-value))); list peso
                        (list (boolCheck-box1)))))); list direccion

; Crear conexion entre el nodo nuevo y dos existentes
(define (crearNodo2conexiones)
  (cond ((equal? (send search-box5.1 get-value) "")
         "Nada que hacer")
        (else
          (agregar-nodo (send search-box3 get-value); int el que estoy creando
                        
                        (list  (lugar-numero(send search-box4 get-value )1)
                               (lugar-numero(send search-box5 get-value )1)); list al que me quiero conectar
                        
                        (list ( string->number(send search-box4.1 get-value))
                              ( string->number(send search-box5.1 get-value))); list peso
                        
                        (list (boolCheck-box1) (boolCheck-box2)))))); list direccion

; Crear conexion entre el nodo nuevo y tres existentes
(define (crearNodo3conexiones)
  (cond ((equal? (send search-box6.1 get-value) "")
         "Nada que hacer")
        
        (else
          (agregar-nodo (send search-box3 get-value); int el que estoy creando
                        
                         (list  (lugar-numero(send search-box4 get-value )1)
                                (lugar-numero(send search-box5 get-value )1)
                                (lugar-numero(send search-box6 get-value )1)); list al que me quiero conectar
                        
                         (list ( string->number(send search-box4.1 get-value))
                               ( string->number(send search-box5.1 get-value))
                              ( string->number(send search-box6.1 get-value)));; list peso
                        
                        (list (boolCheck-box1) (boolCheck-box2) (boolCheck-box3))))
        )); list direccion
;______________________________________________________________________________________________________________________Extra;

(require racket/gui map-widget)
(define toplevel (new frame% [label "Map Demo"] [width 800] [height 800]))
(define panelMapa (new panel% [parent toplevel]
                             [border 0]
                             [vert-margin 0]
                             [spacing 0]
                             [alignment '(center center)]))
(define map (new map-widget% [parent panelMapa]))

(define (go)
  (send map move-to ( vector(string->number(send search-boxx get-value)) (string->number (send search-boxx2 get-value)                                                                                                      
  ))))
(define (irCasa)
   (send map set-track-current-location #t)
  )
(define (casa)
  (define color (make-color 0 135 36))
  (send map add-marker( vector(string->number(send search-boxx get-value)) (string->number (send search-boxx2 get-value))) "Casa "  1 color)
  (send map set-current-location  ( vector(string->number(send search-boxx get-value)) (string->number (send search-boxx2 get-value))))
  (send map set-track-current-location #t)
  )
(define (zoom)
(send map set-zoom-level (string->number(send search-boxx3 get-value))                                                                                         )
 )
(new button% [parent toplevel]
             [label "Ir a las cordenadas"]
             [callback (lambda (button event)
                         (go))])
(define search-boxx (new text-field% [parent toplevel]
                                    [label "Latitud     "]))
(define search-boxx2 (new text-field% [parent toplevel]
                               [label "Longitud     "]))
(new button% [parent toplevel]
             [label "Escoger como casa"]
             [callback (lambda (button event)
                         (casa))])
(new button% [parent toplevel]
             [label "Ir a casa"]
             [callback (lambda (button event)
                         (irCasa))]) 
(define search-boxx3 (new text-field% [parent toplevel]
                                    [label "Zoom (los valor van del 1 al 16) "]))
(new button% [parent toplevel]
             [label "Cambiar Zoom"]
             [callback (lambda (button event)
                         (zoom))])

; Changes Frame to MapScreen from ConfigurationScreen
(define (toExtraFromMenuScreen)
  (send toplevel show #t)
  (showMenu #f))

;______________________________________________________________________________________________________________________Run;

(showMenu #t)