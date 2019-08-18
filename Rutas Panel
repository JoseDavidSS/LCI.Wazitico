#lang racket/gui
;; ESTAS SON LAS VARIABLES PARA SABER DONDE SE ENCUENTRAN LOS NODOS
;;(define nodos (hash "1" '("Cartago" 100 100) "2" '("SanJose" 400 400)))
(define cantidad-nodos 1 )
(define nodos (make-hash))
;;Esta es la funcion que me actualiza el  hast table de nodos con sus nombres
(define (agregar-nodo nombre x y)
  (hash-set! nodos (number->string cantidad-nodos) (list nombre x y))
  (set! cantidad-nodos (+ cantidad-nodos 1))
  )
;;Esta es la funcion que dibujara los nodos despues haber agregado uno o varios
(define (dibujar-nodos cantidad-de-nodos-restantes)
  (cond
   [(not (zero?  cantidad-de-nodos-restantes ))
    (define valores (hash-ref nodos (number->string cantidad-de-nodos-restantes)))
    (define nombre-del-nodo(first valores))
    (define posicion-x (first(rest valores) ))
    (define posicion-y   (first(rest(rest valores) )))
    (dibujar-nodo posicion-x posicion-y 100 mapa nombre-del-nodo)
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
     (dibujar-linea (first pos-inicial) (first(rest pos-inicial))(first pos-final)(first(rest pos-final)) mapa "RED" 13)
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
(define no-pen (make-object pen% "BLACK" 1 'transparent))
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

  ; Make a 300 x 300 frame
  (define frame (new frame% [label "Pantalla de Mapa "]
                            [width 800]
                            [height 800]))
  ; Make the drawing area
  (define mapa (new canvas% [parent frame]))
  ; Get the canvas's drawing context

  (send frame show #t)