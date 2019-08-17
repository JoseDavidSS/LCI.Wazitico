;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Logic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
>(define grafo null)

>(define (getGrafo)
   (cond ((null? grafo)
         (getGrafo_aux 1 1 '()))
         (else
          grafo)))

>(define (getGrafo_aux i j fila)
   (cond ((= i 16)
          grafo)
         ((= j 16)
          (begin
            (set! grafo (append grafo (list fila)))
            (getGrafo_aux (+ i 1) 1 '())))
         ((= i j)
          (getGrafo_aux i (+ j 1) (append fila (list 0))))
         ((or (and (= i 1) (= j 2)) (and (= i 6) (= j 11)) (and (= i 7) (= j 2)) (and (= i 8) (= j 7)) (and (= i 10) (= j 13)) (and (= i 12) (= j 9)) (and (= i 13) (= j 10)))
          (getGrafo_aux i (+ j 1) (append fila (list 3))))
         ((or (and (= i 1) (= j 3)) (and (= i 3) (= j 1)) (and (= i 8) (= j 9)) (and (= i 9) (= j 8)) (and (= i 10) (= j 9)) (and (= i 11) (= j 10)))
          (getGrafo_aux i (+ j 1) (append fila (list 5))))
         ((or (and (= i 2) (= j 3)) (and (= i 3) (= j 2)) (and (= i 3) (= j 5)) (and (= i 4) (= j 5)) (and (= i 4) (= j 6)) (and (= i 5) (= j 4)) (and (= i 8) (= j 12)) (and (= i 10) (= j 5)) (and (= i 11) (= j 13)) (and (= i 12) (= j 8)) (and (= i 12) (= j 14)) (and (= i 13) (= j 15)) (and (= i 14) (= j 12)))
          (getGrafo_aux i (+ j 1) (append fila (list 6))))
         ((and (= i 15) (= j 14))
          (getGrafo_aux i (+ j 1) (append fila (list 15))))
         (else
          (getGrafo_aux i (+ j 1) (append fila (list -1))))))

>(define (anadirAlGrafo idAgregar ids pesos bi)
   (cond ((or (< idAgregar 1) (null? ids) (null? pesos) (null? bi))
          grafo)
         (else
          (anadirAlGrafo_aux idAgregar ids pesos bi 1 '() ids pesos))))

>(define (anadirAlGrafo_aux idAgregar ids pesos bi j fila rIds rPesos)
   (cond ((= j idAgregar)
          (begin
            (set! grafo (append grafo (list (append fila (list 0)))))
            (anadirAlGrafo_aux2 idAgregar rIds rPesos bi '() 1 rIds rPesos bi grafo)))
         ((null? ids)
          (anadirAlGrafo_aux idAgregar rIds rPesos bi (+ j 1) (append fila (list -1)) rIds rPesos))
         ((= j (car ids))
          (anadirAlGrafo_aux idAgregar rIds rPesos bi (+ j 1) (append fila (list (car pesos))) rIds rPesos))
         (else
          (anadirAlGrafo_aux idAgregar (cdr ids) (cdr pesos) bi j fila rIds rPesos))))
          
>(define (anadirAlGrafo_aux2 idAgregar ids pesos bi nGrafo i rIds rPesos rBi rGrafo)
   (cond ((= i idAgregar)
          (begin
            (set! grafo (append nGrafo (list (car rGrafo))))))
         (else
          (begin
            (cond ((null? ids)
                   (anadirAlGrafo_aux2 idAgregar rIds rPesos rBi (append nGrafo (list (append (car rGrafo) (list -1)))) (+ i 1) rIds rPesos rBi (cdr rGrafo)))
                  ((and (= i (car ids)) (= 1 (car bi)))
                   (anadirAlGrafo_aux2 idAgregar rIds rPesos rBi (append nGrafo (list (append (car rGrafo) (list (car pesos))))) (+ i 1) rIds rPesos rBi (cdr rGrafo)))
                  (else
                   (anadirAlGrafo_aux2 idAgregar (cdr ids) (cdr pesos) (cdr bi) nGrafo i rIds rPesos rBi rGrafo)))))))

>(define (revisarAdyacentes nodo)
   (cond ((or (< nodo 1) (> nodo (largoGrafo)))
          '())
         (else
          (revisarAdyacentes_aux nodo 1 grafo))))

>(define (revisarAdyacentes_aux nodo i grafoAux)
   (cond ((= nodo i)
          (revisarAdyacentes_aux2 (car grafoAux) '() 1))
         (else
          (revisarAdyacentes_aux nodo (+ i 1) (cdr grafoAux)))))
         
>(define (revisarAdyacentes_aux2 fila adj j)
   (cond ((null? fila)
          adj)
         ((> (car fila) -1)
          (revisarAdyacentes_aux2 (cdr fila) (append adj (list j)) (+ j 1)))
         (else
          (revisarAdyacentes_aux2 (cdr fila) adj (+ j 1)))))

>(define (revisarPeso nodo1 nodo2)
   (cond ((or (< nodo1 1) (< nodo2 1) (> nodo1 (largoGrafo)) (> nodo2 (largoGrafo)))
          -1)
         ((= nodo1 nodo2)
          0)
         (else
          (revisarPeso_aux nodo1 nodo2 grafo 1))))

>(define (revisarPeso_aux n1 n2 grafoAux i)
   (cond ((= n1 i)
          (revisarPeso_aux2 n1 n2 1 (car grafoAux)))
         (else
          (revisarPeso_aux n1 n2 (cdr grafoAux) (+ i 1)))))

>(define (revisarPeso_aux2 n1 n2 j fila)
   (cond ((= n2 j)
          (car fila))
         (else
          (revisarPeso_aux2 n1 n2 (+ j 1) (cdr fila)))))

>(define (listaIds)
   (cond ((null? grafo)
          '())
         (else
          (listaIds_aux grafo 1))))

>(define (listaIds_aux rGrafo i)
   (cond ((null? rGrafo)
          '())
         (else
          (append (list i) (listaIds_aux (cdr rGrafo) (+ i 1))))))

>(define (largoGrafo)
   (cond ((null? grafo)
          0)
         (else
          (largoGrafo_aux grafo))))

>(define (largoGrafo_aux rGrafo)
   (cond ((null? rGrafo)
          0)
         (else
          (+ 1 (largoGrafo_aux (cdr rGrafo))))))

>(define (infoNodo nodo)
   (cond ((or (< nodo 1) (> nodo (largoGrafo)))
          '())
         (else
          (infoNodo_aux nodo (revisarAdyacentes 5) '()))))

>(define (infoNodo_aux nodo adj nAdj)
   (cond ((null? adj)
          (infoNodo_aux2 nodo nAdj '() nAdj))
         ((= nodo (car adj))
          (infoNodo_aux nodo (cdr adj) nAdj))
         (else
          (infoNodo_aux nodo (cdr adj) (append nAdj (list (car adj)))))))

>(define (infoNodo_aux2 nodo adj pesos rAdj)
   (cond ((null? adj)
          (append (list rAdj) (list pesos)))
         (else
          (infoNodo_aux2 nodo (cdr adj) (append pesos (list (revisarPeso nodo (car adj)))) rAdj))))

>(define (iniciarDistancia distancia i largo inicio)
   (cond ((> i largo)
          distancia)
         ((= i inicio)
          (iniciarDistancia (append distancia (list 0)) (+ i 1) largo inicio))
         (else
          (iniciarDistancia (append distancia (list 999)) (+ i 1) largo inicio))))

>(define (iniciarVisitados visitados i largo)
   (cond ((> i largo)
          visitados)
         (else
          (iniciarVisitados (append visitados (list 0)) (+ i 1) largo))))

>(define (iniciarCamino camino i largo inicio)
   (cond ((> i largo)
          camino)
         ((= i inicio)
          (iniciarCamino (append camino (list -1)) (+ i 1) largo inicio))
         (else
          (iniciarCamino (append camino (list 0)) (+ i 1) largo inicio))))

>(define (buscarElemento i posicion lista)
   (cond ((null? lista)
          -1)
         ((= i posicion)
          (car lista))
         (else
          (buscarElemento (+ i 1) posicion (cdr lista)))))

>(define (modificarElemento nuevoElemento i posicion lista nuevaLista)
   (cond ((null? lista)
          nuevaLista)
         ((= i posicion)
          (modificarElemento nuevoElemento (+ i 1) posicion (cdr lista) (append nuevaLista (list nuevoElemento))))
         (else
          (modificarElemento nuevoElemento (+ i 1) posicion (cdr lista) (append nuevaLista (list (car lista)))))))

>(define (modificarLista listas nuevaListas i numeroDeLista nuevoElemento posicion)
   (cond ((null? listas)
          nuevaListas)
         ((= i numeroDeLista)
          (modificarLista (cdr listas) (append nuevaListas (list (modificarElemento nuevoElemento 1 posicion (car listas) '()))) (+ 1 i) numeroDeLista nuevoElemento posicion))
         (else
          (modificarLista (cdr listas) (append nuevaListas (list (car listas))) (+ i 1) numeroDeLista nuevoElemento posicion))))

>(define (dijkstra inicio fin)
   (cond ((or (< inicio 1) (< fin 1) (> inicio (largoGrafo)) (> fin (largoGrafo)))
          '())
         (else
          (dijkstra_aux inicio fin (largoGrafo) (append (list (iniciarDistancia '() 1 (largoGrafo) inicio)) (list (iniciarVisitados '() 1 (largoGrafo))) (list (iniciarCamino '() 1 (largoGrafo) inicio))) 1))))

>(define (dijkstra_aux inicio fin numNodos listas i)
   (cond ((= i numNodos)
          (dijkstra_aux4 numNodos inicio fin (car listas) (caddr listas) '()))
         (else
          (dijkstra_aux inicio fin numNodos (dijkstra_aux2 numNodos listas -1 999 1) (+ i 1)))))

>(define (dijkstra_aux2 numNodos listas nodoCercano distanciaCorta j)
   (cond ((> j numNodos)
          (dijkstra_aux3 numNodos (modificarLista listas '() 1 2 1 nodoCercano) nodoCercano distanciaCorta (revisarPeso nodoCercano 1) 1))
         ((and (= 0 (buscarElemento 1 j (cadr listas))) (< (buscarElemento 1 j (car listas)) distanciaCorta))
          (dijkstra_aux2 numNodos listas j (buscarElemento 1 j (car listas)) (+ j 1)))
         (else
          (dijkstra_aux2 numNodos listas nodoCercano distanciaCorta (+ j 1)))))

>(define (dijkstra_aux3 numNodos listas nodoCercano distanciaCorta peso k)
   (cond ((> k numNodos)
          listas)
         ((and (> peso 0) (< (+ distanciaCorta peso) (buscarElemento 1 k (car listas))))
          (dijkstra_aux3 numNodos (append (list (modificarElemento (+ distanciaCorta peso) 1 k (car listas) '())) (list (cadr listas)) (list (modificarElemento nodoCercano 1 k (caddr listas) '()))) nodoCercano distanciaCorta (revisarPeso nodoCercano (+ k 1)) (+ k 1)))
         (else
          (dijkstra_aux3 numNodos listas nodoCercano distanciaCorta (revisarPeso nodoCercano (+ k 1)) (+ k 1)))))

>(define (dijkstra_aux4 numNodos inicio fin distancias caminos caminoFinal)
   (cond ((= fin -1)
          caminoFinal)
         (else
          (dijkstra_aux4 numNodos inicio (buscarElemento 1 fin caminos) distancias caminos (append (list fin) caminoFinal)))))

>(anadirAlGrafo 1 '(0) '(0) '(0))
>(anadirAlGrafo 2 '(1) '(70) '(0))
>(anadirAlGrafo 3 '(1 2) '(90 76) '(0 1))
>(anadirAlGrafo 4 '(3 2) '(12 65) '(0 1))
>(anadirAlGrafo 5 '(1 2 4) '(10 23 34) '(1 1 1))
>(getGrafo)
;>(revisarAdyacentes 5)
;>(revisarPeso 3 4)
;>(listaIds)
;>(infoNodo 5)
;>(largoGrafo)
>(dijkstra 3 4)