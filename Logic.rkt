#lang racket/base

;Variable del grafo
>(define grafo null)

;Función que se encarga de añadir un nodo al grafo
;Recibe como argumentos el id del nodo a agregar, una lista con las ids de los nodos a los que va a estar conectado,
;una lista con los pesos que van a tener cada arista entre nodos y una última lista indicando si la conexión va a ser bidireccional.
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

;Función que recibe una id de un nodo y retorna una lista con todos los nodos adyacentes a este, incluyendose a si mismo
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

;Función que recibe 2 ids de dos nodos y retorna cuál es el peso que tiene la arista entre ellos.
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

;Función que retorna una lista con todos los ids de los nodos que hay actualmente en el grafo.
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

;Función que retorna un número indicando la cantidad de nodos que hay actualmente en el grafo.
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

;Función que recibe una id de un nodo como argumento, y retorna una lista con 3 listas dentro de esta,
;la primera indica todos los nodos adyacentes al nodo recibido (sin contarse a si mismo), la segunda
;indica los pesos que hay entre todos estos nodos adyacentes y la tercera indica si hay nodos bidireccionales
;entre el nodo recibido y los adyacentes a este.
>(define (infoNodo nodo)
   (cond ((or (< nodo 1) (> nodo (largoGrafo)))
          '())
         (else
          (infoNodo_aux nodo (revisarAdyacentes nodo) '()))))

>(define (infoNodo_aux nodo adj nAdj)
   (cond ((null? adj)
          (infoNodo_aux2 nodo nAdj '() nAdj))
         ((= nodo (car adj))
          (infoNodo_aux nodo (cdr adj) nAdj))
         (else
          (infoNodo_aux nodo (cdr adj) (append nAdj (list (car adj)))))))

>(define (infoNodo_aux2 nodo adj pesos rAdj)
   (cond ((null? adj)
          (infoNodo_aux3 nodo rAdj rAdj pesos '()))
         (else
          (infoNodo_aux2 nodo (cdr adj) (append pesos (list (revisarPeso nodo (car adj)))) rAdj))))

>(define (infoNodo_aux3 nodo adj rAdj pesos bidireccion)
   (cond ((null? adj)
          (append (list rAdj) (list pesos) (list bidireccion)))
         ((> (revisarPeso (car adj) nodo) 0)
          (infoNodo_aux3 nodo (cdr adj) rAdj pesos (append bidireccion (list 1))))
         (else
          (infoNodo_aux3 nodo (cdr adj) rAdj pesos (append bidireccion (list 0))))))

;Función que se encarga de inicializar una lista que almacenará distancias para el algoritmo de Dijkstra,
;recibe como argumentos una lista vacía (que será la retornada), un contador i (iniciando en 1), el
;largo que es la cantidad de nodos del grafo y la id del nodo de inicio, la cual se le colocará una distancia de 0.
>(define (iniciarDistancia distancia i largo inicio)
   (cond ((> i largo)
          distancia)
         ((= i inicio)
          (iniciarDistancia (append distancia (list 0)) (+ i 1) largo inicio))
         (else
          (iniciarDistancia (append distancia (list 999)) (+ i 1) largo inicio))))

;Función que se encarga de inicializar una lista que almacenará aquellos nodos que ya fueron visitados por el algoritmo
;de Dijkstra, recibe como argumentos una lista vacía (que será la retornada), un contador i (iniciando en 1) y el largo
;que es la cantidad de nodos del grafo.
>(define (iniciarVisitados visitados i largo)
   (cond ((> i largo)
          visitados)
         (else
          (iniciarVisitados (append visitados (list 0)) (+ i 1) largo))))

;Función que se encarga de inicializar una lista que almacenará el camino a seguir para llegar al destino utilizando la
;ruta más corta, recibe como argumentos una lista vacía (que será la retornada), un contador i (iniciando en 1), el largo
;que es la cantidad de nodos del grafo y el id del nodo de inicio.
>(define (iniciarCamino camino i largo inicio)
   (cond ((> i largo)
          camino)
         ((= i inicio)
          (iniciarCamino (append camino (list -1)) (+ i 1) largo inicio))
         (else
          (iniciarCamino (append camino (list 0)) (+ i 1) largo inicio))))

;Función que se encarga de buscar un elemento de una lista, recibe como argumentos un contador i (iniciando en 1), la
;posición en la que se encuentra el elemento deseado y la lista a la cuál se le aplicará la búsqueda del elemento.
>(define (buscarElemento i posicion lista)
   (cond ((null? lista)
          -1)
         ((= i posicion)
          (car lista))
         (else
          (buscarElemento (+ i 1) posicion (cdr lista)))))

;Función que se encarga de modificar un elemento de una lista, recibe como argumentos el elemento nuevo que se insertará,
;un contador i (iniciando en 1), la posición del elemento a ser cambiado, la lista a ser modificada y una nueva lista que será
;la que contenga el elemento modificado (será la retornada).
>(define (modificarElemento nuevoElemento i posicion lista nuevaLista)
   (cond ((null? lista)
          nuevaLista)
         ((= i posicion)
          (modificarElemento nuevoElemento (+ i 1) posicion (cdr lista) (append nuevaLista (list nuevoElemento))))
         (else
          (modificarElemento nuevoElemento (+ i 1) posicion (cdr lista) (append nuevaLista (list (car lista)))))))

;Función que se encarga de modificar una lista dentro de una lista de listas (la lista de listas tendrá por defecto la lista de distancias,
;la lista de visitados y la lista de caminos), recibe como argumentos la lista de listas, una lista vacía (que será la que contenga la nueva información),
;un contador i (iniciando en 1), el número de la lista a realizar la modificación, el elemento a ser introducido en la lista y la posición donde se quiera
;insertar.
>(define (modificarLista listas nuevaListas i numeroDeLista nuevoElemento posicion)
   (cond ((null? listas)
          nuevaListas)
         ((= i numeroDeLista)
          (modificarLista (cdr listas) (append nuevaListas (list (modificarElemento nuevoElemento 1 posicion (car listas) '()))) (+ 1 i) numeroDeLista nuevoElemento posicion))
         (else
          (modificarLista (cdr listas) (append nuevaListas (list (car listas))) (+ i 1) numeroDeLista nuevoElemento posicion))))

;Función de dijkstra que recibe dos ids de 2 nodos del grafo y se encarga de buscar la ruta más corta entre ellos, retornará una lista de listas con la distacia
;más corta entre los nodos y otra lista con el camino a seguir para ir por la ruta más corta.
>(define (dijkstra inicio fin)
   (cond ((or (< inicio 1) (< fin 1) (> inicio (largoGrafo)) (> fin (largoGrafo)))
          '())
         (else
          (dijkstra_aux inicio fin (largoGrafo) (append (list (iniciarDistancia '() 1 (largoGrafo) inicio)) (list (iniciarVisitados '() 1 (largoGrafo))) (list (iniciarCamino '() 1 (largoGrafo) inicio))) 1))))

>(define (dijkstra_aux inicio fin numNodos listas i)
   (cond ((= i numNodos)
          (dijkstra_aux4 numNodos inicio fin (car listas) (caddr listas) '() (buscarElemento 1 fin (car listas))))
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

>(define (dijkstra_aux4 numNodos inicio fin distancias caminos caminoFinal distanciaARecorrer)
   (cond ((= fin -1)
          (cons (list distanciaARecorrer) (list caminoFinal)))
         (else
          (dijkstra_aux4 numNodos inicio (buscarElemento 1 fin caminos) distancias caminos (append (list fin) caminoFinal) distanciaARecorrer))))
(provide (all-defined-out))