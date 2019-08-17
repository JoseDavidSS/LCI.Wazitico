;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname src) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
   (cond ((< nodo 1)
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
   (cond ((or (< nodo1 1) (< nodo2 1) (> nodo1 15) (> nodo2 15))
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

>(anadirAlGrafo 1 '(0) '(0) '(0))
>(anadirAlGrafo 2 '(1) '(70) '(0))
>(anadirAlGrafo 3 '(1 2) '(90 76) '(0 1))
>(anadirAlGrafo 4 '(3) '(12) '(0))
>(anadirAlGrafo 5 '(1 2 4) '(10 23 34) '(1 1 1))
>(getGrafo)
>(revisarAdyacentes 5)
>(revisarPeso 3 4)