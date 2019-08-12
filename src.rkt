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

>(define (revisarAdyacentes nodo)
   (cond ((or (< nodo 1) (> nodo 15))
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

>(getGrafo)
>(revisarAdyacentes 1)
>(revisarPeso 1 5)