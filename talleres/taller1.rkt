#lang eopl
;; Punto 2
;; list-tails :
;; Proposito:
;; retornar en una lista todas las
;;sublistas de los elementos consecutivos de la lista
;;
(define list-tails
(lambda (L)
(if (null? L)empty
(cons (cons(car L)(cdr L)) (list-tails (cdr L)))     
)))

;; Pruebas
(list-tails '(1 2 3 4 5))
(list-tails '(1 a (e 4) 5 v))
(list-tails '(a b c d e f g))
(list-tails '(7 8 9 10 11 9))

;; Punto 3
;; sublist :
;; Proposito:
;; sublist retorna la sublista entre el elemento en posicion inicial (posicionInicial) y el elemento en posicion final (posicionFinal)
;;

(define (sublist L posicionInicial posicionFinal)
  (cond
    [(and (= 0 posicionInicial)(= 0 posicionFinal)) (cons (car L) empty)]
    [(= 0 posicionInicial) (cons (car L) (sublist (cdr L) posicionInicial (- posicionFinal 1)))]
    [else (sublist (cdr L) (- posicionInicial 1) (- posicionFinal 1))] 
  )
 )

;; Pruebas
(sublist '(a b c d e) 1 3)
(sublist '((a b) c a b c 9) 3 4)

;; Punto 5
;; auxsuma-fibo
;; crea la sucesion de fibonacci

(define auxsuma-fibo
  (lambda (n)
    (cond
      [(= n 0) 0]
      [(= n 1) 1]
      [else (+ (auxsuma-fibo (- n 1))(auxsuma-fibo (- n 2)))]
      )))

;; auxlista-fibo
;; crea una lista con cada numero de fibonacci

(define auxlista-fibo
  (lambda (n)
    (if (>= n 0)
        (cons (auxsuma-fibo n) (auxlista-fibo (- n 1)))empty
        ))             
  )

;; list-fibo :
;; Proposito:
;; recibe como argumento un numero entero n,
;; y retorna una lista ascendente con los n-terminos de la sucesion Fibonacci.


(define list-fibo
  (lambda (n)
    (reverse (auxlista-fibo n))))

;; Pruebas
(list-fibo 6)
(list-fibo 1)
(list-fibo 9)
(list-fibo 20)

;; Punto 6
;; aux-factorial:
;; Proposito:
;; aux-factorial calcula el factorial de n
;;

(define (aux-factorial n)
  (cond
    [(= 0 n) 1]
    [else (* n (aux-factorial (- n 1)))]
  )
)

;; Pruebas
(aux-factorial 7)
(aux-factorial 3)

;; list-facts-two-increase:
;; Proposito:
;; list-facst-two-increase genera una lista factoriales, partiendo desde el (inicio) hasta el (final) pero donde cada numero a partir de inicio (sin incluirle)
;; es aplicado un factor por (factor (es una funcion)) y donde el ultimo numero es el factorial de (final)
;;

(define (list-facts-two-increase final factor inicio)
  (cond
    [(>= inicio final) (cons (aux-factorial final) empty)]
    [else (cons (aux-factorial inicio) (list-facts-two-increase final factor (factor inicio)) )]
  )
)

;; Pruebas:
(list-facts-two-increase 8 (lambda (x) (* 2 x)) 2)
(list-facts-two-increase 7 (lambda (x) (+ 1 (* 2 x))) 1)

;; list-facts-two:
;; Proposito:
;; list-facts-two recibe como argumento un numero entero n, y retorna una lista incremental de factoriales dobles. Un
;; factorial doble inicia en 1! si n es impar y continuara calculando los factoriales de los numeros impares hasta n!.
;; Si n es par, entonces inicia en 2! y continuara calculando los factoriales de numeros pares hasta n!.
;;

(define (list-facts-two n)
  (cond
    [(= 0(modulo n 2))  (list-facts-two-increase n (lambda (x) (* 2 x)) 2)  ]
    [else  (list-facts-two-increase n (lambda (x) (+ 1 (* 2 x))) 1)]
  )
)

;; Pruebas
(list-facts-two 5)
(list-facts-two 8)

;; Punto 8
;; flatten :
;; Proposito:
;; Recibe como argumento una lista L y devuelve la lista eliminando los parentesis internos


(define (flatten L)
  (cond ((null? L) '())
        ((pair? L)
         (append (flatten (car L)) (flatten (cdr L))))
        (else (list L))))

;;Pruebas

(flatten '((a b) c (((d)) e)))
(flatten '((a) () (2 ()) () (c)))
(flatten '((a b c  (d e f)())))
(flatten '((a b c (a)(e i 2)(3)  (d e f)())))

;; Punto 9:
;; every?:
;; Proposito:
;; every? retorna #t si TODOS los elementos de la lista (lista) satisfacen el predicado (predicado). Devuelve #f en caso contrario

(define (every? predicado lista)
  (cond
    [(equal? lista empty) #t]
    [(predicado (car lista)) (every? predicado (cdr lista))]
    [else #f]
  )
)

;; Pruebas:

(every? symbol? '(a b c 3 e))
(every? number? '(1 2 3 5 4))

;; Punto 11
;; merge
;; Proposito: retorna una lista ordenada de todos los elementos de las listas L1 y L2.

(define (merge L1 L2)
  (cond
    [(null? L1) L2]    
    [(null? L2) L1]    
    [(< (car L1) (car L2))    
     (cons (car L1) (merge (cdr L1) L2))]     
    [else (cons (car L2) (merge L1 (cdr L2)))]))
     
;; PRUEBAS

(merge '(1 4) '(1 2 8))
(merge '(1 4) '())
(merge '() '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))

;; Punto 12
;; zip:
;; Proposito:
;; zip retorna una lista donde la posicion n-esima corresponde al resultado de aplicar la funcion (funcion-binaria)
;; sobre los elementos en la posicion n-esima en la primer lista (primerLista) y la segunda lista(segundaLista)
;;

(define (zip  funcion-binaria primerLista segundaLista)
  (cond
    [(equal? primerLista empty) empty]
    [else (cons (funcion-binaria (car primerLista) (car segundaLista)) (zip funcion-binaria (cdr primerLista) (cdr segundaLista)))]
  )
)

;; Pruebas:
(zip + '(1 4) '(6 2))
(zip * '(11 5 6) '(10 9 8))

;; Punto 15
;; hermite:
;; Proposito:
;; hermite recibe como entrada dos argumentos: el orden (orden) del polinomio de Hermite y la abscisa (abscisa). Y retorna el resultado del calculo.
;;

(define (hermite orden abscisa)
  (cond
    [(= 0 orden) 1]
    [(= 1 orden) (* 2 abscisa)]
    [else (- (*(* 2 abscisa) (hermite (- orden 1) abscisa)) (* (* 2 (- orden 1)) (hermite (- orden 2) abscisa)))]
  )
)

;; Pruebas
(hermite 5 2)
(hermite 5 8)

;; Punto 16
;; FUNCIONES AUXILIARES
;; bubble-algorithm
;; La funcion ejecuta el algoritmo bubble-sort con los pasos correspondientes explicados a continuación.

(define (bubble-algorithm L)
    (if (null? (cdr L))L  ;; Si no hay elementos a la derecha para comparar, se retorna la lista como está.             
        (if (< (car L) (cadr L)) ;; Se toma el primer elemento y se compara si es menor que el siguiente
            (cons (car L) (bubble-algorithm (cdr L))) ;; en este caso es menor, por lo tanto se deja el elemento en la posicion que esta y se va creando la lista ordenada                 
            (cons (cadr L) (bubble-algorithm (cons (car L) (cddr L))))))) ;; en este caso es mayor, por lo tanto con un pequeño algoritmo utilzando el manejo de listas intercambiamos de posicion los dos elementos comparados.
;;con este algoritmo hemos puesto el cadr(elemento de la derecha) de primero y con el cons car y cdddr hemos dejado el elemento de la izquierda al final con el resto de la lista que no se ha comparado.

;;bubble-sort-aux
;; Se necesita una funcion auxiliar que lleve un conteo de cuantas pasadas se llevan, ya que para que el algoritmo
;; sea correcto se deben realizar n-1 pasadas dependiendo del tamaño de la lista, nuestro primer algoritmo no se ejecutaba las veces necesarias recursivamente.

(define (bubble-sort-aux tamano Lista)    
    (cond ((= tamano 1) (bubble-algorithm Lista))   
          (else (bubble-sort-aux (- tamano 1) (bubble-algorithm Lista)))))


;; bubble-sort
;; Proposito:
;; recibe como entrada una lista de numeros L y retorna la lista L ordenada de manera ascendente.

(define (bubble-sort L) 
    (bubble-sort-aux (length L) L))


;; Pruebas
(bubble-sort '(5 10 9 8 7))
(bubble-sort '(8 2 1 6 8))
(bubble-sort '(8 2 5 2 3))
(bubble-sort '(9 8 7 6 -1 5 -4))

;; Punto 17
;; aux-path:
;; Proposito:
;; aux-path busca un numero (n) en un arbol binario (BST) y retorna una lista con el camino a tomar, a parte toma otro argumento llamado camino, el cual
;; almacena el recorrido que lleva la funcion y se retorna si se encuentra (n)
;;

(define (aux-path n BST camino)
  (cond
    [(null? BST) empty]
    [(= (car BST) n) camino]
    [else (append (aux-path n (car (cdr BST)) (append camino (cons 'left empty))) (aux-path n (car (cddr BST)) (append camino (cons 'right empty))))]
  )
)

;; Pruebas:
(aux-path 13 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))) empty)
(aux-path 17 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))) empty)


;; path:
;; proposito:
;; path recibe como entrada dos parametros: un numero n (n) y un arbol binario de busqueda (BST) y retorna una lista con la ruta a tomar
;; indicada por cadenas left y right, hasta llegar al numero n recibido. Retorna vacio si n esta en el nodo raiz.
;;

(define (path n BST) (aux-path n BST empty))

;; Pruebas:
(path 13 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ())()) (31 () ()))))



