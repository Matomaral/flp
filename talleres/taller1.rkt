#lang eopl
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



