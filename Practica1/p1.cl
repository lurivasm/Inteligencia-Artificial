;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APARADO 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; producto-escalar (x y)
;;; Calcula el producto escalar de dos vectores recursivamente
;;; 
;;; INPUT : x vector como lista
;;;         y vector como lista
;;; OUTPUT : el producto escalar de x e y
;;;
(defun producto-escalar (x y)
	(cond
		((null x) 0)
		((null y) 0)
		(T
			(+ 
				(* (first x) (first y))
				(producto-escalar (rest x) (rest y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; suma-cuadrados (x)
;;; Calcula la norma de x al cuadrado recursivamente
;;; 
;;; INPUT : x vector como lista
;;; OUTPUT : la norma de x al cuadrado
;;;
(defun suma-cuadrados (x)
	(if (null x)
		0
		(+
			(* (first x) (first x))
			(suma-cuadrados (rest x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-rec (x y)
	(let
		((sx (suma-cuadrados x))
		 (sy (suma-cuadrados y)))
		(cond
			((and (= 0 sx)
				  (= 0 sy))
				0)
			((= 0 sx) 1)
			((= 0 sy) 1)
			(T 	(- 1
					(/ (producto-escalar x y)
						(*  (sqrt sx)
							(sqrt sy))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; producto-escalar-map (x y)
;;; Calcula el producto escalar de dos vectores con mapcar
;;; 
;;; INPUT : x vector como lista
;;;         y vector como lista
;;; OUTPUT : el producto escalar de x e y
;;;
(defun producto-escalar-map (x y)
	(apply #'+
		(mapcar #'* x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; suma-cuadrados-map (x)
;;; Calcula la norma de x al cuadrado con mapcar
;;; 
;;; INPUT : x vector como lista
;;; OUTPUT : la norma de x al cuadrado
;;;
(defun suma-cuadrados-map (x)
	(apply #'+
		(mapcar #'* x x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;
(defun cosine-distance-mapcar (x y)
	(let
		((sx (suma-cuadrados-map x))
		 (sy (suma-cuadrados-map y)))
		(cond
			((and (= 0 sx)
				  (= 0 sy))
				0)
			((= 0 sx) 1)
			((= 0 sy) 1)
			(T 	(- 1
					(/ (producto-escalar-map x y)
						(*  (sqrt sx)
							(sqrt sy))))))))

(cosine-distance-mapcar '(1 2) '(1 2 3)) ;;; --> 0.40238577
(cosine-distance-mapcar nil '(1 2 3)) ;;; --> 1
(cosine-distance-mapcar '() '()) ;;; --> 0
(cosine-distance-mapcar '(0 0) '(0 0)) ;;; --> 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; APARTADO 1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; vectores-validos
;;;;;; Devuelve un lista con los vectores validos según la confianza
;;;;;; INPUT:  vector : vector de la categoria
;;;;;;         lista : lista de vectores
;;;;;;         confianza : Nivel de confianza
;;;;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;;;;         categoria es superior al nivel de confianza 
;;;;;;
(defun vectores-validos (vector lista confianza)
  (let
      ((cosx (cosine-distance-mapcar vector  (car lista))))
    
  (cond
   ((null (car lista)) nil)
   ((< (- 1 cosx) confianza) 
          (vectores-validos vector (cdr lista) confianza))
   (t (cons (car lista) (vectores-validos vector (cdr lista) confianza))))))
          


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lista vector de vectores
;;;         confianzal: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;
(defun order-vectors-cosine-distance (vector lista &optional (confianza 0))
  (cond 
   ((null lista) nil)
   ((null vector) nil)
    (t (sort (copy-list (vectores-validos vector lista confianza))
        #'(lambda(x y) (< (cosine-distance-mapcar vector x) (cosine-distance-mapcar vector y)))))))                
	                     


(order-vectors-cosine-distance '(1 2 3) '((32 454 123) (133 12 1) (4 2 2)) 0.5) ;; ---> ((4 2 2) (32 454 123))

(order-vectors-cosine-distance '(1 2 3) '((32 454 123) (133 12 1) (4 2 2)) 0.3) ;; ---> ((4 2 2) (32 454 123) (133 12 1))

(order-vectors-cosine-distance '(1 2 3) '((32 454 123) (133 12 1) (4 2 2)) 0.99) ;; ---> NIL




(order-vectors-cosine-distance '(1 2 3) '()) ;;; --> NIL

(order-vectors-cosine-distance '() '((4 3 2) (1 2 3))) ;;; --> NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;						 
;;; APARTADO 1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ordenar-categorias (categorias texto funcion-distancia)
;;; Ordena las categorias segun su semejanza al vector .
;;;
;;; INPUT : categorias: vector de vectores, representado como
;;;                     una lista de listas
;;;         texto:      vector a comparar
;;;         funcion-distancia: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
( defun ordenar-categorias (categorias texto funcion-distancia)
		(cond 
			((null categorias) nil)
			((null texto) nil)
			(t 
				(sort (copy-list categorias)
					#'(lambda(x y) (< (funcall funcion-distancia (cdr texto) (cdr x)) (funcall funcion-distancia (cdr texto) (cdr y))))))))     




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;
( defun get-vectors-category (categorias textos funcion-distancia)
		(let 
			((cat (car (ordenar-categorias categorias (car textos) funcion-distancia))))
		(cond
			((null cat) nil)
			(t (cons 
					(list (car cat) (funcall funcion-distancia (cdr cat) (cdr (car textos)))) 
					(get-vectors-category categorias (cdr textos) funcion-distancia))))))


(get-vectors-category '((1 43 23 12) (2 33 54 24)) '((1 3 22 134) (2 43 26 58)) #'cosine-distance-rec) ;;; --> ((2 0.5101813) (1 0.18444914))
(get-vectors-category '((1 43 23 12) (2 33 54 24)) '((1 3 22 134) (2 43 26 58)) #'cosine-distance-mapcar) ;;; --> ((2 0.5101813) (1 0.18444914))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; APARTADO 1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(get-vectors-category '(()) '(()) #'cosine-distance-rec) ;;; --> NIL
(get-vectors-category '(()) '(()) #'cosine-distance-mapcar) ;;; --> NIL
(get-vectors-category '((1 4 2) (2 1 2)) '((1 1 2 3)) #'cosine-distance-rec) ;;; --> ((2 0.40238577))
(get-vectors-category '((1 4 2) (2 1 2)) '((1 1 2 3)) #'cosine-distance-mapcar) ;;; --> ((2 0.40238577))
(get-vectors-category '(()) '((1 1 2 3) (2 4 5 6)) #'cosine-distance-rec) ;;; --> NIL
(get-vectors-category '(()) '((1 1 2 3) (2 4 5 6)) #'cosine-distance-mapcar) ;;; --> NIL




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 2.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; es-raiz
;;; Devuelve si x es raiz de f (i.e. f(x) = 0) con cierta tolerancia
;;;
;;; INPUT : x : punto en el que evaluamos f
;;;         tol : tolerancia
;;; OUTPUT : T si |x| <= tol
;;;          nil en otro caso
(defun es-raiz (x tol)
  (<= (abs x) tol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cambia
;;; Devuelve x - f(x)/df(x)
;;;
;;; INPUT : x : punto en el que evaluamos f y df
;;;         f : f(x) ya evaluado
;;;  	    df : derivada de f df(x) ya evaluado
;;; OUTPUT : x - f(x)/df(x)
;;; Suponemos que ya se ha comprobado que df(x) no es 0
( defun cambia (x f df)
   (- x 
    (/ f df)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;;         df: derivada de f
;;;         max-iter: maximo numero de iteraciones
;;;         x0: estimacion inicial del cero (semilla)
;;;         tol: tolerancia para convergencia (parametro opcional)
;;; OUTPUT: estimacion del cero de f o NIL si no converge
;;;
( defun newton (f df max-iter x0 &optional (tol 0.001))
  (let
      ((fx (funcall f x0))
       (dfx (funcall df x0)))
    (cond
     ((<= max-iter 0) '(nil))          ;; Ultima iteracion
     ((= dfx 0.0) '(nil))              ;; No podemos dividir por 0
     ((es-raiz (/ fx dfx) tol)      ;; Si es raiz devolvemos x0
						X0)  
     (t (newton f df (- max-iter 1) 
                            (cambia x0 fx dfx) 
                                      tol)))))
	

(newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 3.0) ;;---> 4.0

(newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 0.6) ;;---> 1.0

(newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 30 -2.5) ;;---> -3.0

(newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
           #'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 10 100) ;;---> NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 2.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT: f : funcion de la que se desea encontrar un cero
;;;        df : derivada de f
;;;        max-iter : maximo numero de iteraciones
;;;        semillas : semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: el primer cero de f que se encuentre , o NIL si se diverge
;;;          para todas las semillas
;;;
(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (cond
   ((null semillas) nil)  ;; Lista vacia
   ((let ((n (newton f df max-iter (first semillas) tol)))
      (not (null n)) n))
	(T 
         (one-root-newton f df max-iter (rest semillas) tol))))

(one-root-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(0.6 3.0 -2.5)) ;; -->1.0

(one-root-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(3.0 -2.5)) ;; --> 4.0

(one-root-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 1 '(3.0 -2.5)) ;;---> NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 2.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT: f: funcion de la que se desea encontrar un cero
;;;        df: derivada de f
;;;        max-iter: maximo numero de iteraciones
;;;        semillas: semillas con las que invocar a Newton
;;;        tol : tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT: las raices que se encuentren para cada semilla o nil
;;;          si para esa semilla el metodo no converge
;;;
(defun all-roots-newton (f df max-iter semillas &optional ( tol 0.001))
	(cond
		((null semillas) nil)
		(T
			(cons (newton f df max-iter (first semillas) tol)
				(all-roots-newton f df max-iter (rest semillas) tol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-not-nil-roots-newton
;;; Convierte la salida de all-roots-newton a una lista sin nil
;;;
;;; INPUT: f : funcion de la cual son raices
;;;        df : derivada de f
;;;        max-iter : nº maximo de iteraciones
;;;        raices : lista de raices
;;; OUTPUT: lista de semillas
(defun list-not-nil-roots-newton
	(mapcan #'list (all-roots-newton f df max-iter semillas &optional ( tol 0.001))))


(all-roots-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(0.6 3.0 -2.5)) ;;---> (1.0 4.0 -3.0)

(all-roots-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 20 '(0.6 3.0 10000.0)) ;;---> (1.0 4.0 nil)

(all-roots-newton #'(lambda(x) (* (- x 4) (- x 1) (+ x 3)))
#'(lambda (x) (- (* x (- (* x 3) 4)) 11)) 1 '(0.6 3.0 -2.5)) ;;---> (nil nil nil)

	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APARTADO 3.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst (elt lst)
	(mapcar #'(lambda(x) (list elt x)) lst))


(combine-elt-lst 'a '(1 2 3));;; -> ((A 1) (A 2) (A 3))
(combine-elt-lst 'a nil)     ;;; -> nil
(combine-elt-lst nil nil)    ;;; -> nil
(combine-elt-lst nil '(a b)) ;;; -> ((nil a) (nil b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APARTADO 3.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst (lst1 lst2)
	(mapcan #'(lambda(x) (combine-elt-lst x lst2)) lst1))


(combine-lst-lst '(a b c) '(1 2)) ;;; -> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))
(combine-lst-lst nil nil)         ;;; -> nil
(combine-lst-lst '(a b c) nil)    ;;; -> nil
(combine-lst-lst nil '(a b c))    ;;; -> nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APARTADO 3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst-v2
;;; Combina un elemento dado con todos los elementos de una lista
;;; pero con cons para quitar nil
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista
(defun combine-elt-lst-v2 (elt lst)
  (mapcar #'(lambda(x) (cons elt x)) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst-v2
;;; Calcula el producto cartesiano de dos listas pero con cons
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas
(defun combine-lst-lst-v2 (lst1 lst2)
	(mapcan #'(lambda(x) (combine-elt-lst-v2 x lst2)) lst1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion 
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos
(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
      (list nil)
    (combine-lst-lst-v2 (first lstolsts)
                     (combine-list-of-lsts (rest lstolsts)))))


(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))

(combine-list-of-lsts '(() (+ -) (1 2 3 4)))   ;; -> nil
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; -> nil
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; -> nil
(combine-list-of-lsts '((1 2 3 4)))            ;; -> ((1) (2) (3) (4))
(combine-list-of-lsts '(nil))                  ;; -> nil
(combine-list-of-lsts nil)                     ;; -> (nil)


