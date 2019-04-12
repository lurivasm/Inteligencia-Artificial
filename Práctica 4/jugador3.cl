(defpackage :2301_P01_f413c ; se declara un paquete con el grupo, la pareja y el código
	(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
	(:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 2301_P01_f413c)

(defvar *alias* "Yo me llamo tipo de incógnito") ; alias que aparece en el ranking

(defun heuristica (estado) 
	(first (sort (lista-fichas estado)  #'<)))



(defun buscar-ficha (estado columna fila)
		(cond
			((eql columna 8)
				(buscar-ficha estado 0 (+ 1 fila)))
			((eql (obtener-ficha (estado-tablero estado) columna fila) 
			 (estado-turno estado))
				 (list columna fila))))



(defun lista-fichas (estado)
	(let (lst (buscar-ficha (estado 0 0)))
			(list 
				(- 4 (contar-abajo (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst)))
				(- 4 (contar-arriba (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst)))
				(- 4 (contar-derecha (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst)))
				(- 4 (contar-izquierda (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst)))
				(- 4 (contar-abajo-derecha (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst)))
				(- 4 (contar-abajo-izquierda (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst)))
				(- 4 (contar-arriba-derecha (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst)))
				(- 4 (contar-arriba-izquierda (estado-tablero estado) (estado-turno estado)
										(first lst) (second lst))))))
				
				