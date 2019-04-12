(defpackage :2301_P01_c4cca ; se declara un paquete con el grupo, la pareja y el código
	(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
	(:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 2301_P01_c4cca)

(defvar *alias* "¿Quien es Homer?") ; alias que aparece en el ranking

(defun heuristica (estado) 
	(let (lst (buscar-ficha (estado 0 0)))
		(- 4 (contar-arriba-derecha (estado-tablero estado) (estado-turno estado)
									(first lst) (second lst)))))



(defun buscar-ficha (estado columna fila)
		(cond
			((eql columna 8)
				(buscar-ficha estado 0 (+ 1 fila)))
			((eql (obtener-ficha (estado-tablero estado) columna fila) 
			 (estado-turno estado))
				 (list columna fila))))
	