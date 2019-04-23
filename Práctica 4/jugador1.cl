(defpackage :2301_P01_618b6
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))

(in-package 2301_P01_618b6)

(defvar *alias* '|Homer|)

(defun heuristica (estado)
	(let* ((tablero (estado-tablero estado))
	 (ficha-actual (estado-turno estado))
	 (ficha-oponente (siguiente-jugador ficha-actual)))
		(if (juego-terminado-p estado)
	(let ((ganador (ganador estado)))
		(cond ((not ganador) 0)
		((eql ganador ficha-actual) +val-max+)
		(t +val-min+)))
			(let ((puntuacion-actual 0)
			(puntuacion-oponente 0))
	(loop for columna from 0 below (tablero-ancho tablero) do
				(let* ((altura (altura-columna tablero columna))
				 (fila (1- altura))
				 (abajo (contar-abajo tablero ficha-actual columna fila))
				 (der (contar-derecha tablero ficha-actual columna fila))
				 (izq (contar-izquierda tablero ficha-actual columna fila))
				 (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
				 (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
				 (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
				 (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
		(setf puntuacion-actual
					(+ puntuacion-actual
			 (cond ((= abajo 0) 0)
						 ((= abajo 1) 10)
						 ((= abajo 2) 100)
						 ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
						 ((= der 1) 10)
						 ((= der 2) 100)
						 ((= der 3) 1000))
			 (cond ((= izq 0) 0)
						 ((= izq 1) 10)
						 ((= izq 2) 100)
						 ((= izq 3) 1000))
			 (cond ((= abajo-der 0) 0)
						 ((= abajo-der 1) 10)
						 ((= abajo-der 2) 100)
						 ((= abajo-der 3) 1000))
			 (cond ((= abajo-izq 0) 0)
						 ((= abajo-izq 1) 10)
						 ((= abajo-izq 2) 100)
						 ((= abajo-izq 3) 1000))
			 (cond ((= arriba-izq 0) 0)
						 ((= arriba-izq 1) 10)
						 ((= arriba-izq 2) 100)
						 ((= arriba-izq 3) 1000)))))
				(let* ((altura (altura-columna tablero columna))
				 (fila (1- altura))
				 (abajo (contar-abajo tablero ficha-oponente columna fila))
				 (der (contar-derecha tablero ficha-oponente columna fila))
				 (izq (contar-izquierda tablero ficha-oponente columna fila))
				 (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
				 (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
				 (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
				 (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
		(setf puntuacion-oponente
					(+ puntuacion-oponente
			 (cond ((= abajo 0) 0)
						 ((= abajo 1) 10)
						 ((= abajo 2) 100)
						 ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
						 ((= der 1) 10)
						 ((= der 2) 100)
						 ((= der 3) 1000))
			 (cond ((= izq 0) 0)
						 ((= izq 1) 10)
						 ((= izq 2) 100)
						 ((= izq 3) 1000))
			 (cond ((= abajo-der 0) 0)
						 ((= abajo-der 1) 10)
						 ((= abajo-der 2) 100)
						 ((= abajo-der 3) 1000))
			 (cond ((= abajo-izq 0) 0)
						 ((= abajo-izq 1) 10)
						 ((= abajo-izq 2) 100)
						 ((= abajo-izq 3) 1000))
			 (cond ((= arriba-izq 0) 0)
						 ((= arriba-izq 1) 10)
						 ((= arriba-izq 2) 100)
						 ((= arriba-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))
