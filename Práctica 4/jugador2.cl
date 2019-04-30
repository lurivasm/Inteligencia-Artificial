(defpackage :2301_P01_c4cca
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))

(in-package 2301_P01_c4cca)

(defvar *alias* '|Quien_es_Homer|)

(defun contar-horizontal (tablero ficha columna fila)
  (+ (contar-derecha tablero ficha columna fila)
     (contar-izquierda tablero ficha (1- columna) fila)))

(defun contar-vertical (tablero ficha columna fila)
  (+ (contar-abajo tablero ficha columna fila)
     (contar-arriba tablero ficha columna (1+ fila))))

(defun contar-diagonal-ascendente (tablero ficha columna fila)
  (+ (contar-abajo-izquierda tablero ficha columna fila)
     (contar-arriba-derecha tablero ficha (1+ columna) (1+ fila))))

(defun contar-diagonal-descendente (tablero ficha columna fila)
  (+ (contar-abajo-derecha tablero ficha columna fila)
     (contar-arriba-izquierda tablero ficha (1- columna) (1+ fila))))

(defun cuenta-fichas-consecutivas1 (tablero ficha columna fila)
    (let* ((horizontal (contar-horizontal tablero ficha columna fila))
           (vertical (contar-vertical tablero ficha columna fila))
           (diag-asc (contar-diagonal-ascendente tablero ficha columna fila))
           (diag-desc (contar-diagonal-descendente tablero ficha columna fila)))
           (maximo (list horizontal vertical diag-asc diag-desc) 0 0)))


(defun suma (lista)
  (let ((elem (first lista)))
    (cond
        ((null lista) 0)
        ((= 0 elem) (+ 0 (suma (rest lista))))
        ((= 1 elem) (+ 20 (suma (rest lista))))
        ((= 2 elem) (+ 2000 (suma (rest lista))))
        ((= 3 elem) (+ 200 (suma (rest lista)))))))



 ((defun heuristica25 (estado)
  (let* ((tablero (estado-tablero estado))
	       (ficha-actual (estado-turno estado))
	       (ficha-oponente (siguiente-jugador ficha-actual)))
		     (if (juego-terminado-p estado)
	           (let ((ganador (ganador estado)))
		            (cond ((not ganador) 0)
		                  ((eql ganador ficha-actual) +val-max+)
		                   (t +val-min+)))
			   (let ((puntuacion-actual (puntuacion2 tablero ficha-actual))
			         (puntuacion-oponente (puntuacion-contrincante2 tablero ficha-oponente)))
               (+ puntuacion-actual puntuacion-oponente)))))defun cuenta-fichas-consecutivas2 (tablero ficha columna fila)
   (let* ((horizontal (contar-horizontal tablero ficha columna fila))
          (vertical (contar-vertical tablero ficha columna fila))
          (diag-asc (contar-diagonal-ascendente tablero ficha columna fila))
          (diag-desc (contar-diagonal-descendente tablero ficha columna fila)))
          (suma (list horizontal vertical diag-asc diag-desc))))


 (defun puntuacion2 (tablero ficha)
   (let ((punt 0))
     (loop for columna from 0 below (tablero-ancho tablero) do
       (let*  ((altura (altura-columna tablero columna))
               (fila (1- altura))
               (val (cuenta-fichas-consecutivas2 tablero ficha columna fila)))
                 (setf punt
                   (+ punt val))))
     punt))


	(defun puntuacion-contrincante2 (tablero ficha)
	  (let ((punt 0))
	    (loop for columna from 0 below (tablero-ancho tablero) do
	          (let*  ((altura (altura-columna tablero columna))
	  				        (fila (1- altura))
	                  (val (cuenta-fichas-consecutivas tablero ficha columna fila)))
	          (setf punt
	            (+ punt (* -1 val)))))
	    punt))



(defun heuristica2 (estado)
  (let* ((tablero (estado-tablero estado))
	       (ficha-actual (estado-turno estado))
	       (ficha-oponente (siguiente-jugador ficha-actual)))
		     (if (juego-terminado-p estado)
	           (let ((ganador (ganador estado)))
		            (cond ((not ganador) 0)
		                  ((eql ganador ficha-actual) +val-max+)
		                   (t +val-min+)))
			   (let ((puntuacion-actual (puntuacion2 tablero ficha-actual))
			         (puntuacion-oponente (puntuacion-contrincante2 tablero ficha-oponente)))
               (+ puntuacion-actual puntuacion-oponente)))))
