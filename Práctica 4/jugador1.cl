(defpackage :2301_P01_618b6
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))

(in-package 2301_P01_618b6)

(defvar *alias* '|Homer|)

(defun maximo (lista cont max)
  (cond
    ((null lista) (list max cont))
    (t
      (if (> (first lista) max)
        (maximo (rest lista) 1 (first lista))
        (if (= (first lista) max)
            (maximo (rest lista) (+ 1 cont) max)
            (maximo (rest lista) cont max))))))


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

 (defun puntuacion1 (tablero ficha)
     (let ((punt 0))
       (loop for columna from 0 below (tablero-ancho tablero) do
               (let*  ((altura (altura-columna tablero columna))
       				        (fila (1- altura))
                       (maxlist (cuenta-fichas-consecutivas1 tablero ficha columna fila)))
               (setf punt
                 (+ punt
                   (cond
                       ((= 0 (first maxlist)) 0)
                       ((= 1 (first maxlist)) (* 20 (second maxlist)))
                       ((= 2 (first maxlist)) (* 2000 (second maxlist)))
                       ((= 3 (first maxlist)) (* 200 (second maxlist))))))))

					             punt))


	 (defun puntuacion-contrincante1 (tablero ficha)
	   (let ((punt 0))
	     (loop for columna from 0 below (tablero-ancho tablero) do
	           (let*  ((altura (altura-columna tablero columna))
   				        (fila (1- altura))
	                   (maxlist (cuenta-fichas-consecutivas tablero ficha columna fila)))
	           (setf punt
	             (+ punt
	               (cond
	                 ((= 0 (first maxlist)) 0)
	                 ((= 1 (first maxlist)) (* -1 (* 20 (second maxlist))))
	                 ((= 2 (first maxlist)) (* -1 (* 2000 (second maxlist))))
					         ((= 3 (first maxlist)) (* -1 (* 200(second maxlist)))))))))

                 punt))


(defun heuristica (estado)
 (let* ((tablero (estado-tablero estado))
	       (ficha-actual (estado-turno estado))
	       (ficha-oponente (siguiente-jugador ficha-actual)))
		     (if (juego-terminado-p estado)
	           (let ((ganador (ganador estado)))
		            (cond ((not ganador) 0)
		                  ((eql ganador ficha-actual) +val-max+)
		                   (t +val-min+)))
			   (let ((puntuacion-actual (puntuacion1 tablero ficha-actual))
			         (puntuacion-oponente (puntuacion-contrincante1 tablero ficha-oponente)))
              (if (> puntuacion-actual (abs puntuacion-oponente))
                puntuacion-actual
	               puntuacion-oponente)))))
