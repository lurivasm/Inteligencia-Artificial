;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Práctica 4 Inteligencia Artificial
;;; Jugador 2
;;; Lucía Rivas Molina       <lucia.rivasmolina@estudiante.uam.es>
;;; Daniel Santo-Tomás López <daniel.santo-tomas@estudiante.uam.es>
;;;

(defpackage :2301_P01_c4cca
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))

(in-package 2301_P01_c4cca)

(defvar *alias* '|Quien_es_Homer|)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion contar-horizontal
;;; Dado un tablero, una ficha y unas coordenadas columna-fila
;;; cuenta cuantas fichas de ese jugador hay en horizontal
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;        columna -> columna donde empezamos a contar
;;;        fila -> fila donde empezamos a contar
;;;
;;; Return : numero de fichas que hay en horizontal sumando las
;;;          de la derecha y las de la izquierda
;;;
(defun contar-horizontal (tablero ficha columna fila)
  (+ (contar-derecha tablero ficha columna fila)
     (contar-izquierda tablero ficha (1- columna) fila)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion contar-vertical
;;; Dado un tablero, una ficha y unas coordenadas columna-fila
;;; cuenta cuantas fichas de ese jugador hay en vertical
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;        columna -> columna donde empezamos a contar
;;;        fila -> fila donde empezamos a contar
;;;
;;; Return : numero de fichas que hay en horizontal sumando las
;;;          de arriba y las de abajo
;;;
(defun contar-vertical (tablero ficha columna fila)
  (+ (contar-abajo tablero ficha columna fila)
     (contar-arriba tablero ficha columna (1+ fila))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion contar-diagonal-ascendente
;;; Dado un tablero, una ficha y unas coordenadas columna-fila
;;; cuenta cuantas fichas de ese jugador hay en la diagonal /
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;        columna -> columna donde empezamos a contar
;;;        fila -> fila donde empezamos a contar
;;;
;;; Return : numero de fichas que hay en la diagonal
;;;
(defun contar-diagonal-ascendente (tablero ficha columna fila)
  (+ (contar-abajo-izquierda tablero ficha columna fila)
     (contar-arriba-derecha tablero ficha (1+ columna) (1+ fila))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion contar-diagonal-descendente
;;; Dado un tablero, una ficha y unas coordenadas columna-fila
;;; cuenta cuantas fichas de ese jugador hay en la diagonal \
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;        columna -> columna donde empezamos a contar
;;;        fila -> fila donde empezamos a contar
;;;
;;; Return : numero de fichas que hay en la diagonal
;;;
(defun contar-diagonal-descendente (tablero ficha columna fila)
  (+ (contar-abajo-derecha tablero ficha columna fila)
     (contar-arriba-izquierda tablero ficha (1- columna) (1+ fila))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion suma
;;; Suma las heuristicas de una lista dada
;;;
;;; Args -> lista de elementos cuyas heuristicas hay que sumar
;;;
;;; Return -> la suma de los siguientes valores
;;;           suma 0 si el elemento de la lista es 0
;;;           suma 20 si el elemento de la lista es 1
;;;           suma 2000 si el elemento de la lista es 2 dandole mas prioridad
;;;           suma 200 si el elemento de la lista es 3
(defun suma (lista)
  (let ((elem (first lista)))
    (cond
        ((null lista) 0)
        ((= 0 elem) (+ 0 (suma (rest lista))))
        ((= 1 elem) (+ 20 (suma (rest lista))))
        ((= 2 elem) (+ 2000 (suma (rest lista))))
        ((= 3 elem) (+ 200 (suma (rest lista)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion cuenta-fichas-consecutivas2
;;; Cuenta las fichas consecutivas del tablero en diagonal,
;;; vertical y horizontal y devuelve la suma de la lista
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;        columna -> columna donde empezamos a contar
;;;        fila -> fila donde empezamos a contar
;;;
;;; Return : suma de las heuristicas de la lista
;;;
(defun cuenta-fichas-consecutivas2 (tablero ficha columna fila)
   (let* ((horizontal (contar-horizontal tablero ficha columna fila))
          (vertical (contar-vertical tablero ficha columna fila))
          (diag-asc (contar-diagonal-ascendente tablero ficha columna fila))
          (diag-desc (contar-diagonal-descendente tablero ficha columna fila)))
          (suma (list horizontal vertical diag-asc diag-desc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion puntuacion2
;;; Dado un tablero y una ficha, lo recorre para todas las
;;; columnas del tablero sacando el maximo de fichas consecutivas
;;; Calcula una puntuación para cada columna y devuelve la suma de ellas
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;
;;; Sea n el número de veces que aparece el máximo de fichas consecutivas
;;; Return : la suma de los valores de cuenta-fichas-consecutivas2 para
;;;           cada columna
;;;
 (defun puntuacion2 (tablero ficha)
   (let ((punt 0))
     (loop for columna from 0 below (tablero-ancho tablero) do
       (let*  ((altura (altura-columna tablero columna))
               (fila (1- altura))
               (val (cuenta-fichas-consecutivas2 tablero ficha columna fila)))
                 (setf punt
                   (+ punt val))))
     punt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion puntuacion-contrincante2
;;; Dado un tablero y una ficha, lo recorre para todas las
;;; columnas del tablero sacando el maximo de fichas consecutivas
;;; y lo multiplica por -1 para que sea negativo
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;
;;; Return : la suma de los valores de cuenta-fichas-consecutivas2
;;;          para cada columna en negativo
;;;
	(defun puntuacion-contrincante2 (tablero ficha)
	  (let ((punt 0))
	    (loop for columna from 0 below (tablero-ancho tablero) do
	          (let*  ((altura (altura-columna tablero columna))
	  				        (fila (1- altura))
	                  (val (cuenta-fichas-consecutivas2 tablero ficha columna fila)))
	          (setf punt
	            (+ punt (* -1 val)))))
	    punt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion heuristica
;;; Calcula nuestra puntuacion y la del oponente y devuelve
;;; una heuristica
;;;
;;; Args : estado -> estado del tablero en el momento de calcularlo
;;;
;;; Return : la suma de las heuristicas tanto a favor como en contra
;;;
(defun heuristica (estado)
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
