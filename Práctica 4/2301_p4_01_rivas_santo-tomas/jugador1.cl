;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Práctica 4 Inteligencia Artificial
;;; Jugador 1
;;; Lucía Rivas Molina       <lucia.rivasmolina@estudiante.uam.es>
;;; Daniel Santo-Tomás López <daniel.santo-tomas@estudiante.uam.es>
;;;

(defpackage :2301_P01_618b6
	(:use :common-lisp :conecta4)
	(:export :heuristica :*alias*))

(in-package 2301_P01_618b6)

(defvar *alias* '|Homer|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Función maximo
;;; Función que dada una lista de números devuelve un par
;;; formado por el número máximo de la lista y el número
;;; de veces que aparece
;;;
;;; Args : lista -> lista de números
;;; 			 cont -> contador para llamada recursiva
;;;			   max -> el número máximo
;;;
;;; Return : par (m, n) con m -> número máximo
;;;                         n -> veces que aparece m en lista
(defun maximo (lista cont max)
  (cond
    ((null lista) (list max cont))								;; Si la lista está vacía devolvemos el par
    (t
      (if (> (first lista) max)										;; Si el primero es mayor que max lo cambiamos
        (maximo (rest lista) 1 (first lista))			;; y llamamos a maximo con el resto de la lista
        (if (= (first lista) max)									;; Si es igual a max aumentamos el contador
            (maximo (rest lista) (+ 1 cont) max)
            (maximo (rest lista) cont max))))))		;; Sino llamamos a maximo con max y el resto de
																									;; la lista


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion cuenta-fichas-consecutivas1
;;; Cuenta las fichas consecutivas del tablero en diagonal,
;;; vertical y horizontal y devuelve el maximo de fichas
;;; consecutivas
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;        columna -> columna donde empezamos a contar
;;;        fila -> fila donde empezamos a contar
;;;
;;; Return : numero de fichas consecutivas maximo en todas direcciones
;;;
(defun cuenta-fichas-consecutivas1 (tablero ficha columna fila)
    (let* ((horizontal (contar-horizontal tablero ficha columna fila))
           (vertical (contar-vertical tablero ficha columna fila))
           (diag-asc (contar-diagonal-ascendente tablero ficha columna fila))
           (diag-desc (contar-diagonal-descendente tablero ficha columna fila)))
           (maximo (list horizontal vertical diag-asc diag-desc) 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion puntuacion1
;;; Dado un tablero y una ficha, lo recorre para todas las
;;; columnas del tablero sacando el maximo de fichas consecutivas
;;; Calcula una puntuación para cada columna y devuelve la suma de ellas
;;; Le da mas prioridad a 2 fichas consecutivas
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;
;;; Sea n el número de veces que aparece el máximo de fichas consecutivas
;;; Return : 0 si no hay fichas consecutivas
;;;          20*n si hay una ficha consecutiva
;;;          2000*n si hay dos fichas consecutivas
;;;          200*n si hay tres fichas consecutivas
;;;
 (defun puntuacion1 (tablero ficha)
     (let ((punt 0))
       (loop for columna from 0 below (tablero-ancho tablero) do		;; Bucle para las columnas
               (let*  ((altura (altura-columna tablero columna))
       				        (fila (1- altura))                            ;; Calcula el maximo de fichas consecutivas
                       (maxlist (cuenta-fichas-consecutivas1 tablero ficha columna fila)))
               (setf punt
                 (+ punt
                   (cond
                       ((= 0 (first maxlist)) 0)															;; 0 fichas consecutivas
                       ((= 1 (first maxlist)) (* 20 (second maxlist)))				;; 1 ficha consecutiva
                       ((= 2 (first maxlist)) (* 2000 (second maxlist)))			;; 2 fichas consecutivas
                       ((= 3 (first maxlist)) (* 200 (second maxlist))))))))  ;; 3 fichas consecutivas

					             punt))			;; Suma las puntuaciones y lo devuelve

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion puntuacion-contrincante1
;;; Dado un tablero y una ficha, lo recorre para todas las
;;; columnas del tablero sacando el maximo de fichas consecutivas
;;; Calcula una puntuación para cada columna y devuelve la suma de ellas
;;; Le da más prioridad a 2 fichas consecutivas
;;;
;;; Args : tablero -> tablero del juego
;;;        ficha -> ficha del jugador que queremos contar
;;;
;;; Sea n el número de veces que aparece el máximo de fichas consecutivas
;;; Return : 0 si no hay fichas consecutivas
;;;          1-20*n si hay una ficha consecutiva
;;;          1-2000*n si hay dos fichas consecutivas
;;;          1-200*n si hay tres fichas consecutivas
;;;
	(defun puntuacion-contrincante1 (tablero ficha)
	  (let ((punt 0))
	    (loop for columna from 0 below (tablero-ancho tablero) do			;; Bucle para las columnas
	          (let*  ((altura (altura-columna tablero columna))
  				        (fila (1- altura))                                ;; Maximas fichas consecutivas
	                  (maxlist (cuenta-fichas-consecutivas1 tablero ficha columna fila)))
	          (setf punt
	            (+ punt
	              (cond
	                ((= 0 (first maxlist)) 0)                                   ;; 0 fichas consecutivas
	                ((= 1 (first maxlist)) (* -1 (* 20 (second maxlist))))      ;; 1 ficha consecutiva
	                ((= 2 (first maxlist)) (* -1 (* 2000 (second maxlist))))    ;; 2 fichas consecutivas
				          ((= 3 (first maxlist)) (* -1 (* 200(second maxlist))))))))) ;; 3 fichas consecutivas

               punt))    ;; Suma las puntuaciones y lo devuelve

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Funcion heuristica
;;; Calcula nuestra puntuacion y la del oponente y devuelve
;;; una heuristica
;;;
;;; Args : estado -> estado del tablero en el momento de calcularlo
;;;
;;; Return : una heuristica a favor si es mayor que el valor
;;;              absoluto de la heuristica en contra
;;;          la heuristica en contra si no se cumple la condición anterior
;;;
(defun heuristica (estado)
 (let* ((tablero (estado-tablero estado))
	       (ficha-actual (estado-turno estado))
	       (ficha-oponente (siguiente-jugador ficha-actual)))
		     (if (juego-terminado-p estado)												;; Si el juego termina devuelve
	           (let ((ganador (ganador estado)))                ;; los valores máximo o mínimo
		            (cond ((not ganador) 0)
		                  ((eql ganador ficha-actual) +val-max+)
		                   (t +val-min+)))
			   (let ((puntuacion-actual (puntuacion1 tablero ficha-actual))		;; Calculamos la heuristica a favor y la contra
			         (puntuacion-oponente (puntuacion-contrincante1 tablero ficha-oponente)))
              (if (> puntuacion-actual (abs puntuacion-oponente))
                puntuacion-actual							;; Si favor > abs(contra) devuelve favor
	               puntuacion-oponente)))))     ;; Sino devuelve contra
