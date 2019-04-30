(use-package 'conecta4)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; -------------------------------------------------------------------------------
;; Funciones de evaluación
;; -------------------------------------------------------------------------------

(defun f-eval-bueno (estado)
  ; current player standpoint
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
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000)))))
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
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Jugador 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defun heuristica1 (estado)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Jugador 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Jugador 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Esta es la heuristica que utiliza el jugador 3, mirando todas las direcciones
;;; posibles para calcular la mejor opcion
(defun heuristica3 (estado)
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
			 (cond ((= arriba-der 0) 0)
						 ((= arriba-der 1) 10)
						 ((= arriba-der 2) 100)
						 ((= arriba-der 3) 1000))
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
			 (cond ((= arriba-der 0) 0)
						 ((= arriba-der 1) 10)
						 ((= arriba-der 2) 100)
						 ((= arriba-der 3) 1000))
			 (cond ((= arriba-izq 0) 0)
						 ((= arriba-izq 1) 10)
						 ((= arriba-izq 2) 100)
						 ((= arriba-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; heuristica anterior a la heuristica3 que cambia los valores dandole más importancia
;;; a cuando tenemos 2 fichas y al contrincante
  (defun heuristica31 (estado)
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
  						 ((= abajo 2) 200)
  						 ((= abajo 3) 2000))
  			 (cond ((= der 0) 0)
  						 ((= der 1) 10)
  						 ((= der 2) 200)
  						 ((= der 3) 2000))
  			 (cond ((= izq 0) 0)
  						 ((= izq 1) 10)
  						 ((= izq 2) 200)
  						 ((= izq 3) 2000))
  			 (cond ((= abajo-der 0) 0)
  						 ((= abajo-der 1) 10)
  						 ((= abajo-der 2) 200)
  						 ((= abajo-der 3) 2000))
  			 (cond ((= abajo-izq 0) 0)
  						 ((= abajo-izq 1) 10)
  						 ((= abajo-izq 2) 200)
  						 ((= abajo-izq 3) 2000))
  			 (cond ((= arriba-der 0) 0)
  						 ((= arriba-der 1) 10)
  						 ((= arriba-der 2) 200)
  						 ((= arriba-der 3) 2000))
  			 (cond ((= arriba-izq 0) 0)
  						 ((= arriba-izq 1) 10)
  						 ((= arriba-izq 2) 200)
  						 ((= arriba-izq 3) 2000)))))
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
  						 ((= abajo 2) 200)
  						 ((= abajo 3) 3000))
  			 (cond ((= der 0) 0)
  						 ((= der 1) 10)
  						 ((= der 2) 200)
  						 ((= der 3) 3000))
  			 (cond ((= izq 0) 0)
  						 ((= izq 1) 10)
  						 ((= izq 2) 200)
  						 ((= izq 3) 3000))
  			 (cond ((= abajo-der 0) 0)
  						 ((= abajo-der 1) 10)
  						 ((= abajo-der 2) 200)
  						 ((= abajo-der 3) 3000))
  			 (cond ((= abajo-izq 0) 0)
  						 ((= abajo-izq 1) 10)
  						 ((= abajo-izq 2) 200)
  						 ((= abajo-izq 3) 3000))
  			 (cond ((= arriba-der 0) 0)
  						 ((= arriba-der 1) 10)
  						 ((= arriba-der 2) 200)
  						 ((= arriba-der 3) 3000))
  			 (cond ((= arriba-izq 0) 0)
  						 ((= arriba-izq 1) 10)
  						 ((= arriba-izq 2) 200)
  						 ((= arriba-izq 3) 3000))))))
  	(- puntuacion-actual puntuacion-oponente)))))


(defun heuristica25 (estado)
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

;(+ puntuacion-actual puntuacion-oponente)))))
;(if (> puntuacion-actual (abs puntuacion-oponente))
  ; puntuacion-actual
   ;puntuacion-oponente)))))



;; -------------------------------------------------------------------------------
;; Jugadores
;; -------------------------------------------------------------------------------

(defvar *jugador-aleatorio* (make-jugador :nombre 'Jugador-aleatorio
					  :f-jugador #'f-jugador-aleatorio
					  :f-eval  #'f-eval-aleatoria))

(defvar *jugador-bueno* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'f-eval-bueno))

(defvar *jugador-humano* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-humano
				       :f-eval  #'f-no-eval))

(defvar *jugador1* (make-jugador :nombre 'Jugador-humano
               :f-jugador #'f-jugador-negamax
               :f-eval  #'heuristica1))

(defvar *jugador2* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-negamax
				       :f-eval  #'heuristica2))

(defvar *jugador3* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-negamax
				       :f-eval  #'heuristica3))
;; -------------------------------------------------------------------------------
;; Algunas partidas de ejemplo:
;; -------------------------------------------------------------------------------

(setf *verbose* t)

;(print (partida *jugador-aleatorio* *jugador-aleatorio*))
;(print (partida *jugador-aleatorio* *jugador-bueno* 4))
;(print (partida *jugador-bueno* *jugador-aleatorio* 4))
;(print (partida *jugador-bueno* *jugador-bueno* 4))
;(print (partida *jugador-humano* *jugador-humano*))
;(print (partida *jugador-humano* *jugador-aleatorio* 4))
;(print (partida *jugador-humano* *jugador-bueno* 4))
;(print (partida *jugador-aleatorio* *jugador-humano*))
;(print (partida *jugador1* *jugador-bueno* 4))
;(print (partida *jugador1* *jugador2* 4))
;(print (partida *jugador1* *jugador3* 4))
;(print (partida *jugador2* *jugador-bueno* 4))
;(print (partida *jugador2* *jugador1* 4))
;(print (partida *jugador2* *jugador3* 4))
;(print (partida *jugador3* *jugador-bueno* 4))
;(print (partida *jugador3* *jugador1* 4))
(print (partida *jugador3* *jugador2* 4))

;;

;; (load "P4_IA_2018_2019_conecta4.v0.cl")
;;  (load "P4_IA_2018_2019_conecta4.v1.cl")
;;  (load "P4_IA_2018_2019_jugadores.v0.cl")
