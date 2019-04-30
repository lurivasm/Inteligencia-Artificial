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


  (defun heuristica1 (estado)
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

(defun suma (lista)
  (let ((elem (first lista)))
    (cond
        ((null lista) 0)
        ((= 0 elem) (+ 0 (suma (rest lista))))
        ((= 1 elem) (+ 20 (suma (rest lista))))
        ((= 2 elem) (+ 2000 (suma (rest lista))))
        ((= 3 elem) (+ 200 (suma (rest lista)))))))



(defun cuenta-fichas-consecutivas2 (tablero ficha columna fila)
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

;max  100000
;med2 50000
;med1 25000
;min  -100000


(defun heuristica2 (estado)
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
(print (partida *jugador1* *jugador3* 4))

;(print (partida *jugador3* *jugador-bueno* 4))
;;

;; (load "P4_IA_2018_2019_conecta4.v0.cl")
;;  (load "P4_IA_2018_2019_conecta4.v1.cl")
;;  (load "P4_IA_2018_2019_jugadores.v0.cl")
