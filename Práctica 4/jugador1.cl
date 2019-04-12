(defpackage :2301_P01_618b6 ; se declara un paquete con el grupo, la pareja y el código
	(:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
	(:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 2301_P01_618b6)

(defvar *alias* "Homer") ; alias que aparece en el ranking

(defun heuristica (estado) 
	(tam (columnas-jugables (estado-tablero estado)) 0)) 



(defun tam (lst cont)
	(if (eql lst nil) cont
		(tam (rest lst) (+ 1 cont))))