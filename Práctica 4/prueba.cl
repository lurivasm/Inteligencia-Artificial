(defun maximo (lista cont max)
  (cond
    ((null lista) (list max cont))
    (t
      (if (> (first lista) max)
        (maximo (rest lista) 1 (first lista))
        (if (= (first lista) max)
            (maximo (rest lista) (+ 1 cont) max)
            (maximo (rest lista) cont max))))))

(print (maximo '(1 2 3 1 1 3 4 3 3) 0 0))
