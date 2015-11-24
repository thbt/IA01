(defun nbFeuilles (L)
	(cond
		((null L) 0)
		((atom L) 1)
		(T (+ (nbFeuilles(car L)) (nbFeuilles (cdr L))))))

(nbFeuilles '(r ((t)) y (g h) (j m l) p))
