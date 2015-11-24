(defun monEqual(arg1 arg2)
	;; Si on a deux atomes
	(if (and (atom arg1) (atom arg2))
	;; alors on regarde si il sont eq
		(eq arg1 arg2)
		;; Sinon il faut continuer à descendre pour arriver jusqu'aux atomes
		;; On regarde si nombre égaux d'éléments, sinon c'est pas la peine
		(if (not(= (length arg1) (length arg2)))
			;; si pas le même nombre d'elt alors c'est terminé
			nil
			;; sinon on peut continuer
			(if (and 
				(monEqual (car arg1) (car arg2)) 
			(monEqual (cdr arg1) (cdr arg2)))
				T))))

(monEqual 'LUC 'LUC)
(monEqual 'LUC 'DANIEL)
(monEqual (car '(do re)) (cadr '(mi do sol)))
(monEqual '(d p f t r) '(d p f t r))