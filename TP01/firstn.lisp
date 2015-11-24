(defun firstni (n l) 
	(if (and (> n 0) l) ; tant que n > 0 et que l n'est pas nul
		(cons (car l) (firstni (- n 1) (cdr l))))) ; construction du résultat a partir du premier élément et récursion

(defun firstnr (n l) 
	(loop for i from 0 to (- n 1)
		collect (nth i l)))

(firstni 2 '(2 3 4 5))
(firstnr 3 '(2 3 4 5 6))
