; recursive
(defun interr (l m) 
	(if l
		(if (member (car l) m) ;on regarde si le premier élem de l est membre de m
			(cons (car l) (interr (cdr l) m)) ; on ajoute le premier elem de l au début de la liste 
			(interr (cdr l) m)))) ; récursivité : on teste l'intersection du reste de l avec m

;iterative
(defun interi (l m)
	(loop for i in l
		when (member i m)
		collect i))

(interr '(2 3 4 5 7) '(1 4 5 6 8 7))
(interi '(2 3 4 5 7) '(1 4 5 6 8 7))