(defun my-assoc(cle a-liste)
	(cond
		((null a-liste) nil) ; on regarde si clé de 1ere paire = à celle qu'on cherche
		((equal cle (car(car a-liste))) (car a-liste)) ;si oui, alors retourne cette paire
		((my-assoc cle (cdr a-liste))))) ;sinon, on passe à la paire d'après (via le cdr de la liste)

(my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45)))
(my-assoc 'Yves '((Yolande aime Pierre) (Pierre aime Julie) (Julie aime Pierre)))