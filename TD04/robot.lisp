(setq laby '(
	(e 1)
	(1 e 2)
	(2 1 7)
	(3 6)
	(4 5)
	(5 12 4)
	(6 3 7)
	(7 2 6 8)
	(8 7 9)
	(9 8 10)
	(10 9 11 15)
	(11 10 12 14)
	(12 11 5)
	(13 20)
	(14 11)
	(15 10 16)
	(16 15 17)
	(17 16 18)
	(18 17 19)
	(19 18 20)
	(20 13 s)
	(s 20)))

;; récupére les successeurs d'un état du labyrinthe
(defun next (state laby) (cdr (assoc state laby)))

;; retourne les éléments suivants valides
(defun validSuccessors (laby state path)
	(let ((result ()))
		(dolist (x (next laby state) result)
			(if (not (member x path))
				(push x result)))))

(defun explore (labyrinth state goal currentPath)
	(let ((successors (validSuccessors labyrinth state currentPath))) ; on récupère la liste des successeurs
		(push state currentPath) ;on ajoute le noeud actuel au chemin parcouru
		(if (equal state goal)
			(print (reverse currentPath)) ; on est à la sortie : on affiche le chemin parcouru
			(dolist (successor successors nil) ; pour tous les noeuds suivants valides
				(explore labyrinth successor goal currentPath))))) ; bah on explore via récursivité

(explore laby 'e 's '())



(defun successeurs-valides (etat typesEpreuvesPossibles laby epreuves)
	(let ( (successeurs-valides) (successeursPossibles (successeurs (etat laby))))
		(dolist (x successeursPossibles)
			(if (member (cdr x) typesEpreuvesPossibles)
				(push x successeurs-valides)
			)
		)
	)
)

(defun succValid (etat capacite laby chemin)
	(let ((result ()))
		(dolist (x (successeurs laby etat))
			(if (and (member (cdr x) capacites) (not member x chemin))
				(push x result)))))


Explore (etat sortie laby nbEpreuve epreuves typesEpreuvesPossibles)
si nbEpreuve < 7
	si nbEpreuve >= 4
		proposer de sortir
		Si reponse = oui
			on stop
	si etat = tresor
		retourner le resultat de epreuve ultime
	sinon
		succValides = successeurs-valides(etat typesEpreuvesPossibles laby epreuves)
		pour tous les successeurs faire
			explore ( (car successeur) sortie laby  (nbEpreuve + 1) epreuves)
		sinon
			retourner échec
