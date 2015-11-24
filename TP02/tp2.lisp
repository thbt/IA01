(defvar *monde* (list 'A 'B 'C 'D))
(defvar *actionsAutorisees* '((1 2) (2 3) (2 4)))

;(defun echange (p1 p2)
;	(let ((parametres (list p1 p2)) (successeur *monde*))
;		(if (member parametres *actionsAutorisees* :test #'equal)
;			(progn
;				(rotatef (nth (1- p1) successeur) (nth (1- p2) successeur))
;				(let* ((posD (position 'D successeur :test #'equal)) (posA (position 'A successeur :test #'equal)))
;				(if (< posA posD)
;					successeur))))))

;; echange position1 position2 etat => nouvelEtat
;; echange prend en paramètre la position du pion que l'on souhaite déplacé,  sa position d'arrivé
;; et l'état du 'monde'. Elle renvoie la nouvelle liste état du 'monde'.
;; Elle utilise la macro rotatef qui prend en paramètre un ensemble de 'places' et échange les
;; valeurs de ses places. Cette fonction correspond grossièrement à psetf. Puisqu'elle agit
;; directement sur le symbole il est nécessaire de récupérer une copie de la liste état pour éviter
;; d'éditer l'état d'entrée
(defun echange (p1 p2 etat)
	(let ((result (copy-list etat)) (len (length etat)))
		(if (and (<= 1 p1 len) (<= 1 p2 len))
			(progn
				(rotatef (nth (1- p1) result) (nth (1- p2) result))
				result))))

;* (echange 2 3 '(A D B C))
;(A B D C)

;* (echange 4 5 '(A D B C))
;nil

;; successeurs etat => états-successeurs-valides
;; successeurs prend un paramètre un état, et renvoie une liste avec les états successeurs
;; valides (c'est à dire correspondant à la liste d'actions autorisées et dont le résultat est
;; possible)
;; Elle parcourt la liste des actions autorisées définies préalablement, effectue chacune d'entre
;; elles et teste si le résultat est valide (A à gauche de D)
(defun successeurs (etat)
	(let (result)
		(dolist (a *actionsAutorisees* (reverse result))
			(let ((e (echange (car a) (cadr a) etat)))
				(if (and e (< (position 'A e :test #'equal) (position 'D e :test #'equal))) (push e result))))))

;* (SUCCESSEURS '(A D B C))
;((A C B D) (A B D C))

;; recherche etat but [chemin] => nil
;; recherche prend un parametre l'etat courant et le but recherché et affiche la liste des
;; chemins possibles pour atteindre le but. Elle parcourt tout l'arbre des chemins, mais la
;; premiere solution affichée sera la première solution rencontrée (profondeur d'abord)
;; Au cas où elle tombe dans une impasse (cas où le seul successeur de l'etat est contenu
;; dans le chemin) elle s'arrête.

(defun recherche-all (etat but &optional chemin)
	(let ((c (if chemin chemin ())))
		(push etat c)
		(if (equal etat but)
			(print (reverse c))
			(dolist (x (successeurs etat) 'fin)
				(if (not (member x c :test #'equal))
					(recherche-all x but c))))))


(defun recherche-old (etat but &optional chemin)
	(let ((c (if chemin chemin ())) OK (s (successeurs etat)))
		(push etat c)
		(if (not (equal etat but))
			(return-from recherche-old (reverse c))
			(loop
				while (and s (not OK)) 
				do (if (member (car s) c :test #'equal)
					(pop s)
					(setf OK (recherche-old (pop s) but c)))))
		OK))

;(RECHERCHE '(A D B C) '(C B A D))
;((A D B C) (A B D C) (A C D B) (A D C B) (A B C D) (B A C D) (B C A D) (C B A D))
;((A D B C) (A B D C) (A C D B) (A D C B) (A B C D) (A C B D) (C A B D) (C B A D))
;((A D B C) (A C B D) (C A B D) (C B A D))
;((A D B C) (A C B D) (A B C D) (B A C D) (B C A D) (C B A D))
;NIL

(defun choixEtat (listeEtats but chemin)
	(let (result (distanceHamming 0) (distanceManhattan 0) (distanceActuelle 0) lettresMalPlacees)
		(dolist (e listeEtats)
			(setf 
				;; remise à zéro des variables
				distanceHamming 0 
				distanceManhattan 0
			;; calcul de la distance de hamming : nombre de lettres à la mauvaise place
				lettresMalPlacees
					(loop 
						for i from 0 to (1- (length e))
						when (not (equal (nth i e) (nth i but))) do (incf distanceHamming) collect (nth i e)))
			;; calcul de la distance de manhattan pour chaque lettre mal placées :
			;; écart entre la position d'une lettre mal placée dans l'état actuel et
			;; de la position de cette même lettre dans l'état recherché
			(dolist (l lettresMalPlacees)
				(setf distanceManhattan (+ distanceManhattan
						(abs (- (position l but :test #'equal)
							(position l e :test #'equal))))))
			;; on compare les distances actuelles avec la précédente et on modifie la
			;; valeur de retour en conséquence
			(if result
				(if (and (not (member e chemin :test #'equal)) 
					(< (+ distanceManhattan distanceHamming) distanceActuelle))
					(setf result e distanceActuelle (+ distanceManhattan distanceHamming)))
				(if (not (member e chemin :test #'equal))
					(setf result e distanceActuelle (+ distanceManhattan distanceHamming)))))
		result))

(defun recherche (etat but &optional chemin)
	(let ((c (if chemin chemin ())))
		(push etat c) ; on ajoute le noeud actuel au chemin parcouru
		(if (equal etat but)
			(reverse c)
			(recherche (choixEtat (successeurs etat) but c) but c))))
