(defvar *monde* (list 'A 'B 'C 'D))
(defvar *actionsAutorisees* (list (list 1 2) (list 2 3) (list 2 4)))

;(defun echange (p1 p2)
;	(let ((parametres (list p1 p2)) (successeur *monde*))
;		(if (member parametres *actionsAutorisees* :test #'equal)
;			(progn
;				(rotatef (nth (1- p1) successeur) (nth (1- p2) successeur))
;				(let* ((posD (position 'D successeur :test #'equal)) (posA (position 'A successeur :test #'equal)))
;				(if (< posA posD)
;					successeur))))))

(defun echange (p1 p2 etat)
	(let ((parametres (list p1 p2)) (successeur (copy-list etat)))
		(if (member parametres *actionsAutorisees* :test #'equal)
			(progn
				(rotatef (nth (1- p1) successeur) (nth (1- p2) successeur))
				(if (< (position 'A successeur :test #'equal) (position 'D successeur :test #'equal))
					successeur)))))

(defun successeurs (etat)
	(let (result (e12 (echange 1 2 etat)) (e23 (echange 2 3 etat)) (e24 (echange 2 4 etat)))
		(if (and e12 (< (position 'A e12 :test #'equal) (position 'D e12 :test #'equal))) (push e12 result))
		(if (and e23 (< (position 'A e23 :test #'equal) (position 'D e23 :test #'equal))) (push e23 result))
		(if (and e24 (< (position 'A e24 :test #'equal) (position 'D e24 :test #'equal))) (push e24 result))
		result))

(defun recherche (etat but &optional chemin)
	(if (not chemin) (setf chemin ()))
		(push etat chemin) ;on ajoute le noeud actuel au chemin parcouru
		(if (equal etat but) 
			(print (reverse chemin))
			;(return-from recherche (reverse chemin)))
			(dolist (x (successeurs etat))
				(if (not (member x chemin :test #'equal))
					(recherche x but chemin)))))


			
	
;	(let (result))