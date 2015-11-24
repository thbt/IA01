; système expert = Base de Règles (BR) + Moteur d'Inférence (MI) + Bases de Faits (BF) + but

; moteur d'ordre 0 : 
; -> fait = proposition, donc à valeur booléenne
;		SI beau_temps ALORS promenade
; 		   ^prémisse(s)		^conclusion

; Moteur d'ordre 0+ : 
; -> fait = couple (attribut valeur) (AV) SI (lieu_naissance dallas) ALORS (nationalite usa)
; -> fait = triplet (objet attribut valeur) (OAV) SI (lewis lieu_naissance dallas) ALORS (lewis nationalite usa)

; Moteur d'ordre 1 :
; basée sur le calcul des prédicats
; permet de manipuler des variables (locales) en procédant par semi unification (cf IA02)
; Un fait est une expression symbolique sans variable
; -> SI ($pers lieu_naissance $ville)
;	 ET ($ville pays usa)
;	 ALORS ($pers nationalite americaine)

; moteur d'inférences 0
; 1) Règles candidates
; 2) Choix de la règle (heuristique)
; 3) Remplissage de la BF

; (Rx prémisses ccl)
;(setq R1 '((B D F)	F))
;(setq R2 '((D G)	A))
;(setq R3 '((C F)	A))
;(setq R4 '((C)		D))
;(setq R5 '((D)		E))
;(setq R6 '((A)		H))
;(setq R7 '((B)		X))
;(setq R8 '((X C)	A))

(setq rules '(
	(R1 (B D F)	F)
	(R2 (D G)	A)
	(R3 (C F)	A)
	(R4 (C)		D)
	(R5 (D)		E)
	(R6 (A)		H)
	(R7 (B)		X)
	(R8 (X C)	A)))

(setq facts '(B C))

(setq goal 'H)

;; retourne les prémisses de la règle passée en paramètre
(defun premises (rule) (cadr rule))

;; retourne la conclusion de la règle passée en paramètre
(defun conclusion (rule) (caddr rule))

(defun true (facts goal) (member goal facts))

; règles possédant la conclusion passée en paramètre
;(defun premises (rules conclusion)
;	(let ((result ()))
;		(dolist (x rb result)
;			(if (member conclusion (cddr x))
;				(push x result)))))

;(defun CAP (rules facts goal &optional rulesPath)
;	(if (member goal facts)	; si le but est dans les faits...
;		rulesPath ; ...on retourne le chemin actuel
;		;; sinon on 
;		(let ((x (premises rules goal) ) 
;			(dolist (x rb result)
;				(if (member goal (cddr x))
;					(push (car x) result)))))))

(defun candidateRules (rules fact)
	(let (candidates)
		(dolist (r rules (reverse candidates))
			(if (equal (list fact) (conclusion r))
				(push r candidates)))))
	

(defun question (goal)
	(format t "is the fact ~a member of the facts base ? (T or nil)" goal)
	(read))

(defun verifyAnd (rules facts goal rule)
	(let ((OK T) (prem (premises rule)) candidates)
		(dolist (p prem candidateFacts)
			(if (not OK) 
				(return-from verifyAnd OK))
			(setf OK (verify rules facts p)))--
		OK))

(defun verify (rules facts goal) 
	(let (OK)
		(if (member goal facts)
			(setf OK T)
			(let ((candidates (candidateRules rules goal)))
				(dolist (r candidates OK)
					(if (not OK)
						(return-from verify OK))
					(setf OK (verifyAnd rules facts goal r)))
				(if (not OK)
					(setf OK (question goal)))))))


