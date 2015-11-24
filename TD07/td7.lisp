(defvar *nodes* ())
(defvar *arcs* ())

(defun update-index (index val)
	(cond ((not (boundp index)) (set index (list val)))
		((not (listp symbol-value index)) (error "~S n'est pas une liste" index))
		(t (pushnew val (symbol-value index)))))

;; ajoute la valeur val à prop ans id
(defun add-prop-val (id prop val) 
	(let ((pair (assoc prop (symbol-value id))))
		(set-prop-val id prop (cons val (cdr pair)))))

;; retourne la valeur de prop dans id
(defun get-prop-val (id prop) (cdr (assoc prop (symbol-value id))))

;; change la valeur de prop dans id à val
(defun set-prop-val (id prop val) 
	(let ((pair (assoc prop (symbol-value id))))
		(set id (cons (cons prop val) (remove pair (symbol-value id) :test #'equal)))))

(defun defnode (type name)
	(let ((id (gentemp "N")))
 		(set id `((type ,type) (name ,name)))
 		(pushnew id *nodes*)
 		(update-index name id)
 		id))
		
(defun defarc (type from to)
	(let ((id (gentemp "A")))
		(set id `((type ,type) (from ,from) (to ,to)))
		(add-prop-val from 'arc-out id)
		(add-prop-val to 'arc-in id)
		(pushnew id *arcs*)
		id))

(defun mark-node (node mark)
	(add-prop-val node 'mark mark))
	
(defun marked? (node mark)
	(member mark (get-prop-val node 'mark)))

(defun get-marked-nodes (mark)
	(let (marked-nodes)
		(dolist (node *nodes* marked-nodes)
			(if (marked? node mark)
				(push node marked-nodes)))))


(defun get-next-nodes (node type-arc direction)
	(let (result arcs)
		(if (equal direction "direct")
			(setf arcs (get-prop-val node 'arc-out))
			(setf arcs (get-prop-val node 'arc-in)))
		(dolist (arc arcs result)
			(if (equal (cadr (assoc 'type (symbol-value arc))) type-arc)
				(if (equal direction "direct")
					(pushnew (car (get-prop-val arc 'to)) result)
					(pushnew (car (get-prop-val arc 'from)) result))))))

; boucle
;	new-marked-nodes <- nil
;	for each node de marked nodes
;		next nodes <- predecesseurs(node) ou successeurs(node) suivant la direction
;		when next nodes ≠ nil
;			new marked nodes <- new marked nodes + noeuds de next nodes non marqués
; 	si new marked nodes = nil
;		on sort de la boucle
; 	marked nodes <- new marked nodes
; fin boucle
(defun wave (mark type-arc direction)
	(let ((marked-nodes (get-marked-nodes mark)) next-nodes new-marked-nodes (continue T))
		(loop while (equal continue T) do
			(setq new-marked-nodes ())
			(dolist (node marked-nodes) ; pour chaque noeud marqué
				(setq next-nodes (get-next-nodes node type-arc direction)) ; on récupère les suivants a traiter (successeurs ou predecesseurs)
				(if next-nodes ; si cette liste n'est pas vide
					(dolist (next next-nodes) ; on la parcourt
						(if (not (marked? next mark)) ; pour chaque noeud non marqué
							(progn
								(mark-node next mark) ; on le marque
								(pushnew next new-marked-nodes)))))) ; et on l'ajoute a la liste des nouveaux noeuds marqués
			(if new-marked-nodes
				(setq marked-nodes new-marked-nodes)
				(setq continue nil)))
		(return-from wave marked-nodes)))

; algo recherche nobles aimés de roxanne :
; on marque noble (N7) avec M1
; on propage M1 le long des arcs is-a en sens inverse
; on marque roxanne avec m2
; on sélectionne les arcs aime dont l'origine est marquée par M2 et l'extrémité par M1
; resultat : noeuds extrémidé de ces arcs
(defun get-results (mark1 type-arc mark2)
	(let ((wave1 (wave mark1 'is-a "inverse")) result)
		(dolist (origine wave1 result) ; pour chaque sommet marquée par mark1
			(dolist (cible (get-next-nodes origine type-arc "direct") result) ; on étudie les successeurs
				(if (marked? cible mark2) ; et si cette cible est marquée par la marque cherchée, on l'ajoute au résultat
					(pushnew 
						(list (car (get-prop-val origine 'NAME)) 
							type-arc (car (get-prop-val cible 'NAME)))
						result))))))

;;; tests
(defnode 'personne 'Roxane) ; N1
(defnode 'personne 'Cyrano) ; N2
(defnode 'personne 'Christian) ; N3
(defnode 'personne 'deGuiche) ; N4
(defnode 'titre 'cadet_de_gascogne) ; N5
(defnode 'titre 'compte) ; N6
(defnode 'titre 'noble) ; N7
(defnode 'titre 'mondaine) ; N8
(defarc 'aime 'N2 'N1) ; A9
(defarc 'aime 'N3 'N1) ; A10
(defarc 'aime 'N4 'N1) ; ...
(defarc 'aime 'N1 'N3)
(defarc 'is-a 'N1 'N8)
(defarc 'is-a 'N2 'N5)
(defarc 'is-a 'N3 'N5)
(defarc 'is-a 'N4 'N6)
(defarc 'is-a 'N5 'N7)
(defarc 'is-a 'N6 'N7)
(mark-node 'N7 'M1)
(mark-node 'N1 'M2)
(get-results 'M1 'aime 'M2) 
; -> ((DEGUICHE AIME ROXANE) (CYRANO AIME ROXANE) (CHRISTIAN AIME ROXANE)) 
; c'est bien le résultat escompté !!
(get-marked-nodes 'M1) ; (N2 N3 N4 N5 N6 N7)