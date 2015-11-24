; Frames
; frame slot facet démon
; frame = structure de données représentant une situation caractéristique ou prototypique
; 		  composée de plusieurs élts appelés slots ou élts terminaux (sur différents niveaux)
; frame = triplet (frame slot facette)
; chaque slot peut être un frame
; chaque slot peut prendre une valeur par défaut qui est remplacée afin de mieux cadrer avec la réalité
; et servir de variable

(setq Elephant '(elephant 
	(type (value concept))
	(is-a (value etre))
	(color (default "grey"))
	(age (if-needed ask-user)
		(if-added check-age))
	(poids (if-needed computer-weight-from-age))
	(affichage (if-added draw-elephant)
		(if-removed erase-elephant))))
(pushnew 'elephant *frames)

; enconcé changé !
(make-individu 'Clyde 'Elephant '(COLOR "grey" AGE 5))
; (CLYDE
; 	(TYPE (VALUE INDIVIDUAL))
;	(IS-A (VALUE ELEPHANT))
;	(AGE (VALUE 5))
;	(COLOR (VALUE "grey")))

; vérifier que le concept existe déjà
(defun make-individu (name concept properties)
	(if (not (concept-exists? concept)) (return-from make-individu nil))
	(if (not (slot-exist? properties)) (return-from make-individu nil))
	(let ((id (gentemp "I")) temp)
		(loop while properties
			(push (list (pop properties) (list `value (pop properties))) temp))
		(set id (append 
			(list name 
				(list 'type 
					(list 'value 'individual))
				(list 'is-a (list 'value concept)))))
		temp)
	(pushnew id *frames*))

; /!\ manque le check-age quand on traite l'age

; 2. si le slot existe dans le frame individu on retourne
; sinon on regarde s'il existe par défaut dans le concept
; sinon si il y a un démon if-needed on le déclenche
; sinon on recommence pour le concept parent (is-a)
(defun get-slot-value (id slot) 
	(if (slot-exists? id slot)
		(cadr (get-prop-val id slot))
		(if )))


