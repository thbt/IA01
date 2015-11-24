(defvar nodeList nil)

(defun defnode (name type)
	(let ((id (gentemp "N"))) ; on crée une variable id
		(push id nodeList) ; on la place dans une variable globale
		(set id (list (list 'Nom name) ; on la modifie directement en lui rajoutant le nom et le type passé
			(list 'Type type) 
			(list 'arcIn)
			(list 'arcOut)))))

(defun defarc (type nodeOut nodeIn)
	(let ((arc (gentemp "A")))
		(push arc (cdr (caddr nodeIn)))
		(push arc (cdr (cadddr nodeOut)))
		(set arc (list (list 'type type)
			(list 'from nodeOut)
			(list 'to nodeIn)))))

(defnode 'cyrano 'individu)		; n1
(defnode 'deguiche 'individu)	; n2
(defnode 'cadet 'concept)		; n3
(defnode 'comte 'concept)		; n4
(defnode 'noble 'concept)		; n5

(defarc 'is-a n1 n3)
(defarc 'is-a n2 n4)
(defarc 'is-a n3 n5)
(defarc 'is-a n4 n5)
