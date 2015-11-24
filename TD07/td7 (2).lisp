(defvar *nodeList* ())

(defun get-prop-val (a-list clef)
	(cdr (assoc clef (symbol-value a-list))))
	
(defun set-prop-val (a-list clef val)
	(let ((pair (assoc clef (symbol-value a-list))))
		(set a-list(cons (cons clef val) (remove pair (symbol-value id) :test#'equal)))))

;; ajoute la valeur val à prop ans id		
(defun add-prop-val (a-list clef val)
	(let ((pair (assoc clef (symbol-value a-list))))
		(set-prop-val a-list clef (cons val (cdr pair)))))
		
(defun mark-node (M node)
	(add-prop-value node `M))
	
(defun marked? (node mark)
	(if member mark (get-prop-val node `mark)
		T
		nil
	))

(defun get-marked-node (mark)
	(let (marked-node)
		(dolist (noeud *nodeList* marked-node)
			(if (marked? noeud mark)
				(push noeud marked-node)))))
				
(defun defnode (name type)
	(let ((id (gentemp "N")))
		(format t "Noeud n° : ~a" id)
		(push id *nodeList*)
		(set id (list (list 'Nom name)
		(list 'Type type)
		(list 'arcIn)
		(list 'arcOut)))))
		
(defun defarc (type nodeIn nodeOut)
	(let ((arc (gentemp "A")))
		(format t "Arc n° : ~a" arc)
		(push arc (cdr (caddr nodeIn)))
		(push arc (cdr (cadddr nodeOut)))
		(set arc (list (list 'type type)
			(list 'from nodeIn)
			(list 'to nodeOut)))))
			
(defun wave (mark type direction)
	(let ((marked-node (get-marked-node(mark))) (new-marked-nodes) (stop 0))
		(loop while (equal stop 0) do
			(dolist (node marked-node nil)
				(if (equal direction "avant")
					(setq nextNodes (predecesseurs node)) ;peut être inverser les deux
					(setq nextNodes (successeurs node))
				)
				(if nextNode
					(dolist (nextNode nextNodes nil)
						(if (not(marked? nextNode mark))
							(push nextNode new-marked-nodes)
						)
					)
				)
			)
			(if (not(new-marked-nodes))
				(setq stop 1)
			)
			(setq marked-nodes new-marked-nodes)
		)
	)
)

;((Type	t)	(from	n2 n3 ...)	(to	n1 n4	…))
;((Nom	n)	(Type	t)	(arcin	a1	a2 …) (arcout	a3	a4 …))
(defun predecesseurs (node)
	(let (predecessors)
		;récupérer les arcs in
		;pour chaque arc in
		(dolist (arcIn (caddr node) predecessors)
			 ;récupérer les from
			(append predecessors (cdr(cadr arcIn))))))

(defun successeurs (node)
	(let ((successors) (arc-outs (cdr (caddr node))) (nodes))
		;récupérer les arcs out
		;pour chaque arc out
		(dolist (arc-out arc-outs nodes)
			;récupérer les to
			(setq nodes (car(caddr (symbol-value arcOut))))
			(append successors nodes))))
	

			
(defnode 'Robert 'individu)
(defnode 'Arthur 'individu)
(defnode 'Gerard 'individu)
(defarc 'is_a n1 n0)
(defarc 'is_a n2 n0)
(defarc 'is_a n0 n1)
(defarc 'is_a n0 n2)
(successeurs n1)
		
		
				
		
			
