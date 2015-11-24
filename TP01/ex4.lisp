(setq base '(
	("Le dernier jour d'un condamne" "Hugo" 1829 50000)
	("Les Miserables" "Hugo" 1862 2000000)
	("Le Horla" "Maupassant" 1887 2000000)
	("Contes de la becasse" "Maupassant" 1883 500000)
	("Germinal" "Zola" 1885 3000000)))

(defun titre (o) (car o))
(titre '("Les Miserables" "Hugo" 1862 2000000))

(defun auteur (o) (cadr o))
(auteur '("Les Miserables" "Hugo" 1862 2000000))

(defun annee (o) (caddr o))
(annee '("Les Miserables" "Hugo" 1862 2000000))

(defun nombre (o) (cadddr o))
(nombre '("Les Miserables" "Hugo" 1862 2000000))

(defun FB1 (liste-ouvrages)
	(loop for x in liste-ouvrages
		when (not (eq (x nil)))
		do (print x)))
(fb1 base)

(defun fb2 (b)
	(if b
		(if (equal "Hugo" (auteur (car b)))
			(cons (car b) (fb2 (cdr b)))
			(fb2 (cdr b)))))
(defun FB2 (liste-ouvrages)
	(loop for x in liste-ouvrages do
		(if (equal (auteur x) "Hugo")
			(print x))))

(fb2 base)

(defun fb3 (b a)
	(if b
		(if (equal a (auteur (car b)))
			(cons (car b) (fb3 (cdr b) a))
			(fb3 (cdr b) a))))
(fb3 base "Maupassant")


(defun fb4 (b a)
	(if b
		(if (equal a (annee (car b)))
			(car b)
			(fb4 (cdr b) a))))
(fb4 base 1862)

(defun fb5 (b)
	(if b
		(if (> (nombre (car b)) 1000000)
			(cons (car b) (fb5 (cdr b)))
			(fb5 (cdr b)))))
(fb5 base)

(defun FB6 (b a) ; b = base, a = auteur
	(setq nbOuvrages 0)
	(setq nbOuvragesVendus 0)
	(loop for i in b do
		(if (equal (auteur i) a)
			(progn
				(setq nbOuvrages (+ nbOuvrages 1))
				(setq nbOuvragesVendus (+ nbOuvragesVendus (nombre i))))))
	(if (= nbOuvrages 0)
		0
		(/ nbOuvragesVendus nbOuvrages)))

(fb6 base "Hugo")