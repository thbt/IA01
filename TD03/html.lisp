(setq file 
	'(html 
		(header 
			(title "Ma Page"))
		(body
			(h1 "Un titre")
			(p "Soror et aemula Romae"))))

;; Pour la question 2.3, j'ai rajouté stream, afin que make-html écrive directement dans le fichier
;; Bien évidemment, pour la question 2.2 on en a pas besoin 
(defun make-html (html stream)
	(if (listp html)
		(progn ; bloc
			(format stream "<~s>" (car html)) ; on écrit le premier élément de la liste dans le flux
			(dolist (x (cdr html)) ; pour tous les éléments suivants
				(make-html x stream)) ; on recommence
			(format stream "</~s>" (car html))) ; on écrit la balise fermante
		(format stream "~a" html))) ; on écrit le contenu car ce n'est pas le texte d'une balise

(defun save-html (html)
	(with-open-file (fs "/tmp/file.html" ; on utilise la macro expliquée pour le td
		:if-does-not-exist :create ; si le fichier n'existe pas on le crée
		:direction :output ; on l'ouvre en mode écriture
		:if-exists :overwrite) ; s'il existe, on écrit par dessus
		(make-html html fs))) ; on passe le flux ouvert à make-html

(save-html file)

;$ cat /tmp/file.html
;<HTML><HEADER><TITLE>Ma Page</TITLE></HEADER><BODY><H1>Un titre</H1><P>Soror et aemula Romae</P></BODY></HTML>
