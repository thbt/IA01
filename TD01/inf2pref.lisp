(defun inf2pref (L)
	(if (atom L) 
		L
		(list (cadr L) (inf2pref (car L)) (inf2pref (caddr L)))))

(setq liste '(((a + b) - (c + d)) * (e + f)))

(inf2pref liste)
