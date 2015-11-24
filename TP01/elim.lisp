
(setq test '(a z g a s z d g z s))

; recursive
(defun elimr (l)
	(if l
		(if (member (car l) (cdr l))
			(elimr (cdr l))
			(cons (car l) (elimr (cdr l))))))

; iterative
(defun elimi (l)
	(setq result)
	(loop with result = '()
		do (push (something) result) 
		when (something) 
			do (setf result (delete (something) result))
		finally (return result)))

(elimr test)
;(elimi test)
