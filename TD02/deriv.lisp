;; derivation du terme a par rapport à la variable x
(defun deriv-term (a x)	(if a (if (equal a x) 1 0)))

;(deriv-term 'x 'x)
;(deriv-term 'y 'x)
;(deriv-term 35 'x)

;; derivation de l'exp linéaire p par rapport à la variable x
(defun deriv-sum (e x) 
	(list (car e) (deriv (cadr e) x) (deriv (caddr e) x))))

;(deriv-sum '(+ x 3) 'x)
;(deriv-sum '(+ (+ x 3) (- x 4)) 'x)


(defun deriv-mul (e x) 
	(if (atom e)
		(deriv-term e x)
		(list '+ 
			(list '* (deriv-mult (cadr e) x) (caddr e))
			(list '* (deriv-mult (caddr e) x) (cadr e)))))


(defun deriv (exp)
	(cond 
		((atom exp) (if (equal exp var) 1 0))
		((equal (car exp) '+) (deriv-sum exp var))
		((equal (car exp) '-) (deriv-sub exp var))
		((equal (car exp) '*) (deriv-mul exp var))
		((equal (car exp) '/) (deriv-div exp var))
		(t (error "exp non traitée"))))

(deriv '(* 3 x) 'x)

(defun simplify (exp) 
	(if (atom exp)
		exp
		(if (and (atom (cadr exp)) (atom (caddr exp)))
			(cond 
				((equal (car exp) '+) (cond 
					((equal (cadr exp) '0) (caddr exp))
					((equal (caddr exp) '0) (cadr exp))
					(T (list (cadr exp) (car exp) (caddr exp)))))
				((equal (car exp) '*) (cond
					((OR (equal (cadr exp) '0) (equal (caddr exp) '0)) '0)
					((equal (cadr exp) '1) (caddr exp))
					((equal (caddr exp) '1) (cadr exp))
					(T (list(cadr exp) (car exp) (caddr exp)))))) 
			(list (simpl (cadr exp)) (car exp) (simpl (caddr exp))))))

(simplify '(+ (* 0 X) (* 1 3)))
