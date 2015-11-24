(setq ll '(A 1 BB 2 CCC 3 DDD 4))

; 1
(defun f1 (l) (mapcar #'print l))
(f1 ll)

; 2
(defun f2 (l) (when l (print (car l)) (f2 (cdr l))))
(f2 ll)

; 3
(defun f3 (l) (loop for i in l do (print i)))
(f3 ll)

; 4
(defun f4 (l) (dolist (x l NIL) (print x)))
(f4 ll)

(defun f5 (l) 
	(cond 
		((null l) nil)
		 (t (print (car l)) (f5 (cdr l)))))
(f5 ll)

(defun f6 (l) (dotimes (x (length l) nil) (print (nth x l))))
(f6 ll)
