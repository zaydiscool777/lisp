# https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html

(de p1 (a) (cond ((cdr a) (p1 (cdr a))) ('T a)))

(de p2 (a) (cond ((cddr a) (p2 (cdr a))) ('T a)))

(de p3 (a i) (cond ((=1 i) (car a)) ('T (p3 (cdr a) (dec i)))))

(de p4 (a) (cond (a (+ 1 (p4 (cdr a)))) ('T 0)))

(de p5 (a) (h5 a '()))

(de h5 (i o)
	(cond
		((not i) o)
		('T (h5 (cdr i) (cons (car i) o)))
	)
) 

(de p6 (a) (= a (p5 a)))

(de p7 (a)
	(cond
		((atom a) a)
		((atom (car a)) (cons (car a) (p7 (cdr a))))
		('T (append (p7 (car a)) (p7 (cdr a))))
	)
)

(de p8 (a)
	(cond
		((not a) '())
		((== (car a) (cadr a)) (p8 (cdr a)))
		('T (cons (car a) (p8 (cdr a))))
	)
)

(de p9 (a) (h9 a '()))

(de h9 (i s)
	(cond
		((not i) s)
		((== (car i) (cadr i)) (h9 (cdr i) (cons (car i) s)))
		('T (cons (cons (car i) s) (h9 (cdr i) '())))
	)
)

(de p10 (a) (mapcar list (mapcar 'length (p9 a)) (mapcar car (p9 a))))

(de p11 (a) (mapcar '((b) (cond ((== (car b) 1) (cadr b)) ('T b))) (p10 a)))

(de p12 (a)
	(cond
		((not a) '())
		((atom (car a)) (cons (car a) (p12 (cdr a))))
		((=1 (caar a)) (cons (cadar a) (p12 (cdr a))))
		('T (cons (cadar a)
			(p12 (cons (list (dec (caar a)) (cadar a)) (cdr a)))
		))
	)
)
