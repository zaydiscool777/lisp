# https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html

(de p1 (a) (cond ((cdr a) (p1 (cdr a))) ('T a)))

(de p2 (a) (cond ((cddr a) (p2 (cdr a))) ('T a)))

(de p3 (a i) (cond ((=1 i) (car a)) ('T (p3 (cdr a) (dec i)))))

(de p4 (a) (cond (a (+ 1 (p4 (cdr a)))) ('T 0)))

(de p5 (a) (h5 a '()))

(de h5 (i s)
	(cond
		((not i) s)
		('T (h5 (cdr i) (cons (car i) s)))
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

(de p10 (a) (mapcar list (mapcar 'p4 (p9 a)) (mapcar car (p9 a))))

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

(de p13 (a) (cond (a (h13 (cdr a) (car a) 1)) ('T '())))

(de h13 (i c n)
	(cond
		((not i) '())
		((== (car i) c) (h13 (cdr i) c (inc n)))
		('T (cond
			((=1 n) (cons c (h13 (cdr i) (car i) 1)))
			('T (cons (list n c) (h13 (cdr i) (car i) 1)))
		))
	)
)

(de p14 (a) (cond ((not a) '()) ('T (cons (car a) (cons (car a) (p14 (cdr a)))))))

(de p15 (a n) (h15 a n n))

(de h15 (i l n)
	(cond
		((not i) '())
		((=0 l) (h15 (cdr i) n n))
		('T (cons (car i) (h15 i (dec l) n)))
	)
)

(de p16 (a n) (h16 a n n))

(de h16 (i l n)
	(cond
		((not i) '())
		((=1 l) (h16 (cdr i) n n))
		('T (cons (car i) (h16 (cdr i) (dec l) n)))
	)
)

(de p17 (a n) (h17 a '() n))

(de h17 (i s n) (cond ((=0 n) (list (reverse s) i)) ('T (h17 (cdr i) (cons (car i) s) (dec n)))))

(de p18 (a m n) (car (p17 (cdr (p17 a (dec m))) n)))

(de p19 (a n)
	(cond
		((ge0 n) (let x (p17 a n) (append (cadr x) (car x))))
		('T (let x (p17 a (+ (p4 a) n)) (append (cadr x) (car x))))
	)
)

(de p20 (a n) (cond ((=1 n) (cdr a)) ('T (cons (car a) (p20 (cdr a) (dec n))))))

(de p21 (e a n) (cond ((=1 n) (cons e (cdr a))) ('T (cons (car a) (p20 e (cdr a) (dec n))))))

(de p22 (m n)
	(cond
		((== m n) (list m))
		((< m n) (cons m (p22 (inc m) n)))
		('T (cons m (p22 (dec m) n)))
	)
)

(de p23 (a n) (cond ((=0 n) '()) ('T (let x (rand 1 (p4 a)) (cons (p3 a x) (p23 (p20 a x) (dec n)))))))

(de p24 (n m) (p23 (p22 1 m) n))

(de p25 (a) (p23 a (p4 a)))
