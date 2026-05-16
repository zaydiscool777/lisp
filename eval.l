(de subst (w r x) # good use of primitive functions
	(cond
		((atom x)
			(cond ((== x r) w) ('T x))
		) ('T
			(cons (subst w r (car x)) (subst w r (cdr x)))
		)
	)
)

(de null (x) (== x '())) # technically this should test for atom first

(de and. (x y)
	(cond
		(x
			(cond (y 'T) ('T '()))
		) ('T '())
	)
)

(de not. (x) (cond (x '()) ('T 'T)))

(de append. (x y)
	(cond
		((null x) y) ('T
			(cons (car x) (append. (cdr x) y))
		)
	)
)

(de pair. (x y)
	(cond
		((and. (null x) (null y)) '()) ('T
			(cons
				(list (car x) (car y))
				(pair. (cdr x) (cdr y))
			)
		)
	)
)

(de assoc. (x a)
	(cond
		((null a) '()) # safety clause
		# a = ((key val) ...)
		((== x (caar a)) (cadar a)) ('T
			(assoc. x (cdr a))
		)
	)
)
# if ((null a) x) were used instead, it could allow more first-class functionality in eval.

(de eval. (e a) # e = expression, a = context: ((f def) (g def)...)
	(cond
		((atom e) # primitives are evaled in lists
			(assoc. e a) # replace w/ def
		) ((atom (car e)) # (func arg arg...) = (car cadr caddr...)
			(cond
				((== (car e) 'quote)
					(cadr e) # unevaluated
				) ((== (car e) 'atom) # up until cons is understandable
					(atom (eval. (cadr e) a))
				) ((== (car e) 'eq)
					(== (eval. (cadr e) a) (eval. (caddr e) a))
				) ((== (car e) 'car)
					(car (eval. (cadr e) a))
				) ((== (car e) 'cdr)
					(cdr (eval. (cadr e) a))
				) ((== (car e) 'cons)
					(cons (eval. (cadr e) a) (eval. (caddr e) a))
				) ((== (car e) 'cond)
					(evcon (cdr e) a) # helper function
				) ('T
					(eval. # replace w/ def first, then feed args 
						(cons
							(assoc. (car e) a)
							(cdr e)
						)
					a)
				)
			)
		) ((== (caar e) 'label) # ((label f def) ...)
			(eval.
				(cons (caddar e) (cdr e)) # feed into def, but...
				# remember: label allows the def to contain f itself 
				(cons (list (cadar e) (car e)) a)
			)
		) ((== (caar e) 'lambda) # ((lambda (a b) def) ...)
			(eval.
				(caddar e) # feed into def, but...
				(append.
					(pair.
						(cadar e) # arguments in def
						(evlis (cdr e) a) # become inputs
					)
				a)
			)
		)
	)
)
# a is used instead of simple substitution, because some symbols are bound to expressions, and some are unbound.

(de evcon (c a) # helper function for cond: c = ((a b) (c d)...)
	(cond
		((eval. (caar c) a)
			(eval. (cadar c) a)
		) ('T
			(evcon (cdr c) a)
		)
	)
)

(de evlis (m a) # maps eval a list of inputs
	(cond
		((null m) '()) ('T
			(cons
				(eval. (car m) a)
				(evlis (cdr m) a)
			)
		)
	)
)

(eval. '(
	(label subst (lambda (x y z)
		(cond ((atom z)
			(cond
				((eq z y) x)
				((quote t) z)))
				((quote t)
					(cons
						(subst x y (car z))
						(subst x y (cdr z))
					)
				)
			)
		)
	) (quote m) (quote b) (quote (a b (a b c) d))
))
# -> (a m (a m c) d)

############### extra things from the original paper

(de abs. (x) (cond ((le0 x) (- x)) ('T x)))

(de sign (x) (cond ((le0 x) -1) ((=0 x) 0) ('T 1)))

(de fact (x) (cond ((=0 x) 1) ('T (* x (fact (dec x))))))

(de gcd (m n)
	(cond
		((> m n) (gcd n m))
		((=0 (% n m)) m)
		('T (gcd (% n m) m))
	)
)

# ...

(de ff (x) (cond ((atom x) x) ('T (car x))))

(de equal. (x y)
	(cond
		((and (atom x) (atom y)) (== x y))
		((and (not (atom x)) (not (atom y)))
			(and
				(equal. (car x) (car y))
				(equal. (cdr x) (cdr y))
			)
		)
		('T '())
	)
)

(de among (x y)
	(cond
		((null y) '())
		('T (or (== x (car y)) (among x (cdr y))))
	)
)

(de sub1 (x z) # x is like a from eval: ((r w) ...)
	(cond
		((null x) z)
		((== (caar x) z) (cadar x))
		('T (sub1 (cdr x) z))
	)
)

(de sublis (x y)
	(cond
		((atom y) (sub1 x y))
		('T (cons (sublis x (car y)) (sublis x (cdr y))))
	)
)

(de apply. (f a) (eval (cons f (appq a)) '()))

(de appq (m)
	(cond
		((null m) '())
		('T (cons (list 'quote (car m)) (appq (cdr m))))
	)
)

(de maplist. (x f)
	(cond
		((null x) '())
		('T (cons (f x) (maplist. (cdr x) f)))
	) # note how car wasn't used
)

(de diff. (Y X) # i don't know why, but if they are lowercase it breaks
	(cond
		((atom Y)
			(cond
				((== Y X) 1)
				('T 0)
			)
		) ((== (car Y) '+)
			(cons '+
				(maplist. (cdr Y) '((z) (diff. (car z) X)))
			)
		) ((== (car Y) '*)
			(cons '+
				(maplist. (cdr Y) '((z)
					(maplist. (cdr Y) '((w)
						(cond
							((equal. (car w) (car z)) (diff. (car w) X))
							('T (car w))
						)
					))
				))
			)
		)
	)
) # (diff. '(* x (+ x a) y) 'x) -> (+ (* 1 (+ x a) y) (* x (+ 1 0) y) (* x (+ x a) 0))

(de search. (x p f u) (cond ((null x) u) ((p x) (f x))))
