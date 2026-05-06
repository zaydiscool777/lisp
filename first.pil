(quote x)
'x
'(a b)
'(s)
'(a b (c) d)
'()
(atom 'a)
(atom '())
(atom '(a b))
(atom (atom 'a))
(atom '(atom 'a))
(== 'a 'a) # common lisp: (eq 'a 'a)
(== 'a 'b)
(== '() '())
(== '(a) '(a))
(car '(a b c))
(car '((a) b c))
(cdr '(a b c))
(cond
	((== 'a 'b) 'first)
	((atom 'a) 'second)
)
(cond)
(cond ('T 'else))
('((a b) (cons a b)) 'x 'y) # common lisp: ((lambda (a b) (cons a b)) 'x 'y)
(de f (x) (cond ((atom x) x) ('T (f (car x))))) # common lisp: (label f (lambda...f...))
(f '((b c) a))

# (def 'eq ==)
# (def 'lambda quote)
# use de if using lambda with label (defun), though.
# comment # common lisp: ; comment
# list and cxr shorthands are also supported in pil

