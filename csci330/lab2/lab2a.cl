; In file lab2a.cl, define a function named combine.  The function
;       must expect three parameters, f, g, and h.
;    If both f and g are functions, then combine computes and
;       returns (f (g h)) - you may use funcall, eval, or apply,
;       whichever seems the most appropriate.
;    If f is a function and g isn't, then combine computes and
;       returns (f g h) - again using funcall, eval, or apply
;       as appropriate.
;    If g is a function and f isn't, then combine computes and
;       returns (g f h) - again using funcall, eval, or apply.
;    If neither f nor g are functions then combine simply
;       returns the value 'NOTFUNCTIONS

(defun combine (f g h)
    (cond
        ((and (functionp f) (functionp g)) (funcall f (funcall g h)))
        ((functionp f) (funcall f g h))
        ((functionp g) (funcall g f h))
        (t 'NOTFUNCTIONS)
    )
)