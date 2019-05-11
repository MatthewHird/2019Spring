
; function (sizeof N) needs to be defined
; Specifications for sizeof:
;    sizeof expects a single parameter, N
;    The behaviour of sizeof depends on the datatype of the parameter passed:
;     - if N is a number:
;          sizeof returns the value of N
;     - if N is a list:
;          sizeof returns the length of N
;     - if N is anything else:
;          sizeof returns 0
;    This function must NOT produce any output.
(defun sizeof (N)
    (cond
        ((numberp N) N)
        ((listp N) (length N))
        (t 0)
    )
)


; function (cumulative A B C) needs to be defined
; Specifications for cumulative:
;     cumulative expects three numeric parameters, A, B, C.
;     If one or more of the parameters are non-numeric then
;        the function returns the value 'ERROR
;     otherwise the function returns the sum of the three parameters.
;     This function must NOT produce any output.
(defun cumulative (A B C)
    (if (and (numberp A) (numberp B) (numberp C)) (+ A B C) 'ERROR)
)
