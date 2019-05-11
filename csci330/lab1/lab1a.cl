;   evens takes a single parameter, L. 
;       If L is not a list, return nil.
;       If L is an empty list, return nil. 
;       Otherwise, return a list containing the elements in the even-numbered positions of L.
;   The evens function must not produce any output.
(defun evens (L)
    (let ((M nil) (i 0))
        (cond
            ((not (listp L)) nil)
            ((null L) nil)
            (t 
                (progn
                    (loop while (< i (length L)) do 
                        (progn
                            (setf M (append M (list (nth i L))))
                            (setf i (+ i 2))
                        )
                    )
                    M
                )
            )   
        )
    )
)


;   revodds is a tail-recursive function that takes 
;   Parameters(2):
;       L is required 
;       sofar is optional with a default value of nil.  
;   Conditions
;       If L is not a list, return sofar.
;       If L is an empty list, return sofar.  
;       Otherwise, return a list containing the elements in the odd-numbered positions of L,
;       but in reverse order.  
;   The revodds function must not produce any output.
(defun revodds (L &optional (sofar nil))
    (cond
        ((not (listp L)) sofar)
        ((null L) sofar)
        ((= (length L) 1) sofar)
        (t (revodds (cddr L) (cons (cadr L) sofar)))
    )
)
