;   buildFormatter details:
;       Takes a single argument, N. 
;           N should be a positive integer.  
;           If N is not a positive integer, return 'BADWIDTH.
;       Creates and returns a lambda function that:
;           Takes a single parameter, L.  
;               If L is not a list, prints L padded to width N. 
;               If L is a list, prints each element of L padded to width N.

(defun buildFormatter (N)
    (cond 
        ((and (integerp N) (> N 0)) 
            (let((width N)) 
                (lambda (L) 
                    (if (listp L) (format t (format nil "~~{~~~AA~~}" width) L)
                            (format t (format nil "~~~AA" width) L)
                    )
                )
            )
        )
        (t 'BADWIDTH)
    )
)
