#! /usr/bin/gcl -f

(load "lab3a.cl")

;   Asks the user for values for N (pad width) and L (list or value to display) 
;       and displays L with a pad width of N.
;
;   buildFormatter details:
;       buildFormatter takes a single argument, N, that should be
;           a positive integer.  If N is not a positive integer then
;           buildFormatter returns 'BADWIDTH.
;
;       buildFormatter creates and returns a lambda function that
;           takes a single parameter, L.  If L is not a list then
;           the lambda function prints L padded to width N. If L is
;           a list then the lambda function prints each element of L
;           padded to width N.

(defun main ()
    (let ((N nil) (L nil))
        (format t "~%Please enter an integer for N (pad width):~%>>>  ")
        (setf N (read))
        (defvar myFun (buildFormatter N))
        (if (equal myFun 'BADWIDTH)
            (format t "Invalid pad width~%")
            (progn 
                (format t "~%Please enter an list or value to display with a pad width of ~D:~%>>>  " N)
                (setf L (read))
                (funcall myFun L)
                (format t "~%")
            )
        )
    )
)


(main)
