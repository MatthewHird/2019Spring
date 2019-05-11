#! /usr/bin/gcl -f

(load "lab1a.cl")

; You must also complete the executable script lab1b.cl, which loads lab1a.cl and
;   defines a function named main that prompts the user to enter a value for L,
;   stores it in a local variable, then calls (evens L) and displays L and the
;   returned result, then calls (revodds L) and displays L and the result.

; Your script must call your main function.

(defun main ()
    (let ((L nil))
        (format t "~%Please enter a list:~%>>>  ")
        (setf L (read))
        (format t "~%The list of even indexed elements of ~A is:~%~A~%" L (evens L))
        (format t "~%The list of odd indexed elements of ~A in reverse order is:~%~A~%~%" L (revodds L))
    )
) 

(main)