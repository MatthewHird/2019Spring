#! /usr/bin/gcl -f

(load "lab0a.cl")

; Specifications for main:
;     1. main expects a single list parameter (in this case, containing
;        any command line arguments passed from the user)
;     2. main uses a let block to declare three local variables, X, Y, Z
;     3. if the user provided any command line arguments they are assigned
;        to X, Y, and Z in order.  e.g. if the user ran the script using
;           ./lab0b.cl 10 27
;        then 10 is assigned to X, 27 to Y, and no value is yet set in Z
;     4. if the user provided fewer than three command line arguments,
;        the user is prompted to provide any remaining values needed
;        (e.g. a value for Z in the example from step 3)
;     5. main displays the sizes of X, Y, and Z
;     6. main then calls the cumulative function, passing it the "sizeof"
;        each of the three variables, X, Y, and Z
;     7. finally, main displays the value returned from cumulative, in the form
;           "The cumulative size of X, Y, and Z is: R"
(defun main (arglist)
    (let ((X nil) (xSize nil) (Y nil) (ySize nil) (Z nil) (zSize nil) (argCount (length arglist)))

        (setf X (if (< argCount 1) (getEntry) (read-from-string (car arglist))))
        (setf Y (if (< argCount 2) (getEntry) (read-from-string (cadr arglist))))
        (setf Z (if (< argCount 3) (getEntry) (read-from-string (caddr arglist))))

        (setf xSize (printSize X))
        (setf ySize (printSize Y))
        (setf zSize (printSize Z))

        (format t "The cumulative size of ~A, ~A, and ~A is: ~A~%" X Y Z (cumulative xSize ySize zSize))
    )
)


; prompt user to enter number or list, then returns entered value
(defun getEntry ()
    (format t "Please enter a number or list:~%>>>  ")
    (read)
)


; print size of <inVal> to STDOUT, then returns size of <inVal>
(defun printSize (inVal)
    (let ((valSize nil))
        (setf valSize (sizeof inVal))
        (format t "The size of ~A is ~A~%" inVal valSize)
        valSize
    )
)

(main (cdr si::*command-args*))
