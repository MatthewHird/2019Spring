#! /usr/bin/gcl -f

(load "lab2a.cl")
; Testing requirements
; --------------------
; As with past weeks, your lab2b.cl will be a script used to test
;   the function in your lab2a.cl.  It must load lab2a.cl, and
;   then run at least four test cases as described below.

; As always, make sure your functions are named correctly (correct spelling,
;   correct case) and that they RETURN the expected value (having the
;   function simply display the value is not an acceptable substitute).

; The lab2b.cl file must contain at least four test cases,
;   one illustrating each of the four combinations below using appropriate
;   function calls
;     - one where f and g are both functions, each expecting one parameter,
;       and where the return type of g is appropriate as a parameter for f
;     - one where f is a function but g isn't, and f expects two parameters
;       (and g, h are appropriate types for f)
;     - one where g is a function but f isn't, and g expects two parameters
;       (and f, h are appropriate types for g)
;     - one where neither f nor g is a function

(defun caseA ()
    (testCaseResult "caseA" (= 19 (combine (function numPlus5) (function numTimes2) 7)))
)

(defun caseB ()
    (testCaseResult "caseB" (= 15 (combine (function *) 3 5)))
)

(defun caseC ()
    (testCaseResult "caseC" (= 15 (combine 5 (function *) 3)))
)

(defun caseD ()
    (testCaseResult "caseD" (equalp 'NOTFUNCTIONS (combine 1 2 3)))
)

(defun testRunner ()
    (format t "~%")
    (caseA)
    (caseB)
    (caseC)
    (caseD)
)

(defun numPlus5 (num)
    (+ num 5)
)

(defun numTimes2 (num)
    (* num 2)
)

(defun testCaseResult (caseName caseResult)
    (let ((passFail "FAIL"))
        (if caseResult (setf passFail "PASS"))
        (format t "The result of test ~A is: ~A~%~%" caseName passFail)
    )
)

(testRunner)