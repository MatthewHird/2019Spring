#! /usr/bin/gcl -f

; the timer macro runs a command, displays how long (in seconds) it took to complete,
;     and returns the value the command returned
(defmacro timer (cmd)
    (let ((startTime (gensym)) (endTime (gensym)) (result (gensym)))

        `(let ((,startTime nil) (,endTime nil) (,result nil))
            (setf ,startTime (get-universal-time))
            (setf ,result (eval ,cmd))
            (setf ,endTime (get-universal-time))
            (format t "That took ~A seconds~%" (- ,endTime ,startTime))
            ,result
        )
    )
)

(defun printSlow (n)
   (format t "hi...") (sleep 3) (format t "...there~%") 42)

(defvar r (timer (printSlow 3)))
(format t "The command returned ~A~%" r)

; ; Shows the expansion of "timer" macro, showing that the local variables names were
; ;     properly replaced with unique symbols from gensym 
; (format t "~A~%" (MACROEXPAND-1 '(timer myCmd)))

