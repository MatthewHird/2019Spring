#! /usr/bin/gcl -f

(load "lab4a.cl")

; create one circle that is initialized by user-provided data
(format t "Enter the x then y then r values for a circle, e.g. 3 5 10~%")
(defvar c1 (circle (read) (read) (read)))

; create a second, default, circle
(format t "Creating a default circle~%")
(defvar c2 (circle))

; use circle's get methods to lookup the stats on each
(format t "Circle 1 stats: (~A,~A:~A)~%"
     (funcall c1 'getx) (funcall c1 'gety) (funcall c1 'getr))
(format t "Circle 2 stats: (~A,~A:~A)~%"
     (funcall c2 'getx) (funcall c2 'gety) (funcall c2 'getr))

; set new stats for circle 2 based on more user-provided data
(format t "Enter the x then y then r values for a circle, e.g. 3 5 10~%")
(funcall c2 'setx (read))
(funcall c2 'sety (read))
(funcall c2 'setr (read))
(format t "New circle 2 stats: (~A,~A:~A)~%"
     (funcall c2 'getx) (funcall c2 'gety) (funcall c2 'getr))

; see if the circles overlap
(if (overlap c1 c2)
    (format t "The two circles overlap~%")
    (format t "They are non-overlapping circles~%"))


