;   The circle "constructor"
;   ------------------------
;   Following the let over lambda (labels) structure we have discussed in
;   lectures and the lab session, complete the circle definition, providing
;   support for the following:
;
;       - a default circle has x,y values of 0,0 and radius of 1, e.g. if defined using
;           (defvar c (circle))
;
;       - custom values can be set for a circle's x,y,radius (in that order) at creation, e.g.
;           (defvar c (circle 1 2 3))
;       error handling: all values must be numeric, and radius must be greater than 0,
;           the default values are used in place of any invalid values passed
;
;       - getx, gety, getr commands are supported to look up the current value
;           of x, y, and radius for a circle, e.g.
;           (setf result (funcall c 'getx))
;
;       - setx, sety, setr commands are supported to change the current value
;           of x, y, and radius for a circle, e.g.
;           (funcall c 'setx 10)
;       error handling: does not change the value of x/y/r if passed an invalid value
;
;       - dispatcher: generates an error message if an invalid command is passed, e.g.
;           (funcall c 'foo 10)
(defun circle (&optional (xinit 0) (yinit 0) (rinit 1))
    (let ((x (if (numberp xinit) xinit 0))
            (y (if (numberp yinit) yinit 0))
            (r (if (and (numberp rinit) (> rinit 0)) rinit 1)))

        (labels ((getx () x)
                (gety () y)
                (getr () r)
                (setx (xin) (if (numberp xin) (setf x xin)))
                (sety (yin) (if (numberp yin) (setf y yin)))                    
                (setr (rin) (if (and (numberp rin) (> rin 0)) (setf r rin))))

            (lambda (command &optional (extraVal))
                (cond
                    ((equalp command 'getx) (getx))
                    ((equalp command 'gety) (gety))    
                    ((equalp command 'getr) (getr))    
                    ((equalp command 'setx) (setx extraVal))
                    ((equalp command 'sety) (sety extraVal))
                    ((equalp command 'setr) (setr extraVal))
                    (t (format t "Command Invalid~%"))
                )
            )
        )
    )
)


; The overlap function
; --------------------
;   This function expects two circles as parameters, and returns true if they
;       touch or overlap, false otherwise.  
;   The algorithm to be used is as follows:
;       if (x1 + r1 + r2) < x2 return false
;       if (x2 + r1 + r2) < x1 return false
;       if (y1 + r1 + r2) < y2 return false
;       if (y2 + r1 + r2) < y1 return false
;       
;   compute the distance between the two centres:
;       dist = sqrt((x2-x1)^2 + (y2-y1)^2)
;       if dist > (r1 + r2) return false
;       else return true
;
;   The function returns false if passed invalid parameters.
(defun overlap (circ1 circ2)
    (cond 
        ((not (functionp circ1)) nil)
        ((not (functionp circ2)) nil)
        (t  (let ((x1 (funcall circ1 'getx))
                    (y1 (funcall circ1 'gety))
                    (r1 (funcall circ1 'getr))
                    (x2 (funcall circ2 'getx))
                    (y2 (funcall circ2 'gety))
                    (r2 (funcall circ2 'getr)))
                (if (or 
                        (< (+ x1 r1 r2) x2)
                        (< (+ x2 r1 r2) x1)
                        (< (+ y1 r1 r2) y2)
                        (< (+ y2 r1 r2) y1)
                        (< (+ r1 r2) (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))))
                    )
                    nil
                    t
                )
            )
        )
    )
)

