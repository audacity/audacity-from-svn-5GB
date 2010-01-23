;nyquist plug-in
;version 1
;type process
;name "ramp panning..."
;action "Performing ramp panning..."
;info "Ramp panning by David Sky"

;control start "Start position" real "where" 0.0 0.0 1.0 
;control end "End position" real "where" 1.0 0.0 1.0 

(defun pan3 (s where)
   (vector (mult (aref s 0) (sum 1 (mult -1 where)))
       (mult (aref s 1) where)))

(setf width (sum (max start end) (mult -1 (min start end)))) 

(if (> end start)

(pan3 (mult 2 s) 
(sum start (mult width (ramp))))

(pan3 (mult 2 s)
(sum end (mult width (sum 0.5 (mult -1 (sum -0.5 (ramp))))))))  
