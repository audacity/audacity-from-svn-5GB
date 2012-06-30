;nyquist plug-in
;version 1
;type process
;name "LFO panning..."
;action "Performing LFO panning..."
;info "LFO panning by David Sky"

;control rate "LFO frequency" real "Hz" 0.20 0.00 20.00 
;control spread "stereo spread" int "percent" 80 0 100 

(defun pan3 (s where)
   (vector (mult (aref s 0) (sum 1 (mult -1 where)))
       (mult (aref s 1) where)))

(pan3 (mult 2 s) (sum (mult 0.5 (sum 1 (mult -1 spread 0.01))) 
(mult 0.01 spread (mult 0.5 (sum 1 (lfo rate))))))
