;nyquist plug-in
;version 1
;type process
;name "Random panning..."
;action "Playing around with your panning knob..."
;info "Random panning by David R. Sky"

;control maxspeed "Max pan sweep speed" real "Hz" 0.50 0.01 20.00
;control factor "Sweep depth factor" int "factor" 80 1 300

;; Random Panning by David R. Sky September 2004

;; Note the lower the frequency (maxspeed), 
;; the higher factor must be to hear a result.
;; Factor can also be used to increase 
;; or decrease the depth of the effect.
;; (Factor and maxspeed are inversely proportional.)

(setf offset 0.5)

; ransiglin - random signal generator (linear)

(defun ransiglin (offset factor maxspeed)

(sum offset (lp (mult factor (lp (noise) maxspeed)) (mult 0.5
maxspeed))))


(defun pan3 (s where)
   (vector (mult (aref s 0) (sum 1 (mult -1 where)))
       (mult (aref s 1) where)))

(pan3 (mult 2 s) (ransiglin offset factor maxspeed)) 

