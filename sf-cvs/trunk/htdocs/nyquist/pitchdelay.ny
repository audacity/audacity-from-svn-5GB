;nyquist plug-in
;version 1
;type process
;name "Delay with Pitch Shift..."
;action "Applying Delay with Pitch shift..."
;info "Delay with Pitch shift by David R. sky"

;control decay "Decay amount" real "dB" 0 0 24
;control delay "Delay time" real "seconds" 0.5 0.0 5.0
;control count "Number of echos" int "times" 10 1 30
;control shift "Tempo shift" real "shift" 1.10 0.10 10.00
;control md "Multiply/divide" int "0=multiply 1=divide" 0 0 1

(cond 
((= md 0) (setf shift (/ 1.0 shift))
(= md 1) (setf shift shift)))

(defun change (s shift)
(force-srate 44100 (stretch-abs shift (sound s))))

(defun delays (s decay delay count)
  (if (= count 0) (cue s)
      (sim (cue s)                
(loud decay (at delay (delays (change s shift) decay delay (- count
1))))))) 

(stretch-abs 1 (delays s (- 0 decay) delay count))

