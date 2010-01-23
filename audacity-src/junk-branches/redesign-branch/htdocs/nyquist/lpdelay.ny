;nyquist plug-in
;version 1
;type process
;name "Delay with low pass filter..."
;action "Performing Delay with low pass filter..."
;info "Delay with low pass filter by David Sky"

;control decay "Decay amount" real "dB" 0.0 0.0 24.0
;control delay "Delay time" real "seconds" 0.5 0.0 5.0
;control count "Number of echos" int "times" 10 1 30
;control f "Start cutoff frequency" real "hz" 1000 100 20000 
;control lower "Cutoff reduction" real "octaves" 0.5 0.1 5.0 

;; This plug-in is result of a desire to create a delay effect 
;; heard in a popular Cher tune in the late 1990s or later.
;; 
;; Using delays by Roger Dannenberg.
;; 
;; Note: this effect will use up memory proportional to
;; delay * count, since that many samples must be buffered
;; before the first block is freed.
;; 
;; With each delay, the cutoff frequency of the lowpass 
;; filter is reduced.

; Make count an integer
(truncate count)

; set upcount (which goes in direction 1 to count), 
; reverse of count
(setf upcount (sum count 1))

; Set octave drop [od]
(setf od (expt 2 lower))

(defun lpl (s f od upcount count)
(lp s (/ f (expt od (- upcount count)))))

(defun lpdelays (s decay delay count)
  (if (= count 0) (cue s)

      (sim (cue s)
               (loud decay (at delay (lpdelays (lpl s f od upcount
count) decay delay (- count 1))))))) 

(stretch-abs 1 (lpdelays s (- 0 decay) delay count))
