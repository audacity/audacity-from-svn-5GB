;nyquist plug-in
;version 1
;type process
;name "Reverse bouncing ball delay..."
;action "Applying reverse bouncing ball Delay..."
;info "Reverse bouncing ball delay by David Sky"

;control decay "Decay amount" real "dB" 0.50 0.00 5.00
;control delay "Delay time" real "seconds" 0.05 0.01 1.00
;control count "Number of bounces" int "times" 15 1 100

;; Based on delays by Roger B. Dannenberg.

;; Note: this effect will use up memory proportional to
;; delay * count, since that many samples must be buffered
;; before the first block is freed.

;; The first delay will be 
;; delay time * count, 
;; speeding up to delay time... like a bouncing ball.

(truncate count)
(setf revcount (sum count 1))

(defun revbounces (s decay delay count)
  (if (= count 0) 
(cue s)
      (sim (cue s)
               (loud decay (at (mult delay (- revcount count))
(revbounces s decay delay 
(- count 1 ))))))) 
(stretch-abs 1 (revbounces s (- 0 decay) delay count))
