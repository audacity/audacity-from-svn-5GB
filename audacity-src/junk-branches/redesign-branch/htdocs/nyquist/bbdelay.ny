;nyquist plug-in
;version 1
;type process
;name "Bouncing ball delay with panning..."
;action "Performing bouncing ball Delay with panning..."
;info "Bouncing ball delay with panning by David Sky"

;control decay "Decay amount" real "dB" 1.00 0.00 24.00
;control delay "Delay time" real "seconds" 0.05 0.01 1.00
;control count "Number of bounces" int "times" 15 1 100
;control move "Pan spread movement" real "move" 0.020 0.001 0.100 

;; Note: this effect will use up memory proportional to
;; delay * count, since that many samples must be buffered
;; before the first block is freed.

;; The first delay will be 
;; delay time * count, 
;; speeding up to delay time... like a bouncing ball.

;; Move defines how increasingly far from center each 
;; bounce will be.

; upcount is used to determine how many bounces have 
; occured, whereas count goes from count to zero
(setf upcount (sum count 1))

(defun pan3 (s where)
   (vector (mult (aref s 0) (sum 1 (mult -1 where)))
       (mult (aref s 1) where)))

(defun panbounces (s decay delay count)
  (if (= count 0) 
(cue s)
      (sim (cue s)
               (loud decay (at (mult delay count) (panbounces (pan3
(mult 2 s) (sum 0.5 (mult move (- upcount count) (expt -1 count))))
decay delay 
(- count 1))))))) 
(stretch-abs 1 (panbounces s (- 0 decay) delay count))

