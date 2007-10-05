;nyquist plug-in
;version 1
;type process
;name "Delay..."
;action "Performing Delay Effect..."
;info "by Roger Dannenberg, modified by David R. Sky\nReleased under terms of the GNU General Public License Version 2"

;control decay "Decay amount [db, negative value increases volume]" int "" 6 -12 24
;control delay "Delay time [seconds]" real "" 0.5 0.0 5.0
;control count "Number of echos" int "" 5 1 30

; Delay by Roger B. Dannenberg
; updated by David R. Sky October 2007
; now includes normalization to avoid clipping,
; added negative decay values, which gives delay effect
; increasing volume with each delay

; Note: this effect will use up memory proportional to
; delay * count, since that many samples must be buffered
; before the first block is freed.

; normalize function:
; checks peak level for maximum 1 million samples 
(defun normalize (signal)
(setf x (if (arrayp signal)
(max (peak (aref signal 0) 1000000) (peak (aref signal 1) 1000000))
(peak signal 1000000)))
(scale (/ 0.95 x) signal))

; delay function
(defun delays (s decay delay count)
  (if (= count 0) (cue s)
	 (sim (cue s)
			(loud decay (at delay (delays s decay delay (- count 1)))))))
(stretch-abs 1 

(normalize (delays s (- 0 decay) delay count)))

; arch-tag: 9dc830cf-962c-4429-a587-b7607b5040fa
