;nyquist plug-in
;version 1
;type process
;name "Delay with Stereo Flip..."
;action "Applying delay, flipping stereo..."
;info "Written by David R. Sky\nReleased under terms of the GNU Public License http://www.opensource.org/licenses/gpl-license.php"

;control decay "Decay amount" real "dB" 3.0 0.0 24.0
;control delay "Delay time" real "seconds" 0.5 0.0 5.0
;control count "Number of delays" int "times" 10 1 100

; Delay with Stereo Flip
; a delay effect which flips stereo channels with each delay
; written by David R. Sky December 2, 2004
; Released under terms of the GNU Public License
; http://www.opensource.org/licenses/gpl-license.php
; 
; Inspired by a sound effect in the opening track of
; Mike Oldfield's "Songs From Distant Earth"
; Thanks to Steven Jones for illustrating
; how to check for even/odd numbers

(setf count (truncate count))

; set original and flipped audio samples
(setq s:orig (vector (aref s 0) (aref s 1)))
(setq s:flip (vector (aref s 1) (aref s 0)))
(setq s (s-rest 0))

; producing next delay
(defun nextflip (i decay delay s s:orig s:flip)
(setf pos (* i delay))
(setf vol (* -1 i decay))
(if (evenp i) ; if i is even, do not flip
(sim (cue s) (at-abs pos (loud vol (cue s:orig)))) 
(sim (cue s) (at-abs pos (loud vol (cue s:flip)))))) 

; generating the delays
(simrep (i count)
(nextflip i decay delay s s:orig s:flip)) 
