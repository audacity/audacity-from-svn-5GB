;nyquist plug-in
;version 1
;type process
;name "Time Shift tool..."
;action "Time shifting one track..."
;info "Time Shift Tool by David R. Sky\nChoose the track you want to shift forward in time, by how much"
;; The above line should not be word wrapped!

;; Time Shift Tool by David R. Sky October 28, 2004
;; For people who use screen readers and/or
;; who prefer to use the keyboard over the mouse
;; This is a plug-in option to the Audacity Time Shift Tool

;control track "Track" int "0=left 1=right" 0 0 1
;control shift "Time shift" real "ms" 10.0 0.0 100.0

(setf track (truncate track))
(setf shift (mult 0.001 shift))

(if (= track 0)
(vector 
(sim (s-rest 0.0) (at-abs shift (cue (aref s 0))))
(aref s 1))

(vector 
(aref s 0)
(sim (s-rest 0.0) (at-abs shift (cue (aref s 1))))))
